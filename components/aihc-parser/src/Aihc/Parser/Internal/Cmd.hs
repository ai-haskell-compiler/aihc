{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Cmd
  ( cmdParser,
  )
where

import Aihc.Parser.Internal.CheckPattern (checkPattern)
import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (exprParser, exprParserNoArrowTail, parseLetDeclsParser, parseLetDeclsStmtParser)
import Aihc.Parser.Internal.Pattern (patternParser, simplePatternParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

-- | Parse a command (the body of a @proc@ abstraction).
--
-- Grammar (simplified):
--
-- @
-- cmd   = exp10 -\< exp | exp10 -\<\< exp | cmd0
-- cmd0  = cmd10 (op cmd10)*
-- cmd10 = do { cstmts } | if … | case … | let … | \\pats -> cmd | (cmd)
-- @
cmdParser :: TokParser Cmd
cmdParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    -- Keyword commands parse as cmd10, then check for infix chain.
    TkKeywordDo -> cmdOperandThenInfix cmdDoParser
    TkKeywordIf -> cmdOperandThenInfix cmdIfParser
    TkKeywordCase -> cmdOperandThenInfix cmdCaseParser
    TkKeywordLet ->
      -- 'let decls in cmd' is a command; 'let decls' (without 'in') inside
      -- a do-block is a statement, not handled here.
      cmdOperandThenInfix cmdLetParser
    TkReservedBackslash -> cmdOperandThenInfix cmdLamParser
    TkSpecialLParen -> cmdOperandThenInfix cmdParenParser
    _ -> do
      -- Not a keyword command: parse the left side as an expression while
      -- leaving -< / -<< available for command parsing.
      expr <- exprParserNoArrowTail
      mArrowTail <- MP.optional cmdArrTailParser
      case mArrowTail of
        Just (appType, rhs) ->
          cmdInfixChain (CmdArrApp expr appType rhs)
        Nothing ->
          fail "expected arrow command (-< or -<<)"

-- | Parse a cmd10 operand, then check for command-level infix.
cmdOperandThenInfix :: TokParser Cmd -> TokParser Cmd
cmdOperandThenInfix p = do
  lhs <- p
  cmdInfixChain lhs

-- | Parse an arrow tail operator in command context, returning the
-- application type and the right-hand expression.
cmdArrTailParser :: TokParser (ArrAppType, Expr)
cmdArrTailParser = do
  appType <- tokenSatisfy "arrow operator" $ \tok ->
    case lexTokenKind tok of
      TkArrowTail -> Just HsFirstOrderApp
      TkDoubleArrowTail -> Just HsHigherOrderApp
      _ -> Nothing
  rhs <- exprParser
  pure (appType, rhs)

-- | Parse the command-level infix chain: @cmd (op cmd)*@.
cmdInfixChain :: Cmd -> TokParser Cmd
cmdInfixChain lhs = do
  rest <-
    MP.many
      ( (,)
          <$> infixOperatorParserExcept []
          <*> cmdParser
      )
  pure (foldl buildCmdInfix lhs rest)
  where
    buildCmdInfix l (op, r) = CmdInfix l op r

-- | Parse a command do-block: @do { cstmt ; ... }@
cmdDoParser :: TokParser Cmd
cmdDoParser = withSpanAnn (CmdAnn . mkAnnotation) $ do
  expectedTok TkKeywordDo
  CmdDo <$> bracedSemiSep1 cmdStmtParser

-- | Parse a command if-then-else: @if exp then cmd else cmd@
cmdIfParser :: TokParser Cmd
cmdIfParser = withSpanAnn (CmdAnn . mkAnnotation) $ do
  expectedTok TkKeywordIf
  cond <- exprParser
  skipSemicolons
  expectedTok TkKeywordThen
  yes <- cmdParser
  skipSemicolons
  expectedTok TkKeywordElse
  CmdIf cond yes <$> cmdParser

-- | Parse a command case: @case exp of { calts }@
cmdCaseParser :: TokParser Cmd
cmdCaseParser = withSpanAnn (CmdAnn . mkAnnotation) $ do
  expectedTok TkKeywordCase
  scrut <- region "while parsing case scrutinee" exprParser
  expectedTok TkKeywordOf
  alts <- bracedSemiSep1 cmdCaseAltParser
  pure (CmdCase scrut alts)

cmdCaseAltParser :: TokParser CmdCaseAlt
cmdCaseAltParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedRightArrow
  body <- cmdParser
  pure (\span' -> CmdCaseAlt [mkAnnotation span'] pat body)

-- | Parse a command let: @let decls in cmd@
cmdLetParser :: TokParser Cmd
cmdLetParser = withSpanAnn (CmdAnn . mkAnnotation) $ do
  decls <- parseLetDeclsParser
  expectedTok TkKeywordIn
  CmdLet decls <$> cmdParser

-- | Parse a command lambda: @\\pats -> cmd@
cmdLamParser :: TokParser Cmd
cmdLamParser = withSpanAnn (CmdAnn . mkAnnotation) $ do
  expectedTok TkReservedBackslash
  pats <- MP.some simplePatternParser
  expectedTok TkReservedRightArrow
  CmdLam pats <$> cmdParser

-- | Parse a parenthesised command: @( cmd )@
cmdParenParser :: TokParser Cmd
cmdParenParser =
  withSpanAnn (CmdAnn . mkAnnotation) $
    CmdPar <$> parens cmdParser

-- | Parse a do-statement in command context (arrow do).
cmdStmtParser :: TokParser (DoStmt Cmd)
cmdStmtParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try cmdLetStmtParser <|> cmdBodyStmtParser
    TkKeywordRec -> cmdRecStmtParser
    -- Keyword commands: parse as command body statements.
    TkKeywordDo -> cmdBodyStmtParser
    TkKeywordIf -> cmdBodyStmtParser
    TkKeywordCase -> cmdBodyStmtParser
    TkReservedBackslash -> cmdBodyStmtParser
    TkSpecialLParen -> MP.try cmdBindOrBodyStmtParser <|> cmdBodyStmtParser
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then cmdBindStmtParser
        else cmdBindOrBodyStmtParser

startsWithPatternBind :: TokParser Bool
startsWithPatternBind =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- patternParser
    expectedTok TkReservedLeftArrow

-- | Parse a command do-statement: @cmd@ or @pat <- cmd@.
-- Uses the expression-first approach: parse as expression, check for @<-@.
cmdBindOrBodyStmtParser :: TokParser (DoStmt Cmd)
cmdBindOrBodyStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  -- Parse the LHS as an expression WITHOUT consuming arrow tails.
  -- Arrow tails (-<, -<<) belong to the command level, not the expression.
  expr <- exprParserNoArrowTail
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> DoBind <$> liftCheck (checkPattern expr) <*> cmdParser
    Nothing -> do
      -- No bind arrow: this is a body statement.  Check for arrow tail.
      mArrTail <- MP.optional cmdArrTailParser
      case mArrTail of
        Just (appType, rhs) ->
          pure (DoExpr (CmdArrApp expr appType rhs))
        Nothing ->
          fail "expected arrow command (-< or -<<) in do statement"

-- | Parse a command bind statement where the pattern is unambiguously a
-- pattern (starts with !, ~, or x@).
cmdBindStmtParser :: TokParser (DoStmt Cmd)
cmdBindStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  cmd <- region "while parsing '<-' binding" cmdParser
  pure (DoBind pat cmd)

-- | Parse a body-only command statement (fallback from cmdStmtParser).
cmdBodyStmtParser :: TokParser (DoStmt Cmd)
cmdBodyStmtParser =
  withSpanAnn (DoAnn . mkAnnotation) $
    DoExpr <$> cmdParser

-- | Parse a command let-statement: @let decls@
cmdLetStmtParser :: TokParser (DoStmt Cmd)
cmdLetStmtParser =
  withSpanAnn (DoAnn . mkAnnotation) $
    DoLetDecls <$> parseLetDeclsStmtParser

-- | Parse a command rec-statement: @rec { cstmts }@
cmdRecStmtParser :: TokParser (DoStmt Cmd)
cmdRecStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  expectedTok TkKeywordRec
  DoRecStmt <$> bracedSemiSep1 cmdStmtParser
