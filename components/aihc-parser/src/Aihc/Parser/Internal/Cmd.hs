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
          let span' = mergeSourceSpans (getSourceSpan expr) (getSourceSpan rhs)
           in cmdInfixChain (CmdArrApp span' expr appType rhs)
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
      ( (,) . renderName
          <$> infixOperatorParserExcept []
          <*> cmdParser
      )
  pure (foldl buildCmdInfix lhs rest)
  where
    buildCmdInfix l (op, r) = CmdInfix (mergeSourceSpans (getSourceSpan l) (getSourceSpan r)) l op r

-- | Parse a command do-block: @do { cstmt ; ... }@
cmdDoParser :: TokParser Cmd
cmdDoParser = withSpan $ do
  expectedTok TkKeywordDo
  stmts <- bracedSemiSep1 cmdStmtParser
  pure (`CmdDo` stmts)

-- | Parse a command if-then-else: @if exp then cmd else cmd@
cmdIfParser :: TokParser Cmd
cmdIfParser = withSpan $ do
  expectedTok TkKeywordIf
  cond <- region "while parsing if condition" exprParser
  skipSemicolons
  expectedTok TkKeywordThen
  yes <- region "while parsing then branch" cmdParser
  skipSemicolons
  expectedTok TkKeywordElse
  no <- region "while parsing else branch" cmdParser
  pure (\span' -> CmdIf span' cond yes no)

-- | Parse a command case: @case exp of { calts }@
cmdCaseParser :: TokParser Cmd
cmdCaseParser = withSpan $ do
  expectedTok TkKeywordCase
  scrut <- region "while parsing case scrutinee" exprParser
  expectedTok TkKeywordOf
  alts <- bracedSemiSep1 cmdCaseAltParser
  pure (\span' -> CmdCase span' scrut alts)

cmdCaseAltParser :: TokParser CmdCaseAlt
cmdCaseAltParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedRightArrow
  body <- cmdParser
  pure (\span' -> CmdCaseAlt span' pat body)

-- | Parse a command let: @let decls in cmd@
cmdLetParser :: TokParser Cmd
cmdLetParser = withSpan $ do
  decls <- parseLetDeclsParser
  expectedTok TkKeywordIn
  body <- cmdParser
  pure (\span' -> CmdLet span' decls body)

-- | Parse a command lambda: @\\pats -> cmd@
cmdLamParser :: TokParser Cmd
cmdLamParser = withSpan $ do
  expectedTok TkReservedBackslash
  pats <- MP.some simplePatternParser
  expectedTok TkReservedRightArrow
  body <- cmdParser
  pure (\span' -> CmdLam span' pats body)

-- | Parse a parenthesised command: @( cmd )@
cmdParenParser :: TokParser Cmd
cmdParenParser = withSpan $ do
  cmd <- parens cmdParser
  pure (`CmdPar` cmd)

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
cmdBindOrBodyStmtParser = withSpan $ do
  -- Parse the LHS as an expression WITHOUT consuming arrow tails.
  -- Arrow tails (-<, -<<) belong to the command level, not the expression.
  expr <- exprParserNoArrowTail
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- region "while parsing '<-' binding" cmdParser
      pure (\span' -> DoBind span' pat rhs)
    Nothing -> do
      -- No bind arrow: this is a body statement.  Check for arrow tail.
      mArrTail <- MP.optional cmdArrTailParser
      case mArrTail of
        Just (appType, rhs) ->
          let cmd = CmdArrApp (mergeSourceSpans (getSourceSpan expr) (getSourceSpan rhs)) expr appType rhs
           in pure (`DoExpr` cmd)
        Nothing ->
          fail "expected arrow command (-< or -<<) in do statement"

-- | Parse a command bind statement where the pattern is unambiguously a
-- pattern (starts with !, ~, or x@).
cmdBindStmtParser :: TokParser (DoStmt Cmd)
cmdBindStmtParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  cmd <- region "while parsing '<-' binding" cmdParser
  pure (\span' -> DoBind span' pat cmd)

-- | Parse a body-only command statement (fallback from cmdStmtParser).
cmdBodyStmtParser :: TokParser (DoStmt Cmd)
cmdBodyStmtParser = withSpan $ do
  cmd <- cmdParser
  pure (`DoExpr` cmd)

-- | Parse a command let-statement: @let decls@
cmdLetStmtParser :: TokParser (DoStmt Cmd)
cmdLetStmtParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (`DoLetDecls` decls)

-- | Parse a command rec-statement: @rec { cstmts }@
cmdRecStmtParser :: TokParser (DoStmt Cmd)
cmdRecStmtParser = withSpan $ do
  expectedTok TkKeywordRec
  stmts <- bracedSemiSep1 cmdStmtParser
  pure (`DoRecStmt` stmts)
