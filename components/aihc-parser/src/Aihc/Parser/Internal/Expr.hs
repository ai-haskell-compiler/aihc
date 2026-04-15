{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aihc.Parser.Internal.Expr
  ( exprParser,
    equationRhsParser,
    -- Re-exports from Pattern
    simplePatternParser,
    appPatternParser,
    patternParser,
    -- Re-exports from Type
    typeParser,
    typeInfixParser,
    typeInfixOperatorParser,
    typeHeadInfixParser,
    typeAppParser,
    typeAtomParser,
    -- Re-exports from Common
    startsWithTypeSig,
    startsWithContextType,
    -- Needed by Cmd.hs via SOURCE
    exprParserNoArrowTail,
    parseLetDeclsParser,
    parseLetDeclsStmtParser,
  )
where

import Aihc.Parser.Internal.CheckPattern (checkPattern)
import Aihc.Parser.Internal.Cmd (cmdParser)
import Aihc.Parser.Internal.Common
import Aihc.Parser.Internal.Decl (declParser, pragmaDeclParser)
import Aihc.Parser.Internal.Pattern (appPatternParser, patternParser, simplePatternParser)
import Aihc.Parser.Internal.Type (typeAppParser, typeAtomParser, typeHeadInfixParser, typeInfixOperatorParser, typeInfixParser, typeParser)
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

-- | Like 'withSpan' but stores the span in an 'EAnn' dynamic annotation; the
-- inner builder is applied at 'NoSourceSpan' so spans are not duplicated.
withExprSpanAnn :: TokParser (SourceSpan -> Expr) -> TokParser Expr
withExprSpanAnn parser =
  withSpan (parser >>= \g -> pure (\sp -> exprAnnSpan sp (g NoSourceSpan)))

withDeclSpanAnn :: TokParser (SourceSpan -> Decl) -> TokParser Decl
withDeclSpanAnn parser =
  withSpan (parser >>= \g -> pure (\sp -> declAnnSpan sp (g NoSourceSpan)))

exprParser :: TokParser Expr
exprParser =
  exprParserWithTypeSigParser typeParser

exprParserWithTypeSigParser :: TokParser Type -> TokParser Expr
exprParserWithTypeSigParser typeSigParser =
  label "expression" $
    exprCoreParserWithTypeSigParserExcept typeSigParser []

exprParserExcept :: [Text] -> TokParser Expr
exprParserExcept =
  exprCoreParserWithTypeSigParserExcept typeParser

exprParserNoTopLevelTypeSig :: TokParser Expr
exprParserNoTopLevelTypeSig =
  label "expression" $
    exprCoreParserWithoutTypeSigExcept []

exprCoreParserWithoutTypeSigExcept :: [Text] -> TokParser Expr
exprCoreParserWithoutTypeSigExcept forbiddenInfix = do
  tok <- lookAhead anySingle
  base <- case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordMdo -> mdoExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordLet -> letExprParser
    TkKeywordProc -> procExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept forbiddenInfix
  rest <- MP.many ((,) <$> infixOperatorParserExcept forbiddenInfix <*> region "after infix operator" lexpParser)
  afterArrow <- MP.optional arrowTailParser
  let withInfix = foldl buildInfix base rest
  pure $ case afterArrow of
    Just (op, rhs) ->
      exprAnnSpan
        (mergeSourceSpans (getSourceSpan withInfix) (getSourceSpan rhs))
        (EInfix withInfix op rhs)
    Nothing -> withInfix

exprCoreParserWithTypeSigParserExcept :: TokParser Type -> [Text] -> TokParser Expr
exprCoreParserWithTypeSigParserExcept typeSigParser forbiddenInfix = do
  withArrow <- exprCoreParserWithoutTypeSigExcept forbiddenInfix
  -- Optional type signature: expr :: type
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeSigParser)
  pure $ case mTypeSig of
    Just ty -> exprAnnSpan (mergeSourceSpans (getSourceSpan withArrow) (getSourceSpan ty)) (ETypeSig withArrow ty)
    Nothing -> withArrow

-- | The operator name used to represent @->@ in view-pattern expressions.
viewPatArrowName :: Name
viewPatArrowName = qualifyName Nothing (mkUnqualifiedName NameVarSym "->")

-- | Optionally consume a @->@ token and parse the right-hand side as a
-- view-pattern expression.  Returns the original expression unchanged when
-- no @->@ follows.
maybeViewPattern :: Expr -> TokParser Expr
maybeViewPattern lhs = do
  mArrow <- MP.optional (expectedTok TkReservedRightArrow)
  case mArrow of
    Just () -> do
      viewRhs <- texprParser
      let sp = mergeSourceSpans (getSourceSpan lhs) (getSourceSpan viewRhs)
      pure (exprAnnSpan sp (EInfix lhs viewPatArrowName viewRhs))
    Nothing -> pure lhs

-- | Like 'exprParser' but also allows the view-pattern arrow @->@ at the
-- top level.  This corresponds to GHC\'s @texp@ production, which is used
-- inside delimited contexts such as parentheses @(…)@ and unboxed parens
-- @(# … #)@.
texprParser :: TokParser Expr
texprParser = exprParser >>= maybeViewPattern

-- | Parse an arrow tail operator (@-<@ or @-<<@) followed by its right-hand expression.
arrowTailParser :: TokParser (Name, Expr)
arrowTailParser = do
  op <- tokenSatisfy "arrow operator" $ \tok ->
    case lexTokenKind tok of
      TkArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<"))
      TkDoubleArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<<"))
      _ -> Nothing
  rhs <- exprParser
  pure (op, rhs)

ifExprParser :: TokParser Expr
ifExprParser = do
  nextTok <- lookAhead (anySingle *> anySingle)
  case lexTokenKind nextTok of
    TkSpecialLBrace -> multiWayIfExprParser
    _ -> classicIfExprParser

classicIfExprParser :: TokParser Expr
classicIfExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordIf
  cond <- region "while parsing if condition" exprParser
  skipSemicolons
  expectedTok TkKeywordThen
  yes <- region "while parsing then branch" exprParserNoTopLevelTypeSig
  skipSemicolons
  expectedTok TkKeywordElse
  no <- region "while parsing else branch" exprParserNoTopLevelTypeSig
  pure (\_ -> EIf cond yes no)

multiWayIfExprParser :: TokParser Expr
multiWayIfExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordIf
  rhss <- braces (MP.some multiWayIfAlternative)
  pure (\_ -> EMultiWayIf rhss)

multiWayIfAlternative :: TokParser GuardedRhs
multiWayIfAlternative = withSpan $ do
  expectedTok TkReservedPipe
  guards <- layoutSepBy1 guardQualifierParser (expectedTok TkSpecialComma)
  expectedTok TkReservedRightArrow
  body <- exprParser
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

doExprParser :: TokParser Expr
doExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordDo
  stmts <- bracedSemiSep1 doStmtParser
  pure (\_ -> EDo stmts False)

mdoExprParser :: TokParser Expr
mdoExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordMdo
  stmts <- bracedSemiSep1 doStmtParser
  pure (\_ -> EDo stmts True)

-- | Parse a proc expression: @proc pat -> cmd@
procExprParser :: TokParser Expr
procExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordProc
  pat <- region "while parsing proc pattern" simplePatternParser
  expectedTok TkReservedRightArrow
  body <- region "while parsing proc body" cmdParser
  pure (\_ -> EProc pat body)

-- | Parse an expression without consuming arrow tail operators.
-- Used in command contexts where -< / -<< should be left for the
-- command parser.
exprParserNoArrowTail :: TokParser Expr
exprParserNoArrowTail =
  label "expression" exprCoreParserNoArrowTail

exprCoreParserNoArrowTail :: TokParser Expr
exprCoreParserNoArrowTail = do
  tok <- lookAhead anySingle
  base <- case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordMdo -> mdoExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordLet -> letExprParser
    TkKeywordProc -> procExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept []
  -- No arrow tail check here — leave -< / -<< for the command parser.
  -- Optional type signature: expr :: type
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  pure $ case mTypeSig of
    Just ty -> exprAnnSpan (mergeSourceSpans (getSourceSpan base) (getSourceSpan ty)) (ETypeSig base ty)
    Nothing -> base

doStmtParser :: TokParser (DoStmt Expr)
doStmtParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try doLetStmtParser <|> doBindOrExprStmtParser
    TkKeywordRec -> doRecStmtParser
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then doPatBindStmtParser
        else doBindOrExprStmtParser

startsWithPatternBind :: TokParser Bool
startsWithPatternBind =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- patternParser
    expectedTok TkReservedLeftArrow

doBindOrExprStmtParser :: TokParser (DoStmt Expr)
doBindOrExprStmtParser = withSpan $ do
  mExpr <- MP.optional . MP.try $ exprParser
  case mExpr of
    Nothing -> do
      pat <- patternParser
      expectedTok TkReservedLeftArrow
      rhs <- region "while parsing '<-' binding" exprParser
      pure (\span' -> doStmtAnnSpan span' (DoBind pat rhs))
    Just expr -> do
      tok <- lookAhead anySingle
      case lexTokenKind tok of
        TkReservedAt -> do
          pat <- patternParser
          expectedTok TkReservedLeftArrow
          rhs <- region "while parsing '<-' binding" exprParser
          pure (\span' -> doStmtAnnSpan span' (DoBind pat rhs))
        _ -> do
          mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
          case mArrow of
            Just () -> do
              pat <- liftCheck (checkPattern expr)
              rhs <- region "while parsing '<-' binding" exprParser
              pure (\span' -> doStmtAnnSpan span' (DoBind pat rhs))
            Nothing ->
              pure (\span' -> doStmtAnnSpan span' (DoExpr expr))

doPatBindStmtParser :: TokParser (DoStmt Expr)
doPatBindStmtParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' binding" exprParser
  pure (\span' -> doStmtAnnSpan span' (DoBind pat expr))

parseLetDeclsParser :: TokParser [Decl]
parseLetDeclsParser = do
  expectedTok TkKeywordLet
  bracedDeclsMaybeEmpty <|> plainDeclsMaybeEmpty

parseLetDeclsStmtParser :: TokParser [Decl]
parseLetDeclsStmtParser = do
  decls <- parseLetDeclsParser
  MP.notFollowedBy (expectedTok TkKeywordIn)
  pure decls

-- | Parse let bindings that may be empty.
-- Unlike @where@ clauses and @case@ alternatives, @let@ bindings can be
-- empty (e.g. @let {}@ or a bare @let@ with layout). GHC accepts these
-- syntactically, even though they are semantically useless.
bracedDeclsMaybeEmpty :: TokParser [Decl]
bracedDeclsMaybeEmpty = concat <$> bracedSemiSep localDeclsParser

plainDeclsMaybeEmpty :: TokParser [Decl]
plainDeclsMaybeEmpty = concat <$> plainSemiSep localDeclsParser

doLetStmtParser :: TokParser (DoStmt Expr)
doLetStmtParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (\span' -> doStmtAnnSpan span' (DoLetDecls decls))

-- | Parse a @rec@ statement inside a do-block.
doRecStmtParser :: TokParser (DoStmt Expr)
doRecStmtParser = withSpan $ do
  expectedTok TkKeywordRec
  stmts <- bracedSemiSep1 doStmtParser
  pure (\span' -> doStmtAnnSpan span' (DoRecStmt stmts))

infixExprParserExcept :: [Text] -> TokParser Expr
infixExprParserExcept forbidden = do
  lhs <- MP.try negateExprParser <|> lexpParser
  rest <-
    MP.many
      ( (,)
          <$> infixOperatorParserExcept forbidden
          <*> region "after infix operator" lexpParser
      )
  pure (foldl buildInfix lhs rest)

-- | Parse an lexp (left-expression) - includes do, if, case, let, lambda, and fexp.
lexpParser :: TokParser Expr
lexpParser =
  doExprParser <|> mdoExprParser <|> ifExprParser <|> caseExprParser <|> letExprParser <|> procExprParser <|> lambdaExprParser <|> MP.try negateExprParser <|> appExprParser

buildInfix :: Expr -> (Name, Expr) -> Expr
buildInfix lhs (op, rhs) =
  exprAnnSpan (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) (EInfix lhs op rhs)

intExprParser :: TokParser Expr
intExprParser = withExprSpanAnn $ do
  (ctor, n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (EInt, i, lexTokenText tok)
      TkIntegerHash i txt -> Just (EIntHash, i, txt)
      _ -> Nothing
  pure (\_ -> ctor n repr)

intBaseExprParser :: TokParser Expr
intBaseExprParser = withExprSpanAnn $ do
  (ctor, n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (EIntBase, i, txt)
      TkIntegerBaseHash i txt -> Just (EIntBaseHash, i, txt)
      _ -> Nothing
  pure (\_ -> ctor n repr)

floatExprParser :: TokParser Expr
floatExprParser = withExprSpanAnn $ do
  (ctor, n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (EFloat, x, txt)
      TkFloatHash x txt -> Just (EFloatHash, x, txt)
      _ -> Nothing
  pure (\_ -> ctor n repr)

charExprParser :: TokParser Expr
charExprParser = withExprSpanAnn $ do
  (ctor, c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (EChar, x, lexTokenText tok)
      TkCharHash x txt -> Just (ECharHash, x, txt)
      _ -> Nothing
  pure (\_ -> ctor c repr)

stringExprParser :: TokParser Expr
stringExprParser = withExprSpanAnn $ do
  (ctor, s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (EString, x, lexTokenText tok)
      TkStringHash x txt -> Just (EStringHash, x, txt)
      _ -> Nothing
  pure (\_ -> ctor s repr)

overloadedLabelExprParser :: TokParser Expr
overloadedLabelExprParser = withExprSpanAnn $ do
  (labelName, raw) <- tokenSatisfy "overloaded label" $ \tok ->
    case lexTokenKind tok of
      TkOverloadedLabel lbl repr -> Just (lbl, repr)
      _ -> Nothing
  pure (\_ -> EOverloadedLabel labelName raw)

appExprParser :: TokParser Expr
appExprParser = withExprSpanAnn $ do
  typeAppsEnabled <- isExtensionEnabled TypeApplications
  first <- atomOrRecordExprParser
  rest <- MP.many (appArg typeAppsEnabled)
  pure $ \span' ->
    foldl (applyArg span') first rest
  where
    appArg :: Bool -> TokParser (Either Type Expr)
    appArg typeAppsEnabled
      | typeAppsEnabled = (Left <$> typeAppArg) <|> (Right <$> atomOrRecordExprParser)
      | otherwise = Right <$> atomOrRecordExprParser

    typeAppArg :: TokParser Type
    typeAppArg = MP.try $ do
      expectedTok TkTypeApp
      typeAtomParser

    applyArg :: SourceSpan -> Expr -> Either Type Expr -> Expr
    applyArg span' fn (Left ty) = exprAnnSpan span' (ETypeApp fn ty)
    applyArg span' fn (Right arg) = exprAnnSpan span' (EApp fn arg)

-- | Parse an atom, optionally followed by one or more record construction/update syntax.
atomOrRecordExprParser :: TokParser Expr
atomOrRecordExprParser = do
  base <- atomExprParser
  applyRecordSuffixes base
  where
    applyRecordSuffixes :: Expr -> TokParser Expr
    applyRecordSuffixes e = do
      mRecordFields <- MP.optional recordBracesParser
      case mRecordFields of
        Nothing -> pure e
        Just (fields, hasWildcard) -> do
          let result = case peelExprAnn e of
                EVar name
                  | isConLikeName name ->
                      exprAnnSpan (mergeSourceSpans (getSourceSpan e) (fieldsEndSpan fields)) (ERecordCon (renderName name) (map normalizeField fields) hasWildcard)
                _ ->
                  exprAnnSpan (mergeSourceSpans (getSourceSpan e) (fieldsEndSpan fields)) (ERecordUpd e (map normalizeField fields))
          applyRecordSuffixes result

    fieldsEndSpan :: [(Text, Maybe Expr, SourceSpan)] -> SourceSpan
    fieldsEndSpan [] = NoSourceSpan
    fieldsEndSpan fs = case last fs of (_, _, sp) -> sp
    normalizeField :: (Text, Maybe Expr, SourceSpan) -> (Text, Expr)
    normalizeField (fieldName, mExpr, sp) =
      case mExpr of
        Just expr' -> (fieldName, expr')
        Nothing -> (fieldName, exprAnnSpan sp (EVar (qualifiedVarName fieldName)))

-- | Parse record braces: { field = value, field2 = value2, ... }
recordBracesParser :: TokParser ([(Text, Maybe Expr, SourceSpan)], Bool)
recordBracesParser =
  braces recordFieldListParser
  where
    recordFieldListParser = do
      rwcEnabled <- isExtensionEnabled RecordWildCards
      fields <- layoutSepEndBy recordFieldBindingParser (expectedTok TkSpecialComma)
      if rwcEnabled
        then do
          mDotDot <- MP.optional (expectedTok TkReservedDotDot)
          case mDotDot of
            Nothing -> pure (fields, False)
            Just _ -> do
              _ <- MP.optional (expectedTok TkSpecialComma)
              pure (fields, True)
        else pure (fields, False)

recordFieldBindingParser :: TokParser (Text, Maybe Expr, SourceSpan)
recordFieldBindingParser = withSpan $ do
  fieldName <- tokenSatisfy "field name" $ \tok ->
    case lexTokenKind tok of
      TkVarId name -> Just name
      TkQVarId modName name -> Just (modName <> "." <> name)
      _ -> Nothing
  mAssign <- MP.optional (expectedTok TkReservedEquals *> exprParser)
  pure (fieldName,mAssign,)

atomExprParser :: TokParser Expr
atomExprParser = do
  blockArgsEnabled <- isExtensionEnabled BlockArguments
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  let thAny = thEnabled || thFullEnabled
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkImplicitParam {} -> implicitParamExprParser
    _ ->
      MP.try prefixNegateAtomExprParser
        <|> MP.try parenOperatorExprParser
        <|> lambdaExprParser
        <|> letExprParser
        <|> (if blockArgsEnabled then MP.try doExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try mdoExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try caseExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try ifExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try procExprParser else MP.empty)
        <|> (if thAny then thQuoteExprParser else MP.empty)
        <|> (if thAny then thNameQuoteExprParser else MP.empty)
        <|> (if thFullEnabled then thSpliceExprParser else MP.empty)
        <|> quasiQuoteExprParser
        <|> parenExprParser
        <|> listExprParser
        <|> intBaseExprParser
        <|> floatExprParser
        <|> intExprParser
        <|> charExprParser
        <|> stringExprParser
        <|> overloadedLabelExprParser
        <|> wildcardExprParser
        <|> varExprParser

prefixNegateAtomExprParser :: TokParser Expr
prefixNegateAtomExprParser = withExprSpanAnn $ do
  prefixMinusTokenParser
  inner <- atomExprParser
  pure (\_ -> ENegate inner)

negateExprParser :: TokParser Expr
negateExprParser = withExprSpanAnn $ do
  _ <- minusTokenValueParser
  inner <- appExprParser
  pure (\_ -> ENegate inner)

minusTokenValueParser :: TokParser LexToken
minusTokenValueParser =
  tokenSatisfy "minus operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym "-" -> Just tok
      TkMinusOperator -> Just tok
      TkPrefixMinus -> Just tok
      _ -> Nothing

prefixMinusTokenParser :: TokParser ()
prefixMinusTokenParser =
  tokenSatisfy "prefix minus" $ \tok ->
    case lexTokenKind tok of
      TkPrefixMinus -> Just ()
      _ -> Nothing

parenOperatorExprParser :: TokParser Expr
parenOperatorExprParser = withExprSpanAnn $ do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym sym))
      TkConSym sym -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym sym))
      TkQVarSym modName sym -> Just (mkName (Just modName) NameVarSym sym)
      TkQConSym modName sym -> Just (mkName (Just modName) NameConSym sym)
      TkMinusOperator -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-"))
      TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"))
      TkReservedDoubleColon -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "::"))
      TkReservedEquals -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "="))
      TkReservedPipe -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "|"))
      TkReservedLeftArrow -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "<-"))
      TkReservedRightArrow -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "->"))
      TkReservedDoubleArrow -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "=>"))
      TkReservedDotDot -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym ".."))
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure (\_ -> EVar op)

rhsParser :: TokParser Rhs
rhsParser = label "right-hand side" (rhsParserWithArrow RhsArrowCase)

equationRhsParser :: TokParser Rhs
equationRhsParser = label "equation right-hand side" (rhsParserWithArrow RhsArrowEquation)

data RhsArrowKind = RhsArrowCase | RhsArrowEquation

rhsArrowText :: RhsArrowKind -> Text
rhsArrowText RhsArrowCase = "->"
rhsArrowText RhsArrowEquation = "="

rhsArrowTok :: RhsArrowKind -> TokParser ()
rhsArrowTok RhsArrowCase = expectedTok TkReservedRightArrow
rhsArrowTok RhsArrowEquation = expectedTok TkReservedEquals

rhsParserWithArrow :: RhsArrowKind -> TokParser Rhs
rhsParserWithArrow arrowKind = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkReservedPipe -> guardedRhssParser arrowKind
    TkReservedRightArrow | RhsArrowCase <- arrowKind -> unguardedRhsParser arrowKind
    TkReservedEquals | RhsArrowEquation <- arrowKind -> unguardedRhsParser arrowKind
    _ -> fail ("expected " <> T.unpack (rhsArrowText arrowKind) <> " or guarded right-hand side")

unguardedRhsParser :: RhsArrowKind -> TokParser Rhs
unguardedRhsParser arrowKind = withSpan $ do
  rhsArrowTok arrowKind
  body <- region (rhsContextText arrowKind) exprParser
  whereDecls <- MP.optional whereClauseParser
  pure (\span' -> UnguardedRhs span' body whereDecls)

rhsContextText :: RhsArrowKind -> Text
rhsContextText RhsArrowCase = "while parsing case alternative right-hand side"
rhsContextText RhsArrowEquation = "while parsing equation right-hand side"

guardedRhssParser :: RhsArrowKind -> TokParser Rhs
guardedRhssParser arrowKind = withSpan $ do
  grhss <- MP.some (guardedRhsParser arrowKind)
  whereDecls <- MP.optional whereClauseParser
  pure (\span' -> GuardedRhss span' grhss whereDecls)

guardedRhsParser :: RhsArrowKind -> TokParser GuardedRhs
guardedRhsParser arrowKind = withSpan $ do
  expectedTok TkReservedPipe
  guards <- layoutSepBy1 guardQualifierParser (expectedTok TkSpecialComma)
  rhsArrowTok arrowKind
  body <- exprParserExcept ["|", rhsArrowText arrowKind]
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

guardQualifierParser :: TokParser GuardQualifier
guardQualifierParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try guardLetParser <|> guardBindOrExprParser
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then guardPatBindParser
        else guardBindOrExprParser

guardBindOrExprParser :: TokParser GuardQualifier
guardBindOrExprParser = withSpan $ do
  expr <- exprParserWithTypeSigParser typeInfixParser
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- exprParser
      pure (\span' -> guardAnnSpan span' (GuardPat pat rhs))
    Nothing ->
      pure (\span' -> guardAnnSpan span' (GuardExpr expr))

guardPatBindParser :: TokParser GuardQualifier
guardPatBindParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- exprParser
  pure (\span' -> guardAnnSpan span' (GuardPat pat expr))

guardLetParser :: TokParser GuardQualifier
guardLetParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (\span' -> guardAnnSpan span' (GuardLet decls))

caseAltParser :: TokParser CaseAlt
caseAltParser = withSpan $ do
  pat <- region "while parsing case alternative" patternParser
  rhs <- region "while parsing case alternative" rhsParser
  pure $ \span' ->
    CaseAlt
      { caseAltSpan = span',
        caseAltPattern = pat,
        caseAltRhs = rhs
      }

caseExprParser :: TokParser Expr
caseExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordCase
  scrutinee <- region "while parsing case expression" exprParser
  expectedTok TkKeywordOf
  alts <- bracedAlts <|> plainAlts
  pure $ \_ -> ECase scrutinee alts
  where
    plainAlts = plainSemiSep1 caseAltParser
    bracedAlts = bracedSemiSep caseAltParser

parenExprParser :: TokParser Expr
parenExprParser = withExprSpanAnn $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mClosed <- MP.optional (expectedTok closeTok)
  case mClosed of
    Just () -> pure (\_ -> ETuple tupleFlavor [])
    Nothing ->
      if tupleFlavor == Boxed
        then MP.try (parseNegateParen closeTok) <|> parseBoxedContent closeTok
        else MP.try (parseUnboxedSumExprLeadingBars closeTok) <|> parseTupleOrParen tupleFlavor closeTok
  where
    parseNegateParen closeTok = do
      minusTok <- minusTokenValueParser
      nextTok <- lookAhead anySingle
      guard (parenNegateAllowed minusTok nextTok)
      inner <- exprParser
      expectedTok closeTok
      pure $ \span' ->
        case lexTokenKind minusTok of
          TkPrefixMinus -> ENegate inner
          _ -> EParen (exprAnnSpan span' (ENegate inner))

    parenNegateAllowed minusTok nextTok =
      case lexTokenKind minusTok of
        TkPrefixMinus -> True
        TkVarSym "-" -> tokensAdjacent minusTok nextTok
        TkMinusOperator -> False
        _ -> False

    tokensAdjacent first second =
      case (lexTokenSpan first, lexTokenSpan second) of
        ( SourceSpan {sourceSpanEndLine = firstEndLine, sourceSpanEndCol = firstEndCol},
          SourceSpan {sourceSpanStartLine = secondStartLine, sourceSpanStartCol = secondStartCol}
          ) ->
            firstEndLine == secondStartLine && firstEndCol == secondStartCol
        _ -> False

    parseBoxedContent closeTok =
      MP.try (parseSectionR [])
        <|> do
          mBase <- MP.optional (MP.try negateExprParser <|> lexpParser)
          case mBase of
            Nothing ->
              finishBoxed closeTok Nothing
            Just base -> do
              mOp <- MP.optional (infixOperatorParserExcept [])
              case mOp of
                Nothing -> do
                  mArrowSection <- MP.optional (MP.try (arrowSectionOperatorParser <* expectedTok closeTok))
                  case mArrowSection of
                    Just op ->
                      pure (\span' -> EParen (exprAnnSpan span' (ESectionL base op)))
                    Nothing -> do
                      mArrow <- MP.optional arrowTailParser
                      let withArrow = case mArrow of
                            Just (arrowOp, arrowRhs) -> exprAnnSpan (mergeSourceSpans (getSourceSpan base) (getSourceSpan arrowRhs)) (EInfix base arrowOp arrowRhs)
                            Nothing -> base
                      mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
                      let typed = case mTypeSig of
                            Just ty -> exprAnnSpan (mergeSourceSpans (getSourceSpan withArrow) (getSourceSpan ty)) (ETypeSig withArrow ty)
                            Nothing -> withArrow
                      -- View pattern arrow: expr -> expr (inside parentheses)
                      finalExpr <- maybeViewPattern typed
                      finishBoxed closeTok (Just finalExpr)
                Just op -> do
                  mClose <- MP.optional (expectedTok closeTok)
                  case mClose of
                    Just () ->
                      pure (\span' -> EParen (exprAnnSpan span' (ESectionL base op)))
                    Nothing -> do
                      rhs <- region "after infix operator" lexpParser
                      more <-
                        MP.many
                          ( MP.try
                              ( (,)
                                  <$> infixOperatorParserExcept []
                                  <*> region "after infix operator" lexpParser
                              )
                          )
                      let fullInfix = foldl buildInfix base ((op, rhs) : more)
                      mTrailingOp <- MP.optional (infixOperatorParserExcept [])
                      case mTrailingOp of
                        Just trailOp -> do
                          expectedTok closeTok
                          pure (\span' -> EParen (exprAnnSpan span' (ESectionL fullInfix trailOp)))
                        Nothing -> do
                          mArrow <- MP.optional arrowTailParser
                          let withArrow = case mArrow of
                                Just (arrowOp, arrowRhs) -> exprAnnSpan (mergeSourceSpans (getSourceSpan fullInfix) (getSourceSpan arrowRhs)) (EInfix fullInfix arrowOp arrowRhs)
                                Nothing -> fullInfix
                          mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
                          let typed = case mTypeSig of
                                Just ty -> exprAnnSpan (mergeSourceSpans (getSourceSpan withArrow) (getSourceSpan ty)) (ETypeSig withArrow ty)
                                Nothing -> withArrow
                          -- View pattern arrow: expr -> expr (inside parentheses)
                          finalExpr <- maybeViewPattern typed
                          finishBoxed closeTok (Just finalExpr)
      where
        parseSectionR forbidden = do
          op <- infixOperatorParserExcept forbidden <|> arrowSectionOperatorParser
          rhs <- exprParser
          expectedTok closeTok
          pure (\span' -> EParen (exprAnnSpan span' (ESectionR op rhs)))

        arrowSectionOperatorParser =
          tokenSatisfy "operator" $ \tok ->
            case lexTokenKind tok of
              TkArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<"))
              TkDoubleArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<<"))
              _ -> Nothing

    finishBoxed closeTok mFirst = do
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case (mFirst, mComma) of
        (Just e, Nothing) -> do
          expectedTok closeTok
          pure (\_ -> EParen e)
        (_, Just ()) -> do
          rest <- parseTupleElems closeTok
          pure (\_ -> ETuple Boxed (mFirst : rest))
        (Nothing, Nothing) ->
          fail "expected expression or closing paren"

    parseTupleOrParen tupleFlavor closeTok = do
      first <- MP.optional texprParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case (first, mComma) of
        (Just e, Nothing) ->
          case tupleFlavor of
            Boxed -> do
              expectedTok closeTok
              pure (\_ -> EParen e)
            Unboxed -> do
              mPipe <- MP.optional (expectedTok TkReservedPipe)
              case mPipe of
                Just () -> do
                  trailingBars <- MP.many (expectedTok TkReservedPipe)
                  expectedTok closeTok
                  let arity = 2 + length trailingBars
                  pure (\_ -> EUnboxedSum 0 arity e)
                Nothing -> do
                  expectedTok closeTok
                  pure (\_ -> ETuple Unboxed [Just e])
        (_, Just ()) -> do
          rest <- parseTupleElems closeTok
          pure (\_ -> ETuple tupleFlavor (first : rest))
        (Nothing, Nothing) ->
          fail "expected expression or closing paren"

    parseTupleElems closeTok = do
      e <- MP.optional texprParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Just () -> (e :) <$> parseTupleElems closeTok
        Nothing -> do
          expectedTok closeTok
          pure [e]

    parseUnboxedSumExprLeadingBars closeTok = do
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      inner <- texprParser
      trailingBars <- MP.many (expectedTok TkReservedPipe)
      expectedTok closeTok
      let arity = altIdx + 1 + length trailingBars
      pure (\_ -> EUnboxedSum altIdx arity inner)

listExprParser :: TokParser Expr
listExprParser = withExprSpanAnn $ do
  expectedTok TkSpecialLBracket
  mClose <- MP.optional (expectedTok TkSpecialRBracket)
  case mClose of
    Just () -> pure (\_ -> EList [])
    Nothing -> do
      first <- exprParser
      parseListTail first

parseListTail :: Expr -> TokParser (SourceSpan -> Expr)
parseListTail first = listCompTailParser <|> arithFromToTailParser <|> commaTailParser <|> singletonTailParser
  where
    listCompTailParser = do
      expectedTok TkReservedPipe
      firstGroup <- compStmtParser `MP.sepBy1` expectedTok TkSpecialComma
      moreGroups <- MP.many (expectedTok TkReservedPipe *> (compStmtParser `MP.sepBy1` expectedTok TkSpecialComma))
      expectedTok TkSpecialRBracket
      pure $ \_ ->
        case moreGroups of
          [] -> EListComp first firstGroup
          _ -> EListCompParallel first (firstGroup : moreGroups)

    arithFromToTailParser = do
      expectedTok TkReservedDotDot
      mTo <- MP.optional exprParser
      expectedTok TkSpecialRBracket
      pure $ \span' ->
        exprAnnSpan span' . EArithSeq $
          arithSeqAnnSpan span' $
            case mTo of
              Nothing -> ArithSeqFrom first
              Just toExpr -> ArithSeqFromTo first toExpr

    commaTailParser = do
      expectedTok TkSpecialComma
      second <- exprParser
      arithFromThenTailParser second <|> listTailParser second

    arithFromThenTailParser second = do
      expectedTok TkReservedDotDot
      mTo <- MP.optional exprParser
      expectedTok TkSpecialRBracket
      pure $ \span' ->
        exprAnnSpan span' . EArithSeq $
          arithSeqAnnSpan span' $
            case mTo of
              Nothing -> ArithSeqFromThen first second
              Just toExpr -> ArithSeqFromThenTo first second toExpr

    listTailParser second = do
      rest <- MP.many (expectedTok TkSpecialComma *> exprParser)
      expectedTok TkSpecialRBracket
      pure (\_ -> EList (first : second : rest))

    singletonTailParser = do
      expectedTok TkSpecialRBracket
      pure (\_ -> EList [first])

compStmtParser :: TokParser CompStmt
compStmtParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try compLetStmtParser <|> compGenOrGuardParser
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then compPatGenParser
        else compGenOrGuardParser

compGenOrGuardParser :: TokParser CompStmt
compGenOrGuardParser = withSpan $ do
  expr <- exprParser
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- region "while parsing '<-' generator" exprParser
      pure (\span' -> compAnnSpan span' (CompGen pat rhs))
    Nothing ->
      pure (\span' -> compAnnSpan span' (CompGuard expr))

compPatGenParser :: TokParser CompStmt
compPatGenParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' generator" exprParser
  pure (\span' -> compAnnSpan span' (CompGen pat expr))

compLetStmtParser :: TokParser CompStmt
compLetStmtParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (\span' -> compAnnSpan span' (CompLetDecls decls))

lambdaExprParser :: TokParser Expr
lambdaExprParser = withExprSpanAnn $ do
  expectedTok TkReservedBackslash
  lambdaCaseParser <|> lambdaPatsParser
  where
    lambdaCaseParser = do
      expectedTok TkKeywordCase
      alts <- bracedAlts
      pure (\_ -> ELambdaCase alts)

    lambdaPatsParser = do
      pats <- MP.some patternParser
      expectedTok TkReservedRightArrow
      body <- region "while parsing lambda body" exprParser
      pure (\_ -> ELambdaPats pats body)

    bracedAlts = bracedSemiSep caseAltParser

letExprParser :: TokParser Expr
letExprParser = withExprSpanAnn $ do
  decls <- parseLetDeclsParser
  expectedTok TkKeywordIn
  body <- exprParser
  pure (\_ -> ELetDecls decls body)

whereClauseParser :: TokParser [Decl]
whereClauseParser = do
  expectedTok TkKeywordWhere
  bracedDeclsMaybeEmpty <|> plainDeclsMaybeEmpty

localDeclsParser :: TokParser [Decl]
localDeclsParser = do
  mPragma <- MP.optional pragmaDeclParser
  case mPragma of
    Just pragmaDecl -> pure [pragmaDecl]
    Nothing -> do
      isTySig <- startsWithTypeSig
      if isTySig
        then localTypeSigDeclsParser
        else do
          tok <- lookAhead anySingle
          case lexTokenKind tok of
            TkImplicitParam {} -> pure <$> implicitParamDeclParser
            _ -> pure <$> (MP.try localFunctionDeclParser <|> localPatternDeclParser)

localTypeSigDeclsParser :: TokParser [Decl]
localTypeSigDeclsParser = do
  sig <- localTypeSigDeclParser
  let (names, ty) =
        case peelDeclAnn sig of
          DeclTypeSig sigNames sigTy -> (sigNames, sigTy)
          _ -> error "localTypeSigDeclParser must produce DeclTypeSig"
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Nothing -> pure [sig]
    Just () ->
      case names of
        [name] -> do
          rhsExpr <- exprParser
          whereDecls <- MP.optional whereClauseParser
          let bindSpan = mergeSourceSpans NoSourceSpan (getSourceSpan rhsExpr)
              pat = patternAnnSpan bindSpan (PTypeSig (patternAnnSpan bindSpan (PVar name)) ty)
              rhs = UnguardedRhs bindSpan rhsExpr whereDecls
          pure [DeclValue (PatternBind bindSpan pat rhs)]
        _ ->
          fail "local typed bindings with '=' require exactly one binder"

localTypeSigDeclParser :: TokParser Decl
localTypeSigDeclParser = withDeclSpanAnn $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure $ \_ -> DeclTypeSig names ty

localFunctionDeclParser :: TokParser Decl
localFunctionDeclParser = withDeclSpanAnn $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  rhs <- equationRhsParser
  pure $ \_ -> functionBindDecl headForm name pats rhs

localPatternDeclParser :: TokParser Decl
localPatternDeclParser = withDeclSpanAnn $ do
  pat <- patternParser
  rhs <- equationRhsParser
  pure $ \_ ->
    DeclValue (PatternBind NoSourceSpan pat rhs)

implicitParamDeclParser :: TokParser Decl
implicitParamDeclParser = withDeclSpanAnn $ do
  name <- implicitParamNameParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  whereDecls <- MP.optional whereClauseParser
  pure $ \_ ->
    DeclValue
      ( PatternBind
          NoSourceSpan
          (patternAnnSpan NoSourceSpan (PVar (mkUnqualifiedName NameVarId name)))
          (UnguardedRhs NoSourceSpan rhsExpr whereDecls)
      )

varExprParser :: TokParser Expr
varExprParser = withExprSpanAnn $ do
  name <- identifierNameParser
  pure (\_ -> EVar name)

implicitParamExprParser :: TokParser Expr
implicitParamExprParser = withExprSpanAnn $ do
  name <- implicitParamNameParser
  pure (\_ -> EVar (qualifyName Nothing (mkUnqualifiedName NameVarId name)))

wildcardExprParser :: TokParser Expr
wildcardExprParser = withExprSpanAnn $ do
  expectedTok TkKeywordUnderscore
  pure (\_ -> EVar (qualifyName Nothing (mkUnqualifiedName NameVarId "_")))

-- | Parse Template Haskell quote brackets
thQuoteExprParser :: TokParser Expr
thQuoteExprParser =
  thExpQuoteParser
    <|> thTypedQuoteParser
    <|> thDeclQuoteParser
    <|> thTypeQuoteParser
    <|> thPatQuoteParser

thExpQuoteParser :: TokParser Expr
thExpQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHExpQuoteOpen
  body <- exprParser
  expectedTok TkTHExpQuoteClose
  pure (\_ -> ETHExpQuote body)

thTypedQuoteParser :: TokParser Expr
thTypedQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHTypedQuoteOpen
  body <- exprParser
  expectedTok TkTHTypedQuoteClose
  pure (\_ -> ETHTypedQuote body)

thDeclQuoteParser :: TokParser Expr
thDeclQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHDeclQuoteOpen
  decls <- bracedSemiSep declParser <|> plainSemiSep declParser
  expectedTok TkTHExpQuoteClose
  pure (\_ -> ETHDeclQuote decls)

thTypeQuoteParser :: TokParser Expr
thTypeQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHTypeQuoteOpen
  ty <- typeParser
  expectedTok TkTHExpQuoteClose
  pure (\_ -> ETHTypeQuote ty)

thPatQuoteParser :: TokParser Expr
thPatQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHPatQuoteOpen
  pat <- patternParser
  expectedTok TkTHExpQuoteClose
  pure (\_ -> ETHPatQuote pat)

thSpliceExprParser :: TokParser Expr
thSpliceExprParser = thTypedSpliceParser <|> thUntypedSpliceParser

thUntypedSpliceParser :: TokParser Expr
thUntypedSpliceParser = withExprSpanAnn $ do
  expectedTok TkTHSplice
  body <- thSpliceBody
  pure (\_ -> ETHSplice body)

thTypedSpliceParser :: TokParser Expr
thTypedSpliceParser = withExprSpanAnn $ do
  expectedTok TkTHTypedSplice
  body <- thSpliceBody
  pure (\_ -> ETHTypedSplice body)

thSpliceBody :: TokParser Expr
thSpliceBody =
  parenSpliceBody <|> bareSpliceBody
  where
    parenSpliceBody = withExprSpanAnn $ do
      body <- parens exprParser
      pure (\_ -> EParen body)
    bareSpliceBody = withExprSpanAnn $ do
      name <- identifierNameParser
      pure (\_ -> EVar name)

thNameQuoteExprParser :: TokParser Expr
thNameQuoteExprParser = thValueNameQuoteParser <|> thTypeNameQuoteParser

thValueNameQuoteParser :: TokParser Expr
thValueNameQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHQuoteTick
  name <- identifierTextParser <|> parenOperatorTextParser
  pure (\_ -> ETHNameQuote name)

thTypeNameQuoteParser :: TokParser Expr
thTypeNameQuoteParser = withExprSpanAnn $ do
  expectedTok TkTHTypeQuoteTick
  name <- identifierNameParser <|> parenOperatorNameParser
  pure (\_ -> ETHTypeNameQuote name)

parenOperatorTextParser :: TokParser Text
parenOperatorTextParser = do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym -> Just sym
      TkConSym sym -> Just sym
      TkReservedColon -> Just ":"
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure ("(" <> op <> ")")

parenOperatorNameParser :: TokParser Name
parenOperatorNameParser = do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym sym))
      TkConSym sym -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym sym))
      TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"))
      TkReservedRightArrow -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "->"))
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure op

quasiQuoteExprParser :: TokParser Expr
quasiQuoteExprParser =
  tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (exprAnnSpan (lexTokenSpan tok) (EQuasiQuote quoter body))
      _ -> Nothing
