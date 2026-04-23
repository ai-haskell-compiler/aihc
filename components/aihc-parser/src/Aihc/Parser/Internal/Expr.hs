{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aihc.Parser.Internal.Expr
  ( exprParser,
    atomExprParser,
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
import Aihc.Parser.Internal.Decl (declParser, fixityDeclParser, pragmaDeclParser, typeSigDeclParser)
import Aihc.Parser.Internal.Pattern (appPatternParser, patternParser, simplePatternParser)
import Aihc.Parser.Internal.Type (typeAppParser, typeAtomParser, typeHeadInfixParser, typeInfixOperatorParser, typeInfixParser, typeParser)
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), mkFoundToken)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Text (Text)
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

-- | Parse an expression, then optionally consume @<-@ and a right-hand side.
-- If the arrow is present, the expression is converted to a pattern via
-- 'checkPattern' and the result is a bind; otherwise it is an expression.
exprOrPatternBindParser ::
  TokParser Expr ->
  TokParser Expr ->
  (Pattern -> Expr -> a) ->
  (Expr -> a) ->
  TokParser a
exprOrPatternBindParser exprP rhsP bindCtor exprCtor = do
  expr <- exprP
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      bindCtor pat <$> rhsP
    Nothing -> pure (exprCtor expr)

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

exprCoreParserWithoutTypeSigExcept :: [Text] -> TokParser Expr
exprCoreParserWithoutTypeSigExcept forbiddenInfix = do
  mSCC <- optionalHiddenPragma getSCCLabel
  case mSCC of
    Just sccLabel -> EPragma (PragmaSCC sccLabel) <$> exprCoreParserWithoutTypeSigExcept forbiddenInfix
    Nothing -> exprCoreParserWithoutTypeSigBody forbiddenInfix

exprCoreParserWithoutTypeSigBody :: [Text] -> TokParser Expr
exprCoreParserWithoutTypeSigBody forbiddenInfix = do
  tok <- lookAhead anySingle
  base <- case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordMdo -> mdoExprParser
    TkQualifiedDo {} -> qualifiedDoExprParser
    TkQualifiedMdo {} -> qualifiedMdoExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordLet -> letExprParser
    TkKeywordProc -> procExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept forbiddenInfix
  rest <- MP.many ((,) <$> infixOperatorParserExcept forbiddenInfix <*> region "after infix operator" lexpParser)
  afterArrow <- MP.optional arrowTailParser
  let withInfix = foldl buildInfix base rest
  pure $ case afterArrow of
    Just (op, rhs) -> EInfix withInfix op rhs
    Nothing -> withInfix

exprCoreParserWithTypeSigParserExcept :: TokParser Type -> [Text] -> TokParser Expr
exprCoreParserWithTypeSigParserExcept typeSigParser forbiddenInfix = do
  withArrow <- exprCoreParserWithoutTypeSigExcept forbiddenInfix
  -- Optional type signature: expr :: type
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeSigParser)
  pure $ case mTypeSig of
    Just ty -> ETypeSig withArrow ty
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
    Just () -> EInfix lhs viewPatArrowName <$> texprParser
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
classicIfExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordIf
  cond <- region "while parsing if condition" exprParser
  skipSemicolons
  expectedTok TkKeywordThen
  yes <- region "while parsing then branch" exprParser
  skipSemicolons
  expectedTok TkKeywordElse
  no <- region "while parsing else branch" exprParser
  pure (EIf cond yes no)

multiWayIfExprParser :: TokParser Expr
multiWayIfExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordIf
  rhss <- braces (MP.some multiWayIfAlternative)
  pure (EMultiWayIf rhss)

multiWayIfAlternative :: TokParser GuardedRhs
multiWayIfAlternative = withSpan $ do
  expectedTok TkReservedPipe
  guards <- layoutSepBy1 (guardQualifierParser RhsArrowCase) (expectedTok TkSpecialComma)
  expectedTok TkReservedRightArrow
  body <- exprParser
  pure $ \span' ->
    GuardedRhs
      { guardedRhsAnns = [mkAnnotation span'],
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

doExprParser :: TokParser Expr
doExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordDo
  stmts <- bracedSemiSep1 doStmtParser
  pure (EDo stmts DoPlain)

mdoExprParser :: TokParser Expr
mdoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordMdo
  stmts <- bracedSemiSep1 doStmtParser
  pure (EDo stmts DoMdo)

qualifiedDoExprParser :: TokParser Expr
qualifiedDoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  modName <- tokenSatisfy "qualified do" $ \tok ->
    case lexTokenKind tok of
      TkQualifiedDo m -> Just m
      _ -> Nothing
  stmts <- bracedSemiSep1 doStmtParser
  pure (EDo stmts (DoQualified modName))

qualifiedMdoExprParser :: TokParser Expr
qualifiedMdoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  modName <- tokenSatisfy "qualified mdo" $ \tok ->
    case lexTokenKind tok of
      TkQualifiedMdo m -> Just m
      _ -> Nothing
  stmts <- bracedSemiSep1 doStmtParser
  pure (EDo stmts (DoQualifiedMdo modName))

-- | Parse a proc expression: @proc pat -> cmd@
procExprParser :: TokParser Expr
procExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordProc
  pat <- region "while parsing proc pattern" simplePatternParser
  expectedTok TkReservedRightArrow
  body <- region "while parsing proc body" cmdParser
  pure (EProc pat body)

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
    TkQualifiedDo {} -> qualifiedDoExprParser
    TkQualifiedMdo {} -> qualifiedMdoExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordLet -> letExprParser
    TkKeywordProc -> procExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept []
  -- No arrow tail check here — leave -< / -<< for the command parser.
  -- Optional type signature: expr :: type
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  pure $ case mTypeSig of
    Just ty -> ETypeSig base ty
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
doBindOrExprStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  mExpr <- MP.optional . MP.try $ exprParser
  case mExpr of
    Nothing -> do
      pat <- patternParser
      expectedTok TkReservedLeftArrow
      rhs <- region "while parsing '<-' binding" exprParser
      pure (DoBind pat rhs)
    Just expr -> do
      tok <- lookAhead anySingle
      case lexTokenKind tok of
        TkReservedAt -> do
          pat <- patternParser
          expectedTok TkReservedLeftArrow
          rhs <- region "while parsing '<-' binding" exprParser
          pure (DoBind pat rhs)
        _ -> do
          mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
          case mArrow of
            Just () -> do
              pat <- liftCheck (checkPattern expr)
              rhs <- region "while parsing '<-' binding" exprParser
              pure (DoBind pat rhs)
            Nothing ->
              pure (DoExpr expr)

doPatBindStmtParser :: TokParser (DoStmt Expr)
doPatBindStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' binding" exprParser
  pure (DoBind pat expr)

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
doLetStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  DoLetDecls <$> parseLetDeclsStmtParser

-- | Parse a @rec@ statement inside a do-block.
doRecStmtParser :: TokParser (DoStmt Expr)
doRecStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do
  expectedTok TkKeywordRec
  stmts <- bracedSemiSep1 doStmtParser
  pure (DoRecStmt stmts)

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
lexpParser = do
  mSCC <- optionalHiddenPragma getSCCLabel
  case mSCC of
    Just sccLabel -> EPragma (PragmaSCC sccLabel) <$> lexpParser
    Nothing -> doExprParser <|> mdoExprParser <|> qualifiedDoExprParser <|> qualifiedMdoExprParser <|> ifExprParser <|> caseExprParser <|> letExprParser <|> procExprParser <|> lambdaExprParser <|> MP.try negateExprParser <|> appExprParser

getSCCLabel :: Pragma -> Maybe Text
getSCCLabel (PragmaSCC sccLabel) = Just sccLabel
getSCCLabel _ = Nothing

buildInfix :: Expr -> (Name, Expr) -> Expr
buildInfix lhs (op, rhs) =
  EInfix lhs op rhs

intExprParser :: TokParser Expr
intExprParser =
  tokenExprParser "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i nt -> Just (EInt i nt (lexTokenText tok))
      _ -> Nothing

floatExprParser :: TokParser Expr
floatExprParser =
  tokenExprParser "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x ft -> Just (EFloat x ft (lexTokenText tok))
      _ -> Nothing

tokenExprParser :: String -> (LexToken -> Maybe Expr) -> TokParser Expr
tokenExprParser expected matchToken =
  withSpanAnn (EAnn . mkAnnotation) (tokenSatisfy expected matchToken)

charExprParser :: TokParser Expr
charExprParser =
  tokenExprParser "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (EChar x (lexTokenText tok))
      TkCharHash x txt -> Just (ECharHash x txt)
      _ -> Nothing

stringExprParser :: TokParser Expr
stringExprParser =
  tokenExprParser "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (EString x (lexTokenText tok))
      TkStringHash x txt -> Just (EStringHash x txt)
      _ -> Nothing

overloadedLabelExprParser :: TokParser Expr
overloadedLabelExprParser =
  tokenExprParser "overloaded label" $ \tok ->
    case lexTokenKind tok of
      TkOverloadedLabel lbl repr -> Just (EOverloadedLabel lbl repr)
      _ -> Nothing

appExprParser :: TokParser Expr
appExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  typeAppsEnabled <- isExtensionEnabled TypeApplications
  first <- atomOrRecordExprParser
  rest <- MP.many (appArg typeAppsEnabled)
  pure $
    foldl applyArg first rest
  where
    appArg :: Bool -> TokParser (Either Type Expr)
    appArg typeAppsEnabled
      | typeAppsEnabled = (Left <$> typeAppArg) <|> (Right <$> atomOrRecordExprParser)
      | otherwise = Right <$> atomOrRecordExprParser

    typeAppArg :: TokParser Type
    typeAppArg = MP.try $ do
      expectedTok TkTypeApp
      typeAtomParser

    applyArg :: Expr -> Either Type Expr -> Expr
    applyArg fn (Left ty) = ETypeApp fn ty
    applyArg fn (Right arg) = EApp fn arg

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
                      ERecordCon name (map normalizeField fields) hasWildcard
                _ ->
                  ERecordUpd e (map normalizeField fields)
          applyRecordSuffixes result

    normalizeField :: (Name, Maybe Expr, SourceSpan) -> (Name, Expr)
    normalizeField (fieldName, mExpr, sp) =
      case mExpr of
        Just expr' -> (fieldName, expr')
        Nothing -> (fieldName, EAnn (mkAnnotation sp) (EVar fieldName))

-- | Parse record braces: { field = value, field2 = value2, ... }
recordBracesParser :: TokParser ([(Name, Maybe Expr, SourceSpan)], Bool)
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

recordFieldBindingParser :: TokParser (Name, Maybe Expr, SourceSpan)
recordFieldBindingParser = withSpan $ do
  fieldName <- identifierNameParser <|> parens operatorNameParser
  mAssign <- MP.optional (expectedTok TkReservedEquals *> exprParser)
  pure (fieldName,mAssign,)

atomExprParser :: TokParser Expr
atomExprParser = do
  blockArgsEnabled <- isExtensionEnabled BlockArguments
  thAny <- thAnyEnabled
  explicitNamespacesEnabled <- isExtensionEnabled ExplicitNamespaces
  requiredTypeArgumentsEnabled <- isExtensionEnabled RequiredTypeArguments
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkImplicitParam {} -> implicitParamExprParser
    TkKeywordType
      | explicitNamespacesEnabled || requiredTypeArgumentsEnabled -> explicitTypeExprParser
    _ ->
      MP.try prefixNegateAtomExprParser
        <|> MP.try parenOperatorExprParser
        <|> lambdaExprParser
        <|> letExprParser
        <|> (if blockArgsEnabled then MP.try doExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try mdoExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try qualifiedDoExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try qualifiedMdoExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try caseExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try ifExprParser else MP.empty)
        <|> (if blockArgsEnabled then MP.try procExprParser else MP.empty)
        <|> (if thAny then thQuoteExprParser else MP.empty)
        <|> (if thAny then thNameQuoteExprParser else MP.empty)
        <|> (if thAny then thTypedSpliceParser else MP.empty)
        <|> (if thAny then thUntypedSpliceParser else MP.empty)
        <|> quasiQuoteExprParser
        <|> parenExprParser
        <|> listExprParser
        <|> intExprParser
        <|> floatExprParser
        <|> charExprParser
        <|> stringExprParser
        <|> overloadedLabelExprParser
        <|> wildcardExprParser
        <|> varExprParser

explicitTypeExprParser :: TokParser Expr
explicitTypeExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  ETypeSyntax TypeSyntaxExplicitNamespace <$> typeParser

prefixNegateAtomExprParser :: TokParser Expr
prefixNegateAtomExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  prefixMinusTokenParser
  ENegate <$> atomExprParser

negateExprParser :: TokParser Expr
negateExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  _ <- minusTokenValueParser
  ENegate <$> appExprParser

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
parenOperatorExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkSpecialLParen
  op <- operatorExprNameParser
  expectedTok TkSpecialRParen
  pure (EVar op)

operatorExprNameParser :: TokParser Name
operatorExprNameParser =
  tokenSatisfy "operator" $ \tok ->
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
    _ ->
      MP.customFailure
        UnexpectedTokenExpecting
          { unexpectedFound = Just (mkFoundToken tok),
            unexpectedExpecting = rhsArrowText arrowKind <> " or guarded right-hand side",
            unexpectedContext = []
          }

unguardedRhsParser :: RhsArrowKind -> TokParser Rhs
unguardedRhsParser arrowKind = withSpan $ do
  rhsArrowTok arrowKind
  body <- region (rhsContextText arrowKind) exprParser
  whereDecls <- MP.optional whereClauseParser
  pure (\span' -> UnguardedRhs [mkAnnotation span'] body whereDecls)

rhsContextText :: RhsArrowKind -> Text
rhsContextText RhsArrowCase = "while parsing case alternative right-hand side"
rhsContextText RhsArrowEquation = "while parsing equation right-hand side"

guardedRhssParser :: RhsArrowKind -> TokParser Rhs
guardedRhssParser arrowKind = withSpan $ do
  grhss <- MP.some (guardedRhsParser arrowKind)
  whereDecls <- MP.optional whereClauseParser
  pure (\span' -> GuardedRhss [mkAnnotation span'] grhss whereDecls)

guardedRhsParser :: RhsArrowKind -> TokParser GuardedRhs
guardedRhsParser arrowKind = withSpan $ do
  expectedTok TkReservedPipe
  guards <- layoutSepBy1 (guardQualifierParser arrowKind) (expectedTok TkSpecialComma)
  rhsArrowTok arrowKind
  body <- exprParserExcept ["|", rhsArrowText arrowKind]
  pure $ \span' ->
    GuardedRhs
      { guardedRhsAnns = [mkAnnotation span'],
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

-- | Parse a guard qualifier. The 'RhsArrowKind' determines the type parser
-- used for type signatures in guard expressions: in equation context (@=@),
-- the full 'typeParser' is used (allowing @->@ in types); in case/multi-way-if
-- context (@->@), 'typeInfixParser' is used to avoid consuming the alternative
-- arrow as a function type arrow.
guardQualifierParser :: RhsArrowKind -> TokParser GuardQualifier
guardQualifierParser arrowKind = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try guardLetParser <|> guardBindOrExprParser arrowKind
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then guardPatBindParser
        else guardBindOrExprParser arrowKind

-- | Parse a guard expression or pattern bind. The 'RhsArrowKind' selects the
-- type parser for @::@ annotations: 'RhsArrowEquation' uses 'typeParser'
-- (which includes @->@), while 'RhsArrowCase' uses 'typeInfixParser' (which
-- does not), matching GHC's behaviour.
guardBindOrExprParser :: RhsArrowKind -> TokParser GuardQualifier
guardBindOrExprParser arrowKind =
  withSpanAnn (GuardAnn . mkAnnotation) $
    exprOrPatternBindParser
      (exprParserWithTypeSigParser (guardTypeSigParser arrowKind))
      exprParser
      GuardPat
      GuardExpr

guardPatBindParser :: TokParser GuardQualifier
guardPatBindParser = withSpanAnn (GuardAnn . mkAnnotation) $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  GuardPat pat <$> exprParser

guardLetParser :: TokParser GuardQualifier
guardLetParser = withSpanAnn (GuardAnn . mkAnnotation) $ do
  GuardLet <$> parseLetDeclsStmtParser

-- | Select the type parser for guard expression type signatures based on the
-- RHS arrow kind.  In equation context the full @typeParser@ is used so that
-- @->@ is accepted inside types (e.g. @x | y :: Int -> Int = z@).  In case
-- and multi-way-if contexts the arrow would be ambiguous with the alternative
-- arrow, so @typeInfixParser@ is used instead, matching GHC behaviour.
guardTypeSigParser :: RhsArrowKind -> TokParser Type
guardTypeSigParser RhsArrowEquation = typeParser
guardTypeSigParser RhsArrowCase = typeInfixParser

caseAltParser :: TokParser CaseAlt
caseAltParser = withSpan $ do
  pat <- region "while parsing case alternative" patternParser
  rhs <- region "while parsing case alternative" rhsParser
  pure $ \span' ->
    CaseAlt
      { caseAltAnns = [mkAnnotation span'],
        caseAltPattern = pat,
        caseAltRhs = rhs
      }

lambdaCaseAltParser :: TokParser LambdaCaseAlt
lambdaCaseAltParser = withSpan $ do
  pats <- region "while parsing lambda-cases alternative" (MP.some simplePatternParser)
  rhs <- region "while parsing lambda-cases alternative" rhsParser
  pure $ \span' ->
    LambdaCaseAlt
      { lambdaCaseAltAnns = [mkAnnotation span'],
        lambdaCaseAltPats = pats,
        lambdaCaseAltRhs = rhs
      }

caseExprParser :: TokParser Expr
caseExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordCase
  scrutinee <- region "while parsing case expression" exprParser
  expectedTok TkKeywordOf
  alts <- bracedAlts <|> plainAlts <|> pure []
  pure (ECase scrutinee alts)
  where
    plainAlts = plainSemiSep1 caseAltParser
    bracedAlts = bracedSemiSep caseAltParser

parenExprParser :: TokParser Expr
parenExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mClosed <- MP.optional (expectedTok closeTok)
  case mClosed of
    Just () -> pure (ETuple tupleFlavor [])
    Nothing ->
      if tupleFlavor == Boxed
        then MP.try (parseNegateParen closeTok) <|> parseBoxedContent closeTok
        else MP.try (parseUnboxedSumExprLeadingBars closeTok) <|> parseTupleOrParen tupleFlavor closeTok
  where
    parseNegateParen closeTok = do
      minusTok <- minusTokenValueParser
      nextTok <- lookAhead anySingle
      guard (parenNegateAllowed minusTok nextTok)
      -- Parse only the application-level expression as the negation's
      -- immediate operand.  This matches GHC, where negation binds tighter
      -- than any infix operator, so @(-l - 1)@ is @((negate l) - 1)@, not
      -- @(negate (l - 1))@.
      negOperand <- appExprParser
      let negBase = ENegate negOperand
      -- Continue with any infix operator chain, type signature, and view
      -- pattern that may follow the negated expression inside the parens.
      rest <-
        MP.many
          ( MP.try
              ( (,)
                  <$> infixOperatorParserExcept []
                  <*> region "after infix operator" lexpParser
              )
          )
      let withInfix = foldl buildInfix negBase rest
      mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
      let typed = case mTypeSig of
            Just ty -> ETypeSig withInfix ty
            Nothing -> withInfix
      finalExpr <- maybeViewPattern typed
      expectedTok closeTok
      -- The negation is already embedded in finalExpr (as negBase).
      -- With TkPrefixMinus (LexicalNegation), the surrounding parens are
      -- just grouping — no EParen wrapper.  Otherwise the parens are part
      -- of the negation-section syntax and need an EParen wrapper.
      pure $
        case lexTokenKind minusTok of
          TkPrefixMinus -> finalExpr
          _ -> EParen finalExpr

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
                      pure (EParen (ESectionL base op))
                    Nothing -> do
                      mArrow <- MP.optional arrowTailParser
                      let withArrow = case mArrow of
                            Just (arrowOp, arrowRhs) -> EInfix base arrowOp arrowRhs
                            Nothing -> base
                      mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
                      let typed = case mTypeSig of
                            Just ty -> ETypeSig withArrow ty
                            Nothing -> withArrow
                      -- View pattern arrow: expr -> expr (inside parentheses)
                      finalExpr <- maybeViewPattern typed
                      finishBoxed closeTok (Just finalExpr)
                Just op -> do
                  mClose <- MP.optional (expectedTok closeTok)
                  case mClose of
                    Just () ->
                      pure (EParen (ESectionL base op))
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
                          pure (EParen (ESectionL fullInfix trailOp))
                        Nothing -> do
                          mArrow <- MP.optional arrowTailParser
                          let withArrow = case mArrow of
                                Just (arrowOp, arrowRhs) -> EInfix fullInfix arrowOp arrowRhs
                                Nothing -> fullInfix
                          mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
                          let typed = case mTypeSig of
                                Just ty -> ETypeSig withArrow ty
                                Nothing -> withArrow
                          -- View pattern arrow: expr -> expr (inside parentheses)
                          finalExpr <- maybeViewPattern typed
                          finishBoxed closeTok (Just finalExpr)
      where
        parseSectionR forbidden = do
          op <- infixOperatorParserExcept forbidden <|> arrowSectionOperatorParser
          rhs <- exprParser
          expectedTok closeTok
          pure (EParen (ESectionR op rhs))

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
          pure (EParen e)
        (_, Just ()) -> do
          rest <- parseTupleElems closeTok
          pure (ETuple Boxed (mFirst : rest))
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
              pure (EParen e)
            Unboxed -> do
              mPipe <- MP.optional (expectedTok TkReservedPipe)
              case mPipe of
                Just () -> do
                  trailingBars <- MP.many (expectedTok TkReservedPipe)
                  expectedTok closeTok
                  let arity = 2 + length trailingBars
                  pure (EUnboxedSum 0 arity e)
                Nothing -> do
                  expectedTok closeTok
                  pure (ETuple Unboxed [Just e])
        (_, Just ()) -> do
          rest <- parseTupleElems closeTok
          pure (ETuple tupleFlavor (first : rest))
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
      pure (EUnboxedSum altIdx arity inner)

listExprParser :: TokParser Expr
listExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkSpecialLBracket
  mClose <- MP.optional (expectedTok TkSpecialRBracket)
  case mClose of
    Just () -> pure (EList [])
    Nothing -> do
      first <- exprParser
      parseListTail first

parseListTail :: Expr -> TokParser Expr
parseListTail first = listCompTailParser <|> arithFromToTailParser <|> commaTailParser <|> singletonTailParser
  where
    listCompTailParser = do
      expectedTok TkReservedPipe
      firstGroup <- compStmtParser `MP.sepBy1` expectedTok TkSpecialComma
      moreGroups <- MP.many (expectedTok TkReservedPipe *> (compStmtParser `MP.sepBy1` expectedTok TkSpecialComma))
      expectedTok TkSpecialRBracket
      pure $
        case moreGroups of
          [] -> EListComp first firstGroup
          _ -> EListCompParallel first (firstGroup : moreGroups)

    arithFromToTailParser = do
      expectedTok TkReservedDotDot
      mTo <- MP.optional exprParser
      expectedTok TkSpecialRBracket
      pure $
        EArithSeq $
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
      pure $
        EArithSeq $
          case mTo of
            Nothing -> ArithSeqFromThen first second
            Just toExpr -> ArithSeqFromThenTo first second toExpr

    listTailParser second = do
      rest <- MP.many (expectedTok TkSpecialComma *> exprParser)
      expectedTok TkSpecialRBracket
      pure (EList (first : second : rest))

    singletonTailParser = do
      expectedTok TkSpecialRBracket
      pure (EList [first])

compStmtParser :: TokParser CompStmt
compStmtParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordLet -> MP.try compLetStmtParser <|> compGenOrGuardParser
    TkKeywordThen -> compTransformStmtParser <|> compGenOrGuardParser
    _ -> do
      isPatternBind <- startsWithPatternBind
      if isPatternBind
        then compPatGenParser
        else compGenOrGuardParser

-- | Parse a TransformListComp qualifier: @then f@, @then f by e@,
-- @then group by e using f@, or @then group using f@.
-- Only attempted when the 'TransformListComp' extension is enabled.
compTransformStmtParser :: TokParser CompStmt
compTransformStmtParser = MP.try $ withSpanAnn (CompAnn . mkAnnotation) $ do
  enabled <- isExtensionEnabled TransformListComp
  guard enabled
  expectedTok TkKeywordThen
  -- Check for 'group' forms first
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkVarId "group" -> compGroupStmtParser
    _ -> compThenStmtParser

-- | Parse @group by e using f@ or @group using f@ (after 'then' has been consumed).
compGroupStmtParser :: TokParser CompStmt
compGroupStmtParser = do
  varIdTok "group"
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkVarId "by" -> do
      varIdTok "by"
      e <- compTransformExprParser
      varIdTok "using"
      CompGroupByUsing e <$> exprParser
    TkVarId "using" -> do
      varIdTok "using"
      CompGroupUsing <$> exprParser
    _ -> fail "expected 'by' or 'using' after 'group'"

-- | Parse @f@ or @f by e@ (after 'then' has been consumed).
-- Uses a restricted expression parser that excludes 'by' and 'using'
-- from being consumed as variable identifiers at the top level.
compThenStmtParser :: TokParser CompStmt
compThenStmtParser = do
  f <- compTransformExprParser
  mBy <- MP.optional (varIdTok "by")
  case mBy of
    Just () -> CompThenBy f <$> exprParser
    Nothing -> pure (CompThen f)

-- | Expression parser for TransformListComp context.
-- Parses an expression but treats bare 'by' and 'using' as terminators
-- (they are not consumed as variable identifiers at the application level).
compTransformExprParser :: TokParser Expr
compTransformExprParser =
  label "expression" $ do
    tok <- lookAhead anySingle
    base <- case lexTokenKind tok of
      TkKeywordDo -> doExprParser
      TkKeywordMdo -> mdoExprParser
      TkQualifiedDo {} -> qualifiedDoExprParser
      TkQualifiedMdo {} -> qualifiedMdoExprParser
      TkKeywordIf -> ifExprParser
      TkKeywordLet -> letExprParser
      TkKeywordProc -> procExprParser
      TkReservedBackslash -> lambdaExprParser
      _ -> compTransformInfixExprParser
    rest <- MP.many ((,) <$> infixOperatorParserExcept [] <*> compTransformLexpParser)
    pure (foldl buildInfix base rest)

compTransformLexpParser :: TokParser Expr
compTransformLexpParser =
  doExprParser
    <|> mdoExprParser
    <|> qualifiedDoExprParser
    <|> qualifiedMdoExprParser
    <|> ifExprParser
    <|> caseExprParser
    <|> letExprParser
    <|> procExprParser
    <|> lambdaExprParser
    <|> MP.try negateExprParser
    <|> compTransformAppExprParser

compTransformInfixExprParser :: TokParser Expr
compTransformInfixExprParser = do
  lhs <- MP.try negateExprParser <|> compTransformLexpParser
  rest <-
    MP.many
      ( (,)
          <$> infixOperatorParserExcept []
          <*> region "after infix operator" compTransformLexpParser
      )
  pure (foldl buildInfix lhs rest)

compTransformAppExprParser :: TokParser Expr
compTransformAppExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  typeAppsEnabled <- isExtensionEnabled TypeApplications
  first <- compTransformAtomExprParser
  rest <- MP.many (compTransformAppArg typeAppsEnabled)
  pure $
    foldl applyArg first rest
  where
    compTransformAppArg :: Bool -> TokParser (Either Type Expr)
    compTransformAppArg typeAppsEnabled
      | typeAppsEnabled = (Left <$> compTransformTypeAppArg) <|> (Right <$> compTransformAtomExprParser)
      | otherwise = Right <$> compTransformAtomExprParser

    compTransformTypeAppArg :: TokParser Type
    compTransformTypeAppArg = MP.try $ do
      expectedTok TkTypeApp
      typeAtomParser

    applyArg :: Expr -> Either Type Expr -> Expr
    applyArg fn (Left ty) = ETypeApp fn ty
    applyArg fn (Right arg) = EApp fn arg

-- | Like 'atomExprParser' but rejects bare 'by' and 'using' identifiers.
-- These are treated as contextual keywords in TransformListComp context.
compTransformAtomExprParser :: TokParser Expr
compTransformAtomExprParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkVarId "by" -> MP.empty
    TkVarId "using" -> MP.empty
    _ -> atomExprParser

compGenOrGuardParser :: TokParser CompStmt
compGenOrGuardParser =
  withSpanAnn (CompAnn . mkAnnotation) $
    exprOrPatternBindParser exprParser (region "while parsing '<-' generator" exprParser) CompGen CompGuard

compPatGenParser :: TokParser CompStmt
compPatGenParser = withSpanAnn (CompAnn . mkAnnotation) $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' generator" exprParser
  pure (CompGen pat expr)

compLetStmtParser :: TokParser CompStmt
compLetStmtParser = withSpanAnn (CompAnn . mkAnnotation) $ do
  CompLetDecls <$> parseLetDeclsStmtParser

lambdaExprParser :: TokParser Expr
lambdaExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkReservedBackslash
  MP.try lambdaCaseParser <|> MP.try lambdaCasesParser <|> lambdaPatsParser
  where
    lambdaCaseParser = do
      expectedTok TkKeywordCase
      ELambdaCase <$> bracedCaseAlts

    lambdaCasesParser = do
      varIdTok "cases"
      ELambdaCases <$> (bracedLambdaCaseAlts <|> plainLambdaCaseAlts)

    lambdaPatsParser = do
      pats <- MP.some simplePatternParser
      expectedTok TkReservedRightArrow
      body <- region "while parsing lambda body" exprParser
      pure (ELambdaPats pats body)

    bracedCaseAlts = bracedSemiSep caseAltParser
    bracedLambdaCaseAlts = bracedSemiSep lambdaCaseAltParser
    plainLambdaCaseAlts = plainSemiSep1 lambdaCaseAltParser

letExprParser :: TokParser Expr
letExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  decls <- parseLetDeclsParser
  expectedTok TkKeywordIn
  ELetDecls decls <$> exprParser

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
            TkKeywordInfix -> pure <$> fixityDeclParser Infix
            TkKeywordInfixl -> pure <$> fixityDeclParser InfixL
            TkKeywordInfixr -> pure <$> fixityDeclParser InfixR
            _ -> pure <$> (MP.try localFunctionDeclParser <|> localPatternDeclParser)

localTypeSigDeclsParser :: TokParser [Decl]
localTypeSigDeclsParser = do
  sig <- typeSigDeclParser
  let (names, ty) =
        case peelDeclAnn sig of
          DeclTypeSig sigNames sigTy -> (sigNames, sigTy)
          _ -> error "typeSigDeclParser must produce DeclTypeSig"
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Nothing -> pure [sig]
    Just () ->
      case names of
        [name] -> do
          rhsExpr <- exprParser
          whereDecls <- MP.optional whereClauseParser
          let bindAnns = []
              pat = PTypeSig (PVar name) ty
              rhs = UnguardedRhs bindAnns rhsExpr whereDecls
          pure [DeclValue (PatternBind pat rhs)]
        _ ->
          fail "local typed bindings with '=' require exactly one binder"

localFunctionDeclParser :: TokParser Decl
localFunctionDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  functionBindDecl headForm name pats <$> equationRhsParser

localPatternDeclParser :: TokParser Decl
localPatternDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  pat <- patternParser
  DeclValue . PatternBind pat <$> equationRhsParser

implicitParamDeclParser :: TokParser Decl
implicitParamDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  name <- implicitParamNameParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  whereDecls <- MP.optional whereClauseParser
  pure $
    DeclValue
      ( PatternBind
          (PVar (mkUnqualifiedName NameVarId name))
          (UnguardedRhs [] rhsExpr whereDecls)
      )

varExprParser :: TokParser Expr
varExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  EVar <$> identifierNameParser

implicitParamExprParser :: TokParser Expr
implicitParamExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  EVar . qualifyName Nothing . mkUnqualifiedName NameVarId <$> implicitParamNameParser

wildcardExprParser :: TokParser Expr
wildcardExprParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkKeywordUnderscore
  pure (EVar (qualifyName Nothing (mkUnqualifiedName NameVarId "_")))

-- | Parse Template Haskell quote brackets
thQuoteExprParser :: TokParser Expr
thQuoteExprParser =
  thExpQuoteParser
    <|> thTypedQuoteParser
    <|> thDeclQuoteParser
    <|> thTypeQuoteParser
    <|> thPatQuoteParser

thExpQuoteParser :: TokParser Expr
thExpQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHExpQuoteOpen TkTHExpQuoteClose exprParser ETHExpQuote

thTypedQuoteParser :: TokParser Expr
thTypedQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHTypedQuoteOpen TkTHTypedQuoteClose exprParser ETHTypedQuote

thDeclQuoteParser :: TokParser Expr
thDeclQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHDeclQuoteOpen TkTHExpQuoteClose (bracedSemiSep declParser <|> plainSemiSep declParser) ETHDeclQuote

thTypeQuoteParser :: TokParser Expr
thTypeQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHTypeQuoteOpen TkTHExpQuoteClose typeParser ETHTypeQuote

thPatQuoteParser :: TokParser Expr
thPatQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHPatQuoteOpen TkTHExpQuoteClose patternParser ETHPatQuote

thUntypedSpliceParser :: TokParser Expr
thUntypedSpliceParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkTHSplice
  ETHSplice <$> atomExprParser

thTypedSpliceParser :: TokParser Expr
thTypedSpliceParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkTHTypedSplice
  ETHTypedSplice <$> atomExprParser

thNameQuoteExprParser :: TokParser Expr
thNameQuoteExprParser = thValueNameQuoteParser <|> thTypeNameQuoteParser

thValueNameQuoteParser :: TokParser Expr
thValueNameQuoteParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkTHQuoteTick
  ETHNameQuote <$> atomExprParser

thTypeNameQuoteParser :: TokParser Expr
thTypeNameQuoteParser = withSpanAnn (EAnn . mkAnnotation) $ do
  expectedTok TkTHTypeQuoteTick
  ETHTypeNameQuote <$> typeAtomParser

quasiQuoteExprParser :: TokParser Expr
quasiQuoteExprParser =
  tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (EAnn (mkAnnotation (lexTokenSpan tok)) (EQuasiQuote quoter body))
      _ -> Nothing
