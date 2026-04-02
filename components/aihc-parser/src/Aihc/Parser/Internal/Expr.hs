{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aihc.Parser.Internal.Expr
  ( exprParser,
    equationRhsParser,
    simplePatternParser,
    appPatternParser,
    patternParser,
    typeParser,
    typeAppParser,
    typeAtomParser,
  )
where

import Aihc.Parser.Internal.Common
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Control.Monad (guard)
import Data.Char (isLower, isUpper)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

exprParser :: TokParser Expr
exprParser =
  label "expression" $ do
    core <- exprCoreParser
    mWhere <- MP.optional whereClauseParser
    pure $
      case mWhere of
        Just decls -> EWhereDecls (mergeSourceSpans (getSourceSpan core) (sourceSpanEnd decls)) core decls
        Nothing -> core

exprParserExcept :: [Text] -> TokParser Expr
exprParserExcept forbiddenInfix = do
  core <- exprCoreParserExcept forbiddenInfix
  mWhere <- MP.optional whereClauseParser
  pure $
    case mWhere of
      Just decls -> EWhereDecls (mergeSourceSpans (getSourceSpan core) (sourceSpanEnd decls)) core decls
      Nothing -> core

exprCoreParser :: TokParser Expr
exprCoreParser = exprCoreParserExcept []

exprCoreParserExcept :: [Text] -> TokParser Expr
exprCoreParserExcept forbiddenInfix = do
  tok <- lookAhead anySingle
  base <- case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordCase -> caseExprParser
    TkKeywordLet -> letExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept forbiddenInfix
  -- Optional type signature: expr :: type
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  pure $ case mTypeSig of
    Just ty -> ETypeSig (mergeSourceSpans (getSourceSpan base) (getSourceSpan ty)) base ty
    Nothing -> base

ifExprParser :: TokParser Expr
ifExprParser = MP.try multiWayIfExprParser <|> classicIfExprParser

classicIfExprParser :: TokParser Expr
classicIfExprParser = withSpan $ do
  keywordTok TkKeywordIf
  cond <- region "while parsing if condition" exprParser
  skipSemicolons
  keywordTok TkKeywordThen
  yes <- region "while parsing then branch" exprParser
  skipSemicolons
  keywordTok TkKeywordElse
  no <- region "while parsing else branch" exprParser
  pure (\span' -> EIf span' cond yes no)

multiWayIfExprParser :: TokParser Expr
multiWayIfExprParser = withSpan $ do
  keywordTok TkKeywordIf
  rhss <- bracedAlts <|> plainAlts
  pure (`EMultiWayIf` rhss)
  where
    plainAlts = plainSemiSep1 multiWayIfAlternative
    bracedAlts = bracedSemiSep multiWayIfAlternative

multiWayIfAlternative :: TokParser GuardedRhs
multiWayIfAlternative = withSpan $ do
  expectedTok TkReservedPipe
  guards <- guardQualifierParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedRightArrow
  body <- exprParser
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

doExprParser :: TokParser Expr
doExprParser = withSpan $ do
  keywordTok TkKeywordDo
  stmts <- bracedStmtListParser doStmtParser
  pure (`EDo` stmts)

bracedStmtListParser :: TokParser a -> TokParser [a]
bracedStmtListParser = bracedSemiSep1

doStmtParser :: TokParser DoStmt
doStmtParser = MP.try doBindStmtParser <|> MP.try doLetStmtParser <|> doExprStmtParser

doBindStmtParser :: TokParser DoStmt
doBindStmtParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' binding" exprParser
  pure (\span' -> DoBind span' pat expr)

parseLetDeclsParser :: TokParser [Decl]
parseLetDeclsParser = do
  keywordTok TkKeywordLet
  bracedDeclsParser <|> plainDeclsParser

parseLetDeclsStmtParser :: TokParser [Decl]
parseLetDeclsStmtParser = do
  decls <- parseLetDeclsParser
  MP.notFollowedBy (keywordTok TkKeywordIn)
  pure decls

doLetStmtParser :: TokParser DoStmt
doLetStmtParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (`DoLetDecls` decls)

doExprStmtParser :: TokParser DoStmt
doExprStmtParser = withSpan $ do
  expr <- exprParser
  pure (`DoExpr` expr)

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
-- This is used on both sides of infix operators per the Haskell Report grammar.
lexpParser :: TokParser Expr
lexpParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordCase -> caseExprParser
    TkKeywordLet -> letExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> appExprParser

buildInfix :: Expr -> (Text, Expr) -> Expr
buildInfix lhs (op, rhs) =
  EInfix (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs op rhs

infixOperatorParserExcept :: [Text] -> TokParser Text
infixOperatorParserExcept forbidden =
  symbolicOperatorParser <|> backtickIdentifierOperatorParser
  where
    symbolicOperatorParser =
      tokenSatisfy "infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op | op `notElem` forbidden -> Just op
          TkConSym op | op `notElem` forbidden -> Just op
          TkQVarSym op | op `notElem` forbidden -> Just op
          TkQConSym op | op `notElem` forbidden -> Just op
          -- TkMinusOperator is minus when LexicalNegation is enabled but used as infix
          TkMinusOperator | "-" `notElem` forbidden -> Just "-"
          -- Reserved operators that can be used as infix operators
          TkReservedColon | ":" `notElem` forbidden -> Just ":"
          _ -> Nothing

    backtickIdentifierOperatorParser = do
      expectedTok TkSpecialBacktick
      op <- identifierTextParser
      expectedTok TkSpecialBacktick
      if op `elem` forbidden then fail "forbidden infix operator" else pure op

intExprParser :: TokParser Expr
intExprParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (EInt, i, lexTokenText tok)
      TkIntegerHash i txt -> Just (EIntHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

intBaseExprParser :: TokParser Expr
intBaseExprParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (EIntBase, i, txt)
      TkIntegerBaseHash i txt -> Just (EIntBaseHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

floatExprParser :: TokParser Expr
floatExprParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (EFloat, x, txt)
      TkFloatHash x txt -> Just (EFloatHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

charExprParser :: TokParser Expr
charExprParser = withSpan $ do
  (ctor, c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (EChar, x, lexTokenText tok)
      TkCharHash x txt -> Just (ECharHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' c repr)

stringExprParser :: TokParser Expr
stringExprParser = withSpan $ do
  (ctor, s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (EString, x, lexTokenText tok)
      TkStringHash x txt -> Just (EStringHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' s repr)

appExprParser :: TokParser Expr
appExprParser = withSpan $ do
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
    applyArg span' fn (Left ty) = ETypeApp span' fn ty
    applyArg span' fn (Right arg) = EApp span' fn arg

-- | Parse an atom, optionally followed by one or more record construction/update syntax.
-- This handles cases like:
--   - Foo { x = 1 }  -- record construction
--   - expr { x = 1 } -- record update
--   - r { a = 1 } { b = 2 } -- chained record update
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
          let result = case e of
                EVar span' name
                  | isConLikeName name ->
                      ERecordCon (mergeSourceSpans span' (fieldsEndSpan fields)) name (map normalizeField fields) hasWildcard
                _ ->
                  ERecordUpd (mergeSourceSpans (getSourceSpan e) (fieldsEndSpan fields)) e (map normalizeField fields)
          -- Recursively check for more record braces (chained updates)
          applyRecordSuffixes result

    -- Get the end span from the last field (or the opening brace position)
    fieldsEndSpan :: [(Text, Maybe Expr, SourceSpan)] -> SourceSpan
    fieldsEndSpan [] = NoSourceSpan
    fieldsEndSpan fs = case last fs of (_, _, sp) -> sp
    -- Normalize field: if no expression given (pun), use field name as expression
    normalizeField :: (Text, Maybe Expr, SourceSpan) -> (Text, Expr)
    normalizeField (fieldName, mExpr, sp) =
      case mExpr of
        Just expr' -> (fieldName, expr')
        Nothing -> (fieldName, EVar sp fieldName) -- NamedFieldPuns: field name becomes variable

-- | Parse record braces: { field = value, field2 = value2, ... }
-- Supports both explicit assignment (field = value) and puns (field)
-- With RecordWildCards enabled, also supports ".." as a final wildcard field.
recordBracesParser :: TokParser ([(Text, Maybe Expr, SourceSpan)], Bool)
recordBracesParser =
  braces recordFieldListParser
  where
    recordFieldListParser = do
      rwcEnabled <- isExtensionEnabled RecordWildCards
      fields <- recordFieldBindingParser `MP.sepEndBy` expectedTok TkSpecialComma
      if rwcEnabled
        then do
          mDotDot <- MP.optional (expectedTok TkReservedDotDot)
          case mDotDot of
            Nothing -> pure (fields, False)
            Just _ -> do
              _ <- MP.optional (expectedTok TkSpecialComma)
              pure (fields, True)
        else pure (fields, False)

-- | Parse a single record field binding: either "field = expr" or just "field" (pun)
-- Accepts both unqualified (field) and qualified (Mod.field) field names.
recordFieldBindingParser :: TokParser (Text, Maybe Expr, SourceSpan)
recordFieldBindingParser = withSpan $ do
  fieldName <- tokenSatisfy "field name" $ \tok ->
    case lexTokenKind tok of
      TkVarId name -> Just name
      TkQVarId name -> Just name
      _ -> Nothing
  mAssign <- MP.optional (expectedTok TkReservedEquals *> exprParser)
  pure (fieldName,mAssign,)

atomExprParser :: TokParser Expr
atomExprParser = do
  blockArgsEnabled <- isExtensionEnabled BlockArguments
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  let thAny = thEnabled || thFullEnabled
  MP.try prefixNegateAtomExprParser
    <|> MP.try parenOperatorExprParser
    <|> lambdaExprParser
    <|> letExprParser
    <|> (if blockArgsEnabled then MP.try doExprParser else MP.empty)
    <|> (if blockArgsEnabled then MP.try caseExprParser else MP.empty)
    <|> (if blockArgsEnabled then MP.try ifExprParser else MP.empty)
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
    <|> varExprParser

prefixNegateAtomExprParser :: TokParser Expr
prefixNegateAtomExprParser = withSpan $ do
  prefixMinusTokenParser
  inner <- atomExprParser
  pure (`ENegate` inner)

negateExprParser :: TokParser Expr
negateExprParser = withSpan $ do
  _ <- minusTokenValueParser
  inner <- appExprParser
  pure (`ENegate` inner)

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
parenOperatorExprParser = withSpan $ do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym -> Just sym
      TkConSym sym -> Just sym
      TkQVarSym sym -> Just sym
      TkQConSym sym -> Just sym
      TkMinusOperator -> Just "-"
      TkReservedColon -> Just ":"
      TkReservedDoubleColon -> Just "::"
      TkReservedEquals -> Just "="
      TkReservedPipe -> Just "|"
      TkReservedLeftArrow -> Just "<-"
      TkReservedRightArrow -> Just "->"
      TkReservedDoubleArrow -> Just "=>"
      TkReservedDotDot -> Just ".."
      -- Note: ~ is now lexed as TkVarSym "~" so TkVarSym case handles it
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure (`EVar` op)

patternParser :: TokParser Pattern
patternParser = label "pattern" infixPatternParser

infixPatternParser :: TokParser Pattern
infixPatternParser = do
  lhs <- asOrAppPatternParser
  rest <- MP.many ((,) <$> conOperatorParser <*> asOrAppPatternParser)
  pure (foldl buildInfixPattern lhs rest)

-- | Parse either an as-pattern (name@atom) or an application pattern.
-- As-patterns bind tighter than infix but looser than application,
-- so they appear as operands of infix patterns.
asOrAppPatternParser :: TokParser Pattern
asOrAppPatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        expectedTok TkReservedAt
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> appPatternParser

buildInfixPattern :: Pattern -> (Text, Pattern) -> Pattern
buildInfixPattern lhs (op, rhs) =
  PInfix (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs op rhs

conOperatorParser :: TokParser Text
conOperatorParser =
  tokenSatisfy "constructor operator" $ \tok ->
    case lexTokenKind tok of
      TkConSym op -> Just op
      TkQConSym op -> Just op
      TkReservedColon -> Just ":"
      _ -> Nothing

appPatternParser :: TokParser Pattern
appPatternParser = do
  first <- patternAtomParser
  if isPatternAppHead first
    then do
      rest <- MP.many patternAtomParser
      pure (foldl buildPatternApp first rest)
    else pure first

buildPatternApp :: Pattern -> Pattern -> Pattern
buildPatternApp lhs rhs =
  case lhs of
    PCon lSpan name args -> PCon (mergeSourceSpans lSpan (getSourceSpan rhs)) name (args <> [rhs])
    PVar lSpan name
      | isConLikeName name -> PCon (mergeSourceSpans lSpan (getSourceSpan rhs)) name [rhs]
    _ -> lhs

patternAtomParser :: TokParser Pattern
patternAtomParser = do
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  MP.try strictPatternParser
    <|> MP.try irrefutablePatternParser
    <|> MP.try negativeLiteralPatternParser
    <|> quasiQuotePatternParser
    <|> (if thFullEnabled then thSplicePatternParser else MP.empty)
    <|> wildcardPatternParser
    <|> literalPatternParser
    <|> listPatternParser
    <|> parenOrTuplePatternParser
    <|> MP.try atomAsPatternParser
    <|> varOrConPatternParser
  where
    -- Parse an as-pattern as an atom: name@atom
    -- This allows as-patterns within constructor application patterns
    -- (e.g., Con x@(Con' y z)).
    atomAsPatternParser :: TokParser Pattern
    atomAsPatternParser = withSpan $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      inner <- patternAtomParser
      pure (\span' -> PAs span' name inner)

strictPatternParser :: TokParser Pattern
strictPatternParser = withSpan $ do
  expectedTok TkPrefixBang
  inner <- patternAtomParser
  pure (`PStrict` inner)

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpan $ do
  expectedTok TkPrefixTilde
  inner <- patternAtomParser
  pure (`PIrrefutable` inner)

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = withSpan $ do
  expectedTok (TkVarSym "-")
  lit <- literalParser
  pure (`PNegLit` lit)

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser = withSpan $ do
  keywordTok TkKeywordUnderscore
  pure PWildcard

literalPatternParser :: TokParser Pattern
literalPatternParser = withSpan $ do
  lit <- literalParser
  pure (`PLit` lit)

quasiQuotePatternParser :: TokParser Pattern
quasiQuotePatternParser = withSpan $ do
  (quoter, body) <- tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote q b -> Just (q, b)
      _ -> Nothing
  pure (\span' -> PQuasiQuote span' quoter body)

quasiQuoteExprParser :: TokParser Expr
quasiQuoteExprParser =
  tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (EQuasiQuote (lexTokenSpan tok) quoter body)
      _ -> Nothing

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

intLiteralParser :: TokParser Literal
intLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (LitInt, i, lexTokenText tok)
      TkIntegerHash i txt -> Just (LitIntHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (LitIntBase, i, txt)
      TkIntegerBaseHash i txt -> Just (LitIntBaseHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (LitFloat, x, txt)
      TkFloatHash x txt -> Just (LitFloatHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

charLiteralParser :: TokParser Literal
charLiteralParser = withSpan $ do
  (ctor, c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (LitChar, x, lexTokenText tok)
      TkCharHash x txt -> Just (LitCharHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' c repr)

stringLiteralParser :: TokParser Literal
stringLiteralParser = withSpan $ do
  (ctor, s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (LitString, x, lexTokenText tok)
      TkStringHash x txt -> Just (LitStringHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' s repr)

rhsParser :: TokParser Rhs
rhsParser = label "right-hand side" (rhsParserWithArrow RhsArrowCase)

equationRhsParser :: TokParser Rhs
equationRhsParser = label "equation right-hand side" (rhsParserWithArrow RhsArrowEquation)

-- | The kind of arrow used in RHS parsing
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
  pure (`UnguardedRhs` body)

rhsContextText :: RhsArrowKind -> Text
rhsContextText RhsArrowCase = "while parsing case alternative right-hand side"
rhsContextText RhsArrowEquation = "while parsing equation right-hand side"

guardedRhssParser :: RhsArrowKind -> TokParser Rhs
guardedRhssParser arrowKind = withSpan $ do
  grhss <- MP.some (guardedRhsParser arrowKind)
  pure (`GuardedRhss` grhss)

guardedRhsParser :: RhsArrowKind -> TokParser GuardedRhs
guardedRhsParser arrowKind = withSpan $ do
  expectedTok TkReservedPipe
  guards <- guardQualifierParser `MP.sepBy1` expectedTok TkSpecialComma
  rhsArrowTok arrowKind
  body <- exprParserExcept ["|", rhsArrowText arrowKind]
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

guardQualifierParser :: TokParser GuardQualifier
guardQualifierParser = MP.try guardPatParser <|> MP.try guardLetParser <|> guardExprParser
  where
    guardPatParser = withSpan $ do
      pat <- patternParser
      expectedTok TkReservedLeftArrow
      expr <- exprParser
      pure (\span' -> GuardPat span' pat expr)

    guardLetParser = withSpan $ do
      decls <- parseLetDeclsStmtParser
      pure (`GuardLet` decls)

    guardExprParser = withSpan $ do
      expr <- exprParser
      pure (`GuardExpr` expr)

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
caseExprParser = withSpan $ do
  keywordTok TkKeywordCase
  scrutinee <- region "while parsing case expression" exprParser
  keywordTok TkKeywordOf
  alts <- bracedAlts <|> plainAlts
  pure $ \span' -> ECase span' scrutinee alts
  where
    plainAlts = plainSemiSep1 caseAltParser
    bracedAlts = bracedSemiSep caseAltParser

parenExprParser :: TokParser Expr
parenExprParser = withSpan $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mClosed <- MP.optional (expectedTok closeTok)
  case mClosed of
    Just () -> pure (\span' -> ETuple span' tupleFlavor [])
    Nothing ->
      if tupleFlavor == Boxed
        then MP.try (parseNegateParen closeTok) <|> MP.try (parseSection closeTok) <|> MP.try (parseTupleSectionExpr tupleFlavor closeTok) <|> parseParenOrTupleExpr tupleFlavor closeTok
        else MP.try (parseTupleSectionExpr tupleFlavor closeTok) <|> MP.try (parseUnboxedSumExprLeadingBars closeTok) <|> parseParenOrTupleExpr tupleFlavor closeTok
  where
    parseNegateParen closeTok = do
      minusTok <- minusTokenValueParser
      nextTok <- lookAhead anySingle
      guard (parenNegateAllowed minusTok nextTok)
      inner <- exprParser
      expectedTok closeTok
      pure $ \span' ->
        case lexTokenKind minusTok of
          TkPrefixMinus -> ENegate span' inner
          _ -> EParen span' (ENegate span' inner)

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

    parseSection closeTok = do
      MP.try parseSectionR <|> parseSectionL
      where
        parseSectionR = do
          op <- infixOperatorParserExcept []
          rhs <- exprParser
          expectedTok closeTok
          pure (\span' -> EParen span' (ESectionR span' op rhs))

        parseSectionL = do
          lhs <- appExprParser
          op <- infixOperatorParserExcept []
          expectedTok closeTok
          pure (\span' -> EParen span' (ESectionL span' lhs op))

    parseTupleSectionExpr tupleFlavor closeTok = do
      -- Try to parse as tuple section first (e.g., "(,1)" or "(1,)")
      -- If that fails, fall back to regular tuple/paren parsing
      values <- parseTupleSection closeTok
      pure (\span' -> ETupleSection span' tupleFlavor values)

    parseParenOrTupleExpr tupleFlavor closeTok = do
      first <- exprParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          -- Check for pipe (unboxed sum: value in first slot)
          mPipe <- if tupleFlavor == Unboxed then MP.optional (expectedTok TkReservedPipe) else pure Nothing
          case mPipe of
            Just () -> do
              -- (# expr | ... #) - value in first slot of sum
              trailingBars <- MP.many (expectedTok TkReservedPipe)
              expectedTok closeTok
              let arity = 2 + length trailingBars
              pure (\span' -> EUnboxedSum span' 0 arity first)
            Nothing -> do
              expectedTok closeTok
              if tupleFlavor == Boxed
                then pure (`EParen` first)
                else fail "not an unboxed tuple"
        Just () -> do
          second <- exprParser
          more <- MP.many (expectedTok TkSpecialComma *> exprParser)
          expectedTok closeTok
          pure (\span' -> ETuple span' tupleFlavor (first : second : more))

    parseUnboxedSumExprLeadingBars closeTok = do
      -- Parse (# | | ... | expr | ... | #) where value is not in first slot
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      inner <- exprParser
      trailingBars <- MP.many (expectedTok TkReservedPipe)
      expectedTok closeTok
      let arity = altIdx + 1 + length trailingBars
      pure (\span' -> EUnboxedSum span' altIdx arity inner)

parseTupleSection :: LexTokenKind -> TokParser [Maybe Expr]
parseTupleSection closeTok = do
  first <- MP.optional exprParser
  _ <- expectedTok TkSpecialComma
  middle <- MP.many (MP.try (MP.optional exprParser <* expectedTok TkSpecialComma))
  lastSlot <- MP.optional exprParser
  expectedTok closeTok
  let vals = first : middle <> [lastSlot]
  let hasMissing = any isNothing vals
  if hasMissing && length vals > 1
    then pure vals
    else fail "not a tuple section"

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

listExprParser :: TokParser Expr
listExprParser = withSpan $ do
  expectedTok TkSpecialLBracket
  mClose <- MP.optional (expectedTok TkSpecialRBracket)
  case mClose of
    Just () -> pure (`EList` [])
    Nothing -> do
      first <- exprParser
      parseListTail first

parseListTail :: Expr -> TokParser (SourceSpan -> Expr)
parseListTail first =
  MP.try listCompTailParser
    <|> MP.try arithFromToTailParser
    <|> MP.try commaTailParser
    <|> singletonTailParser
  where
    -- Parse list comprehension qualifiers, which can be:
    -- - Regular: [ expr | qual1, qual2, qual3 ]
    -- - Parallel (with ParallelListComp): [ expr | qual1, qual2 | qual3, qual4 ]
    listCompTailParser = do
      expectedTok TkReservedPipe
      firstGroup <- compStmtParser `MP.sepBy1` expectedTok TkSpecialComma
      -- Try to parse additional parallel groups separated by |
      moreGroups <- MP.many (expectedTok TkReservedPipe *> (compStmtParser `MP.sepBy1` expectedTok TkSpecialComma))
      expectedTok TkSpecialRBracket
      pure $ \span' ->
        case moreGroups of
          [] -> EListComp span' first firstGroup
          _ -> EListCompParallel span' first (firstGroup : moreGroups)

    arithFromToTailParser = do
      expectedTok TkReservedDotDot
      mTo <- MP.optional exprParser
      expectedTok TkSpecialRBracket
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFrom span' first
            Just toExpr -> ArithSeqFromTo span' first toExpr

    commaTailParser = do
      expectedTok TkSpecialComma
      second <- exprParser
      MP.try (arithFromThenTailParser second) <|> listTailParser second

    arithFromThenTailParser second = do
      expectedTok TkReservedDotDot
      mTo <- MP.optional exprParser
      expectedTok TkSpecialRBracket
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFromThen span' first second
            Just toExpr -> ArithSeqFromThenTo span' first second toExpr

    listTailParser second = do
      rest <- MP.many (expectedTok TkSpecialComma *> exprParser)
      expectedTok TkSpecialRBracket
      pure (\span' -> EList span' (first : second : rest))

    singletonTailParser = do
      expectedTok TkSpecialRBracket
      pure (\span' -> EList span' [first])

compStmtParser :: TokParser CompStmt
compStmtParser = MP.try compGenStmtParser <|> MP.try compLetStmtParser <|> compGuardStmtParser

compGenStmtParser :: TokParser CompStmt
compGenStmtParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- region "while parsing '<-' generator" exprParser
  pure (\span' -> CompGen span' pat expr)

compLetStmtParser :: TokParser CompStmt
compLetStmtParser = withSpan $ do
  decls <- parseLetDeclsStmtParser
  pure (`CompLetDecls` decls)

lambdaExprParser :: TokParser Expr
lambdaExprParser = withSpan $ do
  expectedTok TkReservedBackslash
  lambdaCaseParser <|> lambdaPatsParser
  where
    lambdaCaseParser = do
      keywordTok TkKeywordCase
      alts <- bracedAlts
      pure (`ELambdaCase` alts)

    lambdaPatsParser = do
      pats <- MP.some patternParser
      expectedTok TkReservedRightArrow
      body <- region "while parsing lambda body" exprParser
      pure (\span' -> ELambdaPats span' pats body)

    bracedAlts = bracedSemiSep1 caseAltParser

letExprParser :: TokParser Expr
letExprParser = withSpan $ do
  decls <- parseLetDeclsParser
  keywordTok TkKeywordIn
  body <- exprParser
  pure (\span' -> ELetDecls span' decls body)

whereClauseParser :: TokParser [Decl]
whereClauseParser = do
  keywordTok TkKeywordWhere
  bracedDeclsParser <|> plainDeclsParser

plainDeclsParser :: TokParser [Decl]
plainDeclsParser = plainSemiSep1 localDeclParser

bracedDeclsParser :: TokParser [Decl]
bracedDeclsParser = bracedSemiSep1 localDeclParser

localDeclParser :: TokParser Decl
localDeclParser = MP.try localTypeSigDeclParser <|> MP.try localFunctionDeclParser <|> localPatternDeclParser

localTypeSigDeclParser :: TokParser Decl
localTypeSigDeclParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

localFunctionDeclParser :: TokParser Decl
localFunctionDeclParser = withSpan $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  rhs <- equationRhsParser
  pure (\span' -> functionBindDecl span' headForm name pats rhs)

localPatternDeclParser :: TokParser Decl
localPatternDeclParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  pure (\span' -> DeclValue span' (PatternBind span' pat (UnguardedRhs span' rhsExpr)))

varOrConPatternParser :: TokParser Pattern
varOrConPatternParser = MP.try recordPatternParser <|> bareVarOrConPatternParser

bareVarOrConPatternParser :: TokParser Pattern
bareVarOrConPatternParser = withSpan $ do
  name <- identifierTextParser
  pure $ \span' ->
    if isConLikeName name
      then PCon span' name []
      else PVar span' name

recordPatternParser :: TokParser Pattern
recordPatternParser = withSpan $ do
  con <- constructorIdentifierParser
  (fields, hasWildcard) <- braces recordPatternFieldListParser
  pure (\span' -> PRecord span' con fields hasWildcard)

recordFieldPatternParser :: TokParser (Text, Pattern)
recordFieldPatternParser = withSpan $ do
  field <- identifierTextParser
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Just () -> do
      pat <- patternParser
      pure $ const (field, pat)
    Nothing -> do
      -- NamedFieldPuns: just "field" means "field = field"
      pure $ \srcSpan -> (field, PVar srcSpan field)

-- | Parse the contents of record pattern braces, supporting RecordWildCards ".."
recordPatternFieldListParser :: TokParser ([(Text, Pattern)], Bool)
recordPatternFieldListParser = do
  rwcEnabled <- isExtensionEnabled RecordWildCards
  fields <- recordFieldPatternParser `MP.sepEndBy` expectedTok TkSpecialComma
  if rwcEnabled
    then do
      mDotDot <- MP.optional (expectedTok TkReservedDotDot)
      case mDotDot of
        Nothing -> pure (fields, False)
        Just _ -> do
          _ <- MP.optional (expectedTok TkSpecialComma)
          pure (fields, True)
    else pure (fields, False)

listPatternParser :: TokParser Pattern
listPatternParser = withSpan $ do
  expectedTok TkSpecialLBracket
  elems <- patternParser `MP.sepBy` expectedTok TkSpecialComma
  expectedTok TkSpecialRBracket
  pure (`PList` elems)

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpan $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  MP.try (unitPatternParser tupleFlavor closeTok)
    <|> MP.try (viewPatternParser tupleFlavor closeTok)
    <|> (if tupleFlavor == Unboxed then MP.try (parseUnboxedSumPatLeadingBars closeTok) else MP.empty)
    <|> tupleOrParenPatternParser tupleFlavor closeTok
  where
    unitPatternParser tupleFlavor closeTok = do
      expectedTok closeTok
      pure (\span' -> PTuple span' tupleFlavor [])

    viewPatternParser tupleFlavor closeTok = do
      viewExpr <- exprParser
      expectedTok TkReservedRightArrow
      inner <- patternParser
      expectedTok closeTok
      if tupleFlavor == Boxed
        then pure (\span' -> PView span' viewExpr inner)
        else fail "not an unboxed tuple pattern"

    tupleOrParenPatternParser tupleFlavor closeTok = do
      first <- patternParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          -- Check for pattern type signature: (pat :: type)
          mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
          case mTypeSig of
            Just ty -> do
              expectedTok closeTok
              if tupleFlavor == Boxed
                then pure (\span' -> PParen span' (PTypeSig span' first ty))
                else fail "not an unboxed tuple pattern"
            Nothing -> do
              -- Check for pipe (unboxed sum: pattern in first slot)
              mPipe <- if tupleFlavor == Unboxed then MP.optional (expectedTok TkReservedPipe) else pure Nothing
              case mPipe of
                Just () -> do
                  -- (# pat | ... #) - pattern in first slot of sum
                  trailingBars <- MP.many (expectedTok TkReservedPipe)
                  expectedTok closeTok
                  let arity = 2 + length trailingBars
                  pure (\span' -> PUnboxedSum span' 0 arity first)
                Nothing -> do
                  expectedTok closeTok
                  if tupleFlavor == Boxed
                    then pure (`PParen` first)
                    else fail "not an unboxed tuple pattern"
        Just () -> do
          second <- patternParser
          more <- MP.many (expectedTok TkSpecialComma *> patternParser)
          expectedTok closeTok
          pure (\span' -> PTuple span' tupleFlavor (first : second : more))

    parseUnboxedSumPatLeadingBars closeTok = do
      -- Parse (# | | ... | pat | ... | #) where pattern is not in first slot
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      inner <- patternParser
      trailingBars <- MP.many (expectedTok TkReservedPipe)
      expectedTok closeTok
      let arity = altIdx + 1 + length trailingBars
      pure (\span' -> PUnboxedSum span' altIdx arity inner)

isConLikeName :: Text -> Bool
isConLikeName name =
  case T.uncons name of
    Just (c, _) -> isUpper c
    Nothing -> False

isPatternAppHead :: Pattern -> Bool
isPatternAppHead pat =
  case pat of
    PCon {} -> True
    PVar _ name -> isConLikeName name
    _ -> False

compGuardStmtParser :: TokParser CompStmt
compGuardStmtParser = withSpan $ do
  expr <- exprParser
  pure (`CompGuard` expr)

varExprParser :: TokParser Expr
varExprParser = withSpan $ do
  name <- identifierTextParser
  pure (`EVar` name)

-- | Parse Template Haskell quote brackets:
-- [| expr |], [e| expr |], [|| expr ||], [e|| expr ||],
-- [d| decls |], [t| type |], [p| pat |]
thQuoteExprParser :: TokParser Expr
thQuoteExprParser =
  thExpQuoteParser
    <|> thTypedQuoteParser
    <|> thDeclQuoteParser
    <|> thTypeQuoteParser
    <|> thPatQuoteParser

thExpQuoteParser :: TokParser Expr
thExpQuoteParser = withSpan $ do
  expectedTok TkTHExpQuoteOpen
  body <- exprParser
  expectedTok TkTHExpQuoteClose
  pure (`ETHExpQuote` body)

thTypedQuoteParser :: TokParser Expr
thTypedQuoteParser = withSpan $ do
  expectedTok TkTHTypedQuoteOpen
  body <- exprParser
  expectedTok TkTHTypedQuoteClose
  pure (`ETHTypedQuote` body)

thDeclQuoteParser :: TokParser Expr
thDeclQuoteParser = withSpan $ do
  expectedTok TkTHDeclQuoteOpen
  decls <- bracedSemiSep1 localDeclParser <|> plainSemiSep1 localDeclParser
  expectedTok TkTHExpQuoteClose
  pure (`ETHDeclQuote` decls)

thTypeQuoteParser :: TokParser Expr
thTypeQuoteParser = withSpan $ do
  expectedTok TkTHTypeQuoteOpen
  ty <- typeParser
  expectedTok TkTHExpQuoteClose
  pure (`ETHTypeQuote` ty)

thPatQuoteParser :: TokParser Expr
thPatQuoteParser = withSpan $ do
  expectedTok TkTHPatQuoteOpen
  pat <- patternParser
  expectedTok TkTHExpQuoteClose
  pure (`ETHPatQuote` pat)

-- | Parse Template Haskell splice expressions: $expr, $(expr), $$expr, $$(expr)
thSpliceExprParser :: TokParser Expr
thSpliceExprParser =
  MP.try thTypedSpliceParser <|> thUntypedSpliceParser

-- | Parse untyped TH splice: $name or $(expr)
thUntypedSpliceParser :: TokParser Expr
thUntypedSpliceParser = withSpan $ do
  expectedTok TkTHSplice
  body <- thSpliceBody
  pure (`ETHSplice` body)

-- | Parse typed TH splice: $$name or $$(expr)
thTypedSpliceParser :: TokParser Expr
thTypedSpliceParser = withSpan $ do
  expectedTok TkTHTypedSplice
  body <- thSpliceBody
  pure (`ETHTypedSplice` body)

-- | Parse the body of a splice: either a parenthesized expression or a bare identifier
thSpliceBody :: TokParser Expr
thSpliceBody =
  parenSpliceBody <|> bareSpliceBody
  where
    parenSpliceBody = withSpan $ do
      body <- parens exprParser
      pure (`EParen` body)
    bareSpliceBody = withSpan $ do
      name <- identifierTextParser
      pure (`EVar` name)

-- | Parse Template Haskell name quotes: 'name and ''Type
thNameQuoteExprParser :: TokParser Expr
thNameQuoteExprParser =
  MP.try thValueNameQuoteParser <|> thTypeNameQuoteParser

thValueNameQuoteParser :: TokParser Expr
thValueNameQuoteParser = withSpan $ do
  expectedTok TkTHQuoteTick
  name <- identifierTextParser <|> parenOperatorNameParser
  pure (`ETHNameQuote` name)

thTypeNameQuoteParser :: TokParser Expr
thTypeNameQuoteParser = withSpan $ do
  expectedTok TkTHTypeQuoteTick
  name <- identifierTextParser
  pure (`ETHTypeNameQuote` name)

-- | Parse a parenthesized operator name: (+), (++), (:)
parenOperatorNameParser :: TokParser Text
parenOperatorNameParser = do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym -> Just sym
      TkConSym sym -> Just sym
      TkReservedColon -> Just ":"
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure ("(" <> op <> ")")

-- | Parse Template Haskell pattern splice: $pat or $(pat)
thSplicePatternParser :: TokParser Pattern
thSplicePatternParser = withSpan $ do
  expectedTok TkTHSplice
  body <- thSpliceBody
  pure (`PSplice` body)

-- | Parse Template Haskell type splice: $typ or $(typ)
thSpliceTypeParser :: TokParser Type
thSpliceTypeParser = withSpan $ do
  expectedTok TkTHSplice
  body <- thSpliceBody
  pure (`TSplice` body)

simplePatternParser :: TokParser Pattern
simplePatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        expectedTok TkReservedAt
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> patternAtomParser

typeParser :: TokParser Type
typeParser =
  label "type" (MP.try forallTypeParser <|> MP.try contextTypeParser <|> typeFunParser)

forallTypeParser :: TokParser Type
forallTypeParser = withSpan $ do
  varIdTok "forall"
  binders <- MP.some identifierTextParser
  expectedTok (TkVarSym ".")
  inner <- MP.try contextTypeParser <|> typeFunParser
  pure (\span' -> TForall span' binders inner)

contextTypeParser :: TokParser Type
contextTypeParser = do
  constraints <- constraintsParser
  expectedTok TkReservedDoubleArrow
  inner <- typeParser
  pure (TContext (mergeSourceSpans (constraintHeadSpan constraints) (getSourceSpan inner)) constraints inner)

constraintHeadSpan :: [Constraint] -> SourceSpan
constraintHeadSpan constraints =
  case constraints of
    [] -> NoSourceSpan
    constraint : _ -> getSourceSpan constraint

constraintsParser :: TokParser [Constraint]
constraintsParser = constraintsParserWith typeAtomParser

typeFunParser :: TokParser Type
typeFunParser = do
  lhs <- typeInfixParser
  mRhs <- MP.optional (expectedTok TkReservedRightArrow *> typeParser)
  pure $
    case mRhs of
      Just rhs -> TFun (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs rhs
      Nothing -> lhs

typeInfixParser :: TokParser Type
typeInfixParser = do
  lhs <- typeAppParser
  rest <- MP.many ((,) <$> typeInfixOperatorParser <*> typeAppParser)
  pure (foldl buildInfixType lhs rest)

buildInfixType :: Type -> ((Text, TypePromotion), Type) -> Type
buildInfixType lhs ((op, promoted), rhs) =
  let span' = mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)
      opType = TCon span' op promoted
   in TApp span' (TApp span' opType lhs) rhs

typeInfixOperatorParser :: TokParser (Text, TypePromotion)
typeInfixOperatorParser =
  promotedInfixOperatorParser <|> unpromotedInfixOperatorParser
  where
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!"
                && op /= "-" ->
                Just (op, Unpromoted)
          TkConSym op -> Just (op, Unpromoted)
          TkQVarSym op -> Just (op, Unpromoted)
          TkQConSym op -> Just (op, Unpromoted)
          _ -> Nothing

    promotedInfixOperatorParser = MP.try $ do
      expectedTok (TkVarSym "'")
      expectedTok TkReservedColon
      pure (":", Promoted)

typeAppParser :: TokParser Type
typeAppParser = do
  first <- typeAtomParser
  rest <- MP.many typeAtomParser
  pure (foldl buildTypeApp first rest)

buildTypeApp :: Type -> Type -> Type
buildTypeApp lhs rhs =
  TApp (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs rhs

typeAtomParser :: TokParser Type
typeAtomParser = do
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  MP.try promotedTypeParser
    <|> typeLiteralTypeParser
    <|> typeQuasiQuoteParser
    <|> (if thFullEnabled then thSpliceTypeParser else MP.empty)
    <|> typeListParser
    <|> MP.try typeParenOperatorParser
    <|> typeParenOrTupleParser
    <|> typeStarParser
    <|> typeWildcardParser
    <|> typeIdentifierParser

typeWildcardParser :: TokParser Type
typeWildcardParser = withSpan $ do
  expectedTok TkKeywordUnderscore
  pure TWildcard

typeLiteralTypeParser :: TokParser Type
typeLiteralTypeParser = withSpan $ do
  lit <- tokenSatisfy "type literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger n -> Just (TypeLitInteger n (lexTokenText tok))
      TkIntegerBase n _ -> Just (TypeLitInteger n (lexTokenText tok))
      TkString s -> Just (TypeLitSymbol s (lexTokenText tok))
      TkChar c -> Just (TypeLitChar c (lexTokenText tok))
      _ -> Nothing
  pure (`TTypeLit` lit)

promotedTypeParser :: TokParser Type
promotedTypeParser = withSpan $ do
  expectedTok (TkVarSym "'")
  promotedTy <- MP.try promotedStructuredTypeParser <|> promotedRawTypeParser
  pure (`setTypeSpan` promotedTy)

promotedStructuredTypeParser :: TokParser Type
promotedStructuredTypeParser = do
  ty <-
    MP.try typeListParser
      <|> MP.try typeParenOrTupleParser
      <|> MP.try typeParenOperatorParser
      <|> typeIdentifierParser
  maybe (fail "promoted type") pure (markTypePromoted ty)

promotedRawTypeParser :: TokParser Type
promotedRawTypeParser = withSpan $ do
  suffix <- promotedBracketedSuffixParser <|> promotedParenthesizedSuffixParser
  pure (\span' -> TCon span' suffix Promoted)

promotedBracketedSuffixParser :: TokParser Text
promotedBracketedSuffixParser = collectDelimitedRaw TkSpecialLBracket TkSpecialRBracket

promotedParenthesizedSuffixParser :: TokParser Text
promotedParenthesizedSuffixParser = collectDelimitedRaw TkSpecialLParen TkSpecialRParen

collectDelimitedRaw :: LexTokenKind -> LexTokenKind -> TokParser Text
collectDelimitedRaw openKind closeKind = do
  openTxt <- tokenSatisfy ("opening delimiter " <> show openKind) $ \tok ->
    if lexTokenKind tok == openKind then Just (lexTokenText tok) else Nothing
  go 1 openTxt
  where
    go :: Int -> Text -> TokParser Text
    go depth acc = do
      tok <- anySingle
      let kind = lexTokenKind tok
          txt = lexTokenText tok
          acc' = acc <> txt
      case () of
        _
          | kind == openKind -> go (depth + 1) acc'
          | kind == closeKind ->
              if depth == 1
                then pure acc'
                else go (depth - 1) acc'
          | otherwise -> go depth acc'

typeParenOperatorParser :: TokParser Type
typeParenOperatorParser = withSpan $ do
  expectedTok TkSpecialLParen
  op <- tokenSatisfy "type operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym | sym /= "*" -> Just sym
      TkConSym sym | sym /= "*" -> Just sym
      TkQVarSym sym -> Just sym
      TkQConSym sym -> Just sym
      -- Handle reserved operators that can be used as type constructors
      TkReservedRightArrow -> Just "->"
      -- Note: ~ is now lexed as TkVarSym "~" so TkVarSym case handles it
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure (\span' -> TCon span' op Unpromoted)

typeQuasiQuoteParser :: TokParser Type
typeQuasiQuoteParser =
  tokenSatisfy "type quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (TQuasiQuote (lexTokenSpan tok) quoter body)
      _ -> Nothing

typeIdentifierParser :: TokParser Type
typeIdentifierParser = withSpan $ do
  name <- identifierTextParser
  pure $ \span' ->
    case T.uncons name of
      Just (c, _) | isLower c || c == '_' -> TVar span' name
      _ -> TCon span' name Unpromoted

typeStarParser :: TokParser Type
typeStarParser = withSpan $ do
  expectedTok (TkVarSym "*")
  pure TStar

typeListParser :: TokParser Type
typeListParser = withSpan $ do
  expectedTok TkSpecialLBracket
  inner <- typeParser
  expectedTok TkSpecialRBracket
  pure (\span' -> TList span' Unpromoted inner)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpan $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mClosed <- MP.optional (expectedTok closeTok)
  case mClosed of
    Just () -> pure (\span' -> TTuple span' tupleFlavor Unpromoted [])
    Nothing -> do
      MP.try (tupleConstructorParser tupleFlavor closeTok) <|> parenthesizedTypeOrTupleParser tupleFlavor closeTok
  where
    tupleConstructorParser tupleFlavor closeTok = do
      _ <- expectedTok TkSpecialComma
      moreCommas <- MP.many (expectedTok TkSpecialComma)
      expectedTok closeTok
      let arity = 2 + length moreCommas
          tupleConName =
            case tupleFlavor of
              Boxed -> "(" <> T.replicate (arity - 1) "," <> ")"
              Unboxed -> "(#" <> T.replicate (arity - 1) "," <> "#)"
      pure (\span' -> TCon span' tupleConName Unpromoted)

    parenthesizedTypeOrTupleParser tupleFlavor closeTok = do
      first <- typeParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          -- Check for pipe (unboxed sum type)
          mPipe <- if tupleFlavor == Unboxed then MP.optional (expectedTok TkReservedPipe) else pure Nothing
          case mPipe of
            Just () -> do
              -- (# Type1 | Type2 | ... #) - unboxed sum type
              rest <- typeParser `MP.sepBy1` expectedTok TkReservedPipe
              expectedTok closeTok
              pure (\span' -> TUnboxedSum span' (first : rest))
            Nothing -> do
              expectedTok closeTok
              if tupleFlavor == Boxed
                then pure (`TParen` first)
                else fail "not an unboxed tuple type"
        Just () -> do
          second <- typeParser
          more <- MP.many (expectedTok TkSpecialComma *> typeParser)
          expectedTok closeTok
          pure (\span' -> TTuple span' tupleFlavor Unpromoted (first : second : more))

markTypePromoted :: Type -> Maybe Type
markTypePromoted ty =
  case ty of
    TCon span' name _ -> Just (TCon span' name Promoted)
    TList span' _ inner -> Just (TList span' Promoted inner)
    TTuple span' tupleFlavor _ elems -> Just (TTuple span' tupleFlavor Promoted elems)
    _ -> Nothing

setTypeSpan :: SourceSpan -> Type -> Type
setTypeSpan span' ty =
  case ty of
    TVar _ name -> TVar span' name
    TCon _ name promoted -> TCon span' name promoted
    TTypeLit _ lit -> TTypeLit span' lit
    TStar _ -> TStar span'
    TQuasiQuote _ quoter body -> TQuasiQuote span' quoter body
    TForall _ binders inner -> TForall span' binders inner
    TApp _ lhs rhs -> TApp span' lhs rhs
    TFun _ lhs rhs -> TFun span' lhs rhs
    TTuple _ tupleFlavor promoted elems -> TTuple span' tupleFlavor promoted elems
    TUnboxedSum _ elems -> TUnboxedSum span' elems
    TList _ promoted inner -> TList span' promoted inner
    TParen _ inner -> TParen span' inner
    TContext _ constraints inner -> TContext span' constraints inner
    TSplice _ body -> TSplice span' body
    TWildcard _ -> TWildcard span'
