{-# LANGUAGE OverloadedStrings #-}

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

import Aihc.Lexer (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Ast
import Aihc.Parser.Internal.Common
import Control.Monad (guard)
import Data.Char (isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP

exprParser :: TokParser Expr
exprParser = do
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
ifExprParser = withSpan $ do
  keywordTok TkKeywordIf
  cond <- exprParser
  skipSemicolons
  keywordTok TkKeywordThen
  yes <- exprParser
  skipSemicolons
  keywordTok TkKeywordElse
  no <- exprParser
  pure (\span' -> EIf span' cond yes no)

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
  expr <- exprParser
  pure (\span' -> DoBind span' pat expr)

doLetStmtParser :: TokParser DoStmt
doLetStmtParser = withSpan $ do
  keywordTok TkKeywordLet
  decls <- bracedDeclsParser <|> plainDeclsParser
  MP.notFollowedBy (keywordTok TkKeywordIn)
  pure (`DoLetDecls` decls)

doExprStmtParser :: TokParser DoStmt
doExprStmtParser = withSpan $ do
  expr <- exprParser
  pure (`DoExpr` expr)

infixExprParserExcept :: [Text] -> TokParser Expr
infixExprParserExcept forbidden = do
  lhs <- MP.try negateExprParser <|> lexpParser
  rest <- MP.many ((,) <$> infixOperatorParserExcept forbidden <*> lexpParser)
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
  (n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (i, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> EInt span' n repr)

intBaseExprParser :: TokParser Expr
intBaseExprParser = withSpan $ do
  (n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> EIntBase span' n repr)

floatExprParser :: TokParser Expr
floatExprParser = withSpan $ do
  (n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (x, txt)
      _ -> Nothing
  pure (\span' -> EFloat span' n repr)

charExprParser :: TokParser Expr
charExprParser = withSpan $ do
  (c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (x, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> EChar span' c repr)

stringExprParser :: TokParser Expr
stringExprParser = withSpan $ do
  (s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (x, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> EString span' s repr)

appExprParser :: TokParser Expr
appExprParser = withSpan $ do
  first <- atomOrRecordExprParser
  rest <- MP.many atomOrRecordExprParser
  pure $ \span' ->
    foldl (EApp span') first rest

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
        Just fields -> do
          let result = case e of
                EVar span' name
                  | isConLikeName name ->
                      ERecordCon (mergeSourceSpans span' (fieldsEndSpan fields)) name (map normalizeField fields)
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
recordBracesParser :: TokParser [(Text, Maybe Expr, SourceSpan)]
recordBracesParser = do
  expectedTok TkSpecialLBrace
  mClose <- MP.optional (expectedTok TkSpecialRBrace)
  case mClose of
    Just () -> pure []
    Nothing -> do
      fields <- recordFieldBindingParser `MP.sepEndBy` expectedTok TkSpecialComma
      expectedTok TkSpecialRBrace
      pure fields

-- | Parse a single record field binding: either "field = expr" or just "field" (pun)
recordFieldBindingParser :: TokParser (Text, Maybe Expr, SourceSpan)
recordFieldBindingParser = do
  startPos <- MP.getSourcePos
  fieldName <- tokenSatisfy "field name" $ \tok ->
    case lexTokenKind tok of
      TkVarId name -> Just name
      _ -> Nothing
  mAssign <- MP.optional (expectedTok TkReservedEquals *> exprParser)
  endPos <- MP.getSourcePos
  pure (fieldName, mAssign, sourceSpanFromPositions startPos endPos)

atomExprParser :: TokParser Expr
atomExprParser =
  MP.try prefixNegateAtomExprParser
    <|> MP.try parenOperatorExprParser
    <|> lambdaExprParser
    <|> letExprParser
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
patternParser = asPatternParser

asPatternParser :: TokParser Pattern
asPatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        expectedTok TkReservedAt
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> infixPatternParser

infixPatternParser :: TokParser Pattern
infixPatternParser = do
  lhs <- appPatternParser
  rest <- MP.many ((,) <$> conOperatorParser <*> appPatternParser)
  pure (foldl buildInfixPattern lhs rest)

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
      TkReservedDoubleColon -> Just "::"
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
patternAtomParser =
  MP.try strictPatternParser
    <|> MP.try irrefutablePatternParser
    <|> MP.try negativeLiteralPatternParser
    <|> quasiQuotePatternParser
    <|> wildcardPatternParser
    <|> literalPatternParser
    <|> listPatternParser
    <|> parenOrTuplePatternParser
    <|> varOrConPatternParser

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

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

intLiteralParser :: TokParser Literal
intLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (i, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> LitInt span' n repr)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> LitIntBase span' n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (x, txt)
      _ -> Nothing
  pure (\span' -> LitFloat span' n repr)

charLiteralParser :: TokParser Literal
charLiteralParser = withSpan $ do
  (c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (x, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> LitChar span' c repr)

stringLiteralParser :: TokParser Literal
stringLiteralParser = withSpan $ do
  (s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (x, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> LitString span' s repr)

rhsParser :: TokParser Rhs
rhsParser = rhsParserWithArrow RhsArrowCase

equationRhsParser :: TokParser Rhs
equationRhsParser = rhsParserWithArrow RhsArrowEquation

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
  body <- exprParser
  pure (`UnguardedRhs` body)

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
      keywordTok TkKeywordLet
      decls <- bracedDeclsParser <|> plainDeclsParser
      MP.notFollowedBy (keywordTok TkKeywordIn)
      pure (`GuardLet` decls)

    guardExprParser = withSpan $ do
      expr <- exprParser
      pure (`GuardExpr` expr)

caseAltParser :: TokParser CaseAlt
caseAltParser = withSpan $ do
  pat <- patternParser
  rhs <- rhsParser
  pure $ \span' ->
    CaseAlt
      { caseAltSpan = span',
        caseAltPattern = pat,
        caseAltRhs = rhs
      }

caseExprParser :: TokParser Expr
caseExprParser = withSpan $ do
  keywordTok TkKeywordCase
  scrutinee <- exprParser
  keywordTok TkKeywordOf
  alts <- bracedAlts <|> plainAlts
  pure $ \span' -> ECase span' scrutinee alts
  where
    plainAlts = plainSemiSep1 caseAltParser
    bracedAlts = bracedSemiSep caseAltParser

parenExprParser :: TokParser Expr
parenExprParser = withSpan $ do
  expectedTok TkSpecialLParen
  mClosed <- MP.optional (expectedTok TkSpecialRParen)
  case mClosed of
    Just () -> pure (`ETuple` [])
    Nothing -> MP.try parseNegateParen <|> MP.try parseSection <|> MP.try parseTupleSectionExpr <|> parseParenOrTupleExpr
  where
    parseNegateParen = do
      minusTok <- minusTokenValueParser
      nextTok <- lookAhead anySingle
      guard (parenNegateAllowed minusTok nextTok)
      inner <- exprParser
      expectedTok TkSpecialRParen
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
        (SourceSpan _ _ firstEndLine firstEndCol, SourceSpan secondStartLine secondStartCol _ _) ->
          firstEndLine == secondStartLine && firstEndCol == secondStartCol
        _ -> False

    parseSection = do
      MP.try parseSectionR <|> parseSectionL

    parseSectionR = do
      op <- infixOperatorParserExcept []
      rhs <- exprParser
      expectedTok TkSpecialRParen
      pure (\span' -> EParen span' (ESectionR span' op rhs))

    parseSectionL = do
      lhs <- appExprParser
      op <- infixOperatorParserExcept []
      expectedTok TkSpecialRParen
      pure (\span' -> EParen span' (ESectionL span' lhs op))

    parseTupleSectionExpr = do
      -- Try to parse as tuple section first (e.g., "(,1)" or "(1,)")
      -- If that fails, fall back to regular tuple/paren parsing
      values <- parseTupleSection
      pure (`ETupleSection` values)

    parseParenOrTupleExpr = do
      first <- exprParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          expectedTok TkSpecialRParen
          pure (`EParen` first)
        Just () -> do
          second <- exprParser
          more <- MP.many (expectedTok TkSpecialComma *> exprParser)
          expectedTok TkSpecialRParen
          pure (`ETuple` (first : second : more))

parseTupleSection :: TokParser [Maybe Expr]
parseTupleSection = do
  first <- MP.optional exprParser
  _ <- expectedTok TkSpecialComma
  middle <- MP.many (MP.try (MP.optional exprParser <* expectedTok TkSpecialComma))
  lastSlot <- MP.optional exprParser
  expectedTok TkSpecialRParen
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
compStmtParser = MP.try compGenStmtParser <|> compGuardStmtParser

compGenStmtParser :: TokParser CompStmt
compGenStmtParser = withSpan $ do
  pat <- patternParser
  expectedTok TkReservedLeftArrow
  expr <- exprParser
  pure (\span' -> CompGen span' pat expr)

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
      body <- exprParser
      pure (\span' -> ELambdaPats span' pats body)

    bracedAlts = bracedSemiSep1 caseAltParser

letExprParser :: TokParser Expr
letExprParser = withSpan $ do
  keywordTok TkKeywordLet
  decls <- bracedDeclsParser <|> plainDeclsParser
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
localFunctionDeclParser = withSpan $ MP.try infixLocalFunctionParser <|> prefixLocalFunctionParser
  where
    prefixLocalFunctionParser = do
      name <- binderNameParser
      pats <- MP.many simplePatternParser
      rhs <- equationRhsParser
      pure (\span' -> functionBindDecl span' name pats rhs)

    infixLocalFunctionParser = do
      lhsPat <- patternParser
      op <- infixOperatorNameParser
      rhsPat <- patternParser
      rhs <- equationRhsParser
      pure (\span' -> functionBindDecl span' op [lhsPat, rhsPat] rhs)

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
  expectedTok TkSpecialLBrace
  mClose <- MP.optional (expectedTok TkSpecialRBrace)
  case mClose of
    Just () -> pure (\span' -> PRecord span' con [])
    Nothing -> do
      fields <- recordFieldPatternParser `MP.sepEndBy` expectedTok TkSpecialComma
      expectedTok TkSpecialRBrace
      pure (\span' -> PRecord span' con fields)

recordFieldPatternParser :: TokParser (Text, Pattern)
recordFieldPatternParser = do
  startPos <- MP.getSourcePos
  field <- identifierTextParser
  mEq <- MP.optional (expectedTok TkReservedEquals)
  endPos <- MP.getSourcePos
  case mEq of
    Just () -> do
      pat <- patternParser
      pure (field, pat)
    Nothing -> do
      -- NamedFieldPuns: just "field" means "field = field"
      let span' = sourceSpanFromPositions startPos endPos
      pure (field, PVar span' field)

listPatternParser :: TokParser Pattern
listPatternParser = withSpan $ do
  expectedTok TkSpecialLBracket
  elems <- patternParser `MP.sepBy` expectedTok TkSpecialComma
  expectedTok TkSpecialRBracket
  pure (`PList` elems)

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpan $ do
  expectedTok TkSpecialLParen
  MP.try unitPatternParser <|> MP.try viewPatternParser <|> tupleOrParenPatternParser
  where
    unitPatternParser = do
      expectedTok TkSpecialRParen
      pure (`PTuple` [])

    viewPatternParser = do
      viewExpr <- exprParser
      expectedTok TkReservedRightArrow
      inner <- patternParser
      expectedTok TkSpecialRParen
      pure (\span' -> PView span' viewExpr inner)

    tupleOrParenPatternParser = do
      first <- patternParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          expectedTok TkSpecialRParen
          pure (`PParen` first)
        Just () -> do
          second <- patternParser
          more <- MP.many (expectedTok TkSpecialComma *> patternParser)
          expectedTok TkSpecialRParen
          pure (`PTuple` (first : second : more))

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
typeParser = MP.try forallTypeParser <|> MP.try contextTypeParser <|> typeFunParser

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

buildInfixType :: Type -> (Text, Type) -> Type
buildInfixType lhs (op, rhs) =
  let span' = mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)
      opType = TCon span' op
   in TApp span' (TApp span' opType lhs) rhs

typeInfixOperatorParser :: TokParser Text
typeInfixOperatorParser =
  tokenSatisfy "type infix operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op
        | op /= "."
            && op /= "!"
            && op /= "-" ->
            Just op
      TkConSym op -> Just op
      TkQVarSym op -> Just op
      TkQConSym op -> Just op
      _ -> Nothing

typeAppParser :: TokParser Type
typeAppParser = do
  first <- typeAtomParser
  rest <- MP.many typeAtomParser
  pure (foldl buildTypeApp first rest)

buildTypeApp :: Type -> Type -> Type
buildTypeApp lhs rhs =
  TApp (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs rhs

typeAtomParser :: TokParser Type
typeAtomParser =
  MP.try promotedTypeParser
    <|> typeLiteralTypeParser
    <|> typeQuasiQuoteParser
    <|> typeListParser
    <|> MP.try typeParenOperatorParser
    <|> typeParenOrTupleParser
    <|> typeStarParser
    <|> typeIdentifierParser

typeLiteralTypeParser :: TokParser Type
typeLiteralTypeParser = withSpan $ do
  repr <- tokenSatisfy "type literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger _ -> Just (lexTokenText tok)
      TkIntegerBase _ _ -> Just (lexTokenText tok)
      TkString _ -> Just (lexTokenText tok)
      _ -> Nothing
  pure (`TCon` repr)

promotedTypeParser :: TokParser Type
promotedTypeParser = withSpan $ do
  expectedTok (TkVarSym "'")
  promotedSuffix <- promotedTypeSuffixParser
  pure (\span' -> TCon span' ("'" <> promotedSuffix))

promotedTypeSuffixParser :: TokParser Text
promotedTypeSuffixParser =
  promotedConstructorSuffixParser
    <|> promotedBracketedSuffixParser
    <|> promotedParenthesizedSuffixParser

promotedConstructorSuffixParser :: TokParser Text
promotedConstructorSuffixParser =
  tokenSatisfy "promoted constructor" $ \tok ->
    case lexTokenKind tok of
      TkConId name -> Just name
      TkQConId name -> Just name
      _ -> Nothing

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
  pure (`TCon` op)

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
      _ -> TCon span' name

typeStarParser :: TokParser Type
typeStarParser = withSpan $ do
  expectedTok (TkVarSym "*")
  pure TStar

typeListParser :: TokParser Type
typeListParser = withSpan $ do
  expectedTok TkSpecialLBracket
  inner <- typeParser
  expectedTok TkSpecialRBracket
  pure (`TList` inner)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpan $ do
  expectedTok TkSpecialLParen
  mClosed <- MP.optional (expectedTok TkSpecialRParen)
  case mClosed of
    Just () -> pure (`TTuple` [])
    Nothing -> do
      MP.try tupleConstructorParser <|> parenthesizedTypeOrTupleParser
  where
    tupleConstructorParser = do
      _ <- expectedTok TkSpecialComma
      moreCommas <- MP.many (expectedTok TkSpecialComma)
      expectedTok TkSpecialRParen
      let arity = 2 + length moreCommas
          tupleConName = "(" <> T.replicate (arity - 1) "," <> ")"
      pure (`TCon` tupleConName)

    parenthesizedTypeOrTupleParser = do
      first <- typeParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          expectedTok TkSpecialRParen
          pure (`TParen` first)
        Just () -> do
          second <- typeParser
          more <- MP.many (expectedTok TkSpecialComma *> typeParser)
          expectedTok TkSpecialRParen
          pure (`TTuple` (first : second : more))
