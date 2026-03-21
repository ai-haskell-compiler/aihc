{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Expr
  ( exprParser,
    equationRhsParser,
    simplePatternParser,
    patternParser,
    typeParser,
    typeAtomParser,
  )
where

import Control.Monad (guard)
import Data.Char (isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Lexer (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
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
  case lexTokenKind tok of
    TkKeywordDo -> doExprParser
    TkKeywordIf -> ifExprParser
    TkKeywordCase -> caseExprParser
    TkKeywordLet -> letExprParser
    TkReservedBackslash -> lambdaExprParser
    _ -> infixExprParserExcept forbiddenInfix

ifExprParser :: TokParser Expr
ifExprParser = withSpan $ do
  keywordTok TkKeywordIf
  cond <- exprParser
  keywordTok TkKeywordThen
  yes <- exprParser
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
  operatorLikeTok "<-"
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
  lhs <- MP.try negateExprParser <|> appExprParser
  rest <- MP.many ((,) <$> infixOperatorParserExcept forbidden <*> appExprParser)
  pure (foldl buildInfix lhs rest)

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
      symbolLikeTok "`"
      op <- identifierTextParser
      symbolLikeTok "`"
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
  first <- atomExprParser
  rest <- MP.many atomExprParser
  pure $ \span' ->
    foldl (EApp span') first rest

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
  symbolLikeTok "("
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
      TkReservedTilde -> Just "~"
      TkReservedDotDot -> Just ".."
      _ -> Nothing
  symbolLikeTok ")"
  pure (`EVar` op)

patternParser :: TokParser Pattern
patternParser = asPatternParser

asPatternParser :: TokParser Pattern
asPatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        operatorLikeTok "@"
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
  operatorLikeTok "!"
  inner <- patternAtomParser
  pure (`PStrict` inner)

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpan $ do
  operatorLikeTok "~"
  inner <- patternAtomParser
  pure (`PIrrefutable` inner)

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = withSpan $ do
  operatorLikeTok "-"
  lit <- literalParser
  pure (`PNegLit` lit)

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser = withSpan $ do
  identifierExact "_"
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
rhsParser = rhsParserWithArrow "->"

equationRhsParser :: TokParser Rhs
equationRhsParser = rhsParserWithArrow "="

rhsParserWithArrow :: Text -> TokParser Rhs
rhsParserWithArrow arrow = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkReservedPipe -> guardedRhssParser arrow
    TkReservedRightArrow | arrow == "->" -> unguardedRhsParser arrow
    TkReservedEquals | arrow == "=" -> unguardedRhsParser arrow
    TkVarSym op | op == arrow -> unguardedRhsParser arrow
    TkConSym op | op == arrow -> unguardedRhsParser arrow
    _ -> fail ("expected " <> T.unpack arrow <> " or guarded right-hand side")

unguardedRhsParser :: Text -> TokParser Rhs
unguardedRhsParser arrow = withSpan $ do
  operatorLikeTok arrow
  body <- exprParser
  pure (`UnguardedRhs` body)

guardedRhssParser :: Text -> TokParser Rhs
guardedRhssParser arrow = withSpan $ do
  grhss <- MP.some (guardedRhsParser arrow)
  pure (`GuardedRhss` grhss)

guardedRhsParser :: Text -> TokParser GuardedRhs
guardedRhsParser arrow = withSpan $ do
  operatorLikeTok "|"
  guards <- guardQualifierParser arrow `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok arrow
  body <- exprParserExcept ["|", arrow]
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

guardQualifierParser :: Text -> TokParser GuardQualifier
guardQualifierParser _arrow = MP.try guardPatParser <|> MP.try guardLetParser <|> guardExprParser
  where
    guardPatParser = withSpan $ do
      pat <- patternParser
      operatorLikeTok "<-"
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
  symbolLikeTok "("
  mClosed <- MP.optional (symbolLikeTok ")")
  case mClosed of
    Just () -> pure (`ETuple` [])
    Nothing -> MP.try parseNegateParen <|> MP.try parseSection <|> MP.try parseTupleSectionExpr <|> parseParenOrTupleExpr
  where
    parseNegateParen = do
      minusTok <- minusTokenValueParser
      nextTok <- lookAhead anySingle
      guard (parenNegateAllowed minusTok nextTok)
      inner <- exprParser
      symbolLikeTok ")"
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
      symbolLikeTok ")"
      pure (\span' -> EParen span' (ESectionR span' op rhs))

    parseSectionL = do
      lhs <- appExprParser
      op <- infixOperatorParserExcept []
      symbolLikeTok ")"
      pure (\span' -> EParen span' (ESectionL span' lhs op))

    parseTupleSectionExpr = do
      -- Try to parse as tuple section first (e.g., "(,1)" or "(1,)")
      -- If that fails, fall back to regular tuple/paren parsing
      values <- parseTupleSection
      pure (`ETupleSection` values)

    parseParenOrTupleExpr = do
      first <- exprParser
      mComma <- MP.optional (symbolLikeTok ",")
      case mComma of
        Nothing -> do
          symbolLikeTok ")"
          pure (`EParen` first)
        Just () -> do
          second <- exprParser
          more <- MP.many (symbolLikeTok "," *> exprParser)
          symbolLikeTok ")"
          pure (`ETuple` (first : second : more))

parseTupleSection :: TokParser [Maybe Expr]
parseTupleSection = do
  first <- MP.optional exprParser
  _ <- symbolLikeTok ","
  middle <- MP.many (MP.try (MP.optional exprParser <* symbolLikeTok ","))
  lastSlot <- MP.optional exprParser
  symbolLikeTok ")"
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
  symbolLikeTok "["
  mClose <- MP.optional (symbolLikeTok "]")
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
      operatorLikeTok "|"
      firstGroup <- compStmtParser `MP.sepBy1` symbolLikeTok ","
      -- Try to parse additional parallel groups separated by |
      moreGroups <- MP.many (operatorLikeTok "|" *> (compStmtParser `MP.sepBy1` symbolLikeTok ","))
      symbolLikeTok "]"
      pure $ \span' ->
        case moreGroups of
          [] -> EListComp span' first firstGroup
          _ -> EListCompParallel span' first (firstGroup : moreGroups)

    arithFromToTailParser = do
      operatorLikeTok ".."
      mTo <- MP.optional exprParser
      symbolLikeTok "]"
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFrom span' first
            Just toExpr -> ArithSeqFromTo span' first toExpr

    commaTailParser = do
      symbolLikeTok ","
      second <- exprParser
      MP.try (arithFromThenTailParser second) <|> listTailParser second

    arithFromThenTailParser second = do
      operatorLikeTok ".."
      mTo <- MP.optional exprParser
      symbolLikeTok "]"
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFromThen span' first second
            Just toExpr -> ArithSeqFromThenTo span' first second toExpr

    listTailParser second = do
      rest <- MP.many (symbolLikeTok "," *> exprParser)
      symbolLikeTok "]"
      pure (\span' -> EList span' (first : second : rest))

    singletonTailParser = do
      symbolLikeTok "]"
      pure (\span' -> EList span' [first])

compStmtParser :: TokParser CompStmt
compStmtParser = MP.try compGenStmtParser <|> compGuardStmtParser

compGenStmtParser :: TokParser CompStmt
compGenStmtParser = withSpan $ do
  pat <- patternParser
  operatorLikeTok "<-"
  expr <- exprParser
  pure (\span' -> CompGen span' pat expr)

lambdaExprParser :: TokParser Expr
lambdaExprParser = withSpan $ do
  operatorLikeTok "\\"
  lambdaCaseParser <|> lambdaPatsParser
  where
    lambdaCaseParser = do
      keywordTok TkKeywordCase
      alts <- bracedAlts
      pure (`ELambdaCase` alts)

    lambdaPatsParser = do
      pats <- MP.some patternParser
      operatorLikeTok "->"
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
  names <- binderNameParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

localFunctionDeclParser :: TokParser Decl
localFunctionDeclParser = withSpan $ do
  name <- binderNameParser
  pats <- MP.many simplePatternParser
  rhs <- equationRhsParser
  pure (\span' -> functionBindDecl span' name pats rhs)

localPatternDeclParser :: TokParser Decl
localPatternDeclParser = withSpan $ do
  pat <- patternParser
  operatorLikeTok "="
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
  symbolLikeTok "{"
  mClose <- MP.optional (symbolLikeTok "}")
  case mClose of
    Just () -> pure (\span' -> PRecord span' con [])
    Nothing -> do
      fields <- recordFieldPatternParser `MP.sepEndBy` symbolLikeTok ","
      symbolLikeTok "}"
      pure (\span' -> PRecord span' con fields)

recordFieldPatternParser :: TokParser (Text, Pattern)
recordFieldPatternParser = do
  field <- identifierTextParser
  operatorLikeTok "="
  pat <- patternParser
  pure (field, pat)

listPatternParser :: TokParser Pattern
listPatternParser = withSpan $ do
  symbolLikeTok "["
  elems <- patternParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok "]"
  pure (`PList` elems)

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpan $ do
  symbolLikeTok "("
  MP.try unitPatternParser <|> MP.try viewPatternParser <|> tupleOrParenPatternParser
  where
    unitPatternParser = do
      symbolLikeTok ")"
      pure (`PTuple` [])

    viewPatternParser = do
      viewExpr <- exprParser
      operatorLikeTok "->"
      inner <- patternParser
      symbolLikeTok ")"
      pure (\span' -> PView span' viewExpr inner)

    tupleOrParenPatternParser = do
      first <- patternParser
      mComma <- MP.optional (symbolLikeTok ",")
      case mComma of
        Nothing -> do
          symbolLikeTok ")"
          pure (`PParen` first)
        Just () -> do
          second <- patternParser
          more <- MP.many (symbolLikeTok "," *> patternParser)
          symbolLikeTok ")"
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
        operatorLikeTok "@"
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> patternAtomParser

typeParser :: TokParser Type
typeParser = MP.try forallTypeParser <|> MP.try contextTypeParser <|> typeFunParser

forallTypeParser :: TokParser Type
forallTypeParser = withSpan $ do
  identifierExact "forall"
  binders <- MP.some identifierTextParser
  operatorLikeTok "."
  inner <- MP.try contextTypeParser <|> typeFunParser
  pure (\span' -> TForall span' binders inner)

contextTypeParser :: TokParser Type
contextTypeParser = do
  constraints <- constraintsParser
  operatorLikeTok "=>"
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
  mRhs <- MP.optional (operatorLikeTok "->" *> typeParser)
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
  typeQuasiQuoteParser
    <|> typeListParser
    <|> MP.try typeParenOperatorParser
    <|> typeParenOrTupleParser
    <|> typeStarParser
    <|> typeIdentifierParser

typeParenOperatorParser :: TokParser Type
typeParenOperatorParser = withSpan $ do
  symbolLikeTok "("
  op <- tokenSatisfy "type operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym | sym /= "*" -> Just sym
      TkConSym sym | sym /= "*" -> Just sym
      TkQVarSym sym -> Just sym
      TkQConSym sym -> Just sym
      -- Handle reserved operators that can be used as type constructors
      TkReservedRightArrow -> Just "->"
      TkReservedTilde -> Just "~"
      _ -> Nothing
  symbolLikeTok ")"
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
  operatorLikeTok "*"
  pure TStar

typeListParser :: TokParser Type
typeListParser = withSpan $ do
  symbolLikeTok "["
  inner <- typeParser
  symbolLikeTok "]"
  pure (`TList` inner)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpan $ do
  symbolLikeTok "("
  mClosed <- MP.optional (symbolLikeTok ")")
  case mClosed of
    Just () -> pure (`TTuple` [])
    Nothing -> do
      first <- typeParser
      mComma <- MP.optional (symbolLikeTok ",")
      case mComma of
        Nothing -> do
          symbolLikeTok ")"
          pure (`TParen` first)
        Just () -> do
          second <- typeParser
          more <- MP.many (symbolLikeTok "," *> typeParser)
          symbolLikeTok ")"
          pure (`TTuple` (first : second : more))
