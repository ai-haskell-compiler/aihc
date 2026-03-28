{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Common
  ( TokParser,
    keywordTok,
    expectedTok,
    varIdTok,
    tokenSatisfy,
    moduleNameParser,
    identifierTextParser,
    lowerIdentifierParser,
    constructorIdentifierParser,
    binderNameParser,
    operatorTextParser,
    infixOperatorNameParser,
    stringTextParser,
    withSpan,
    sourceSpanFromPositions,
    parens,
    braces,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep1,
    constraintParserWith,
    constraintsParserWith,
    contextParserWith,
    functionHeadParserWith,
    functionBindValue,
    functionBindDecl,
  )
where

import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..))
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), TokStream, mkFoundToken)
import Data.Char (isUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec ParserErrorComponent TokStream

keywordTok :: LexTokenKind -> TokParser ()
keywordTok expected =
  tokenSatisfy ("keyword " <> renderKeyword expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

-- | Match a specific token kind exactly.
expectedTok :: LexTokenKind -> TokParser ()
expectedTok expected =
  tokenSatisfy (renderTokenKind expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

-- | Match a specific variable identifier (contextual keyword).
varIdTok :: Text -> TokParser ()
varIdTok expected =
  tokenSatisfy ("identifier '" <> T.unpack expected <> "'") $ \tok ->
    case lexTokenKind tok of
      TkVarId ident | ident == expected -> Just ()
      _ -> Nothing

renderTokenKind :: LexTokenKind -> String
renderTokenKind tk = case tk of
  TkSpecialLParen -> "symbol '('"
  TkSpecialRParen -> "symbol ')'"
  TkSpecialUnboxedLParen -> "symbol '(#'"
  TkSpecialUnboxedRParen -> "symbol '#)'"
  TkSpecialComma -> "symbol ','"
  TkSpecialSemicolon -> "symbol ';'"
  TkSpecialLBracket -> "symbol '['"
  TkSpecialRBracket -> "symbol ']'"
  TkSpecialBacktick -> "symbol '`'"
  TkSpecialLBrace -> "symbol '{'"
  TkSpecialRBrace -> "symbol '}'"
  TkReservedDotDot -> "operator '..'"
  TkReservedColon -> "operator ':'"
  TkReservedDoubleColon -> "operator '::'"
  TkReservedEquals -> "operator '='"
  TkReservedBackslash -> "operator '\\'"
  TkReservedPipe -> "operator '|'"
  TkReservedLeftArrow -> "operator '<-'"
  TkReservedRightArrow -> "operator '->'"
  TkReservedAt -> "operator '@'"
  TkReservedDoubleArrow -> "operator '=>'"
  TkPrefixBang -> "bang pattern '!'"
  TkPrefixTilde -> "irrefutable pattern '~'"
  TkVarSym op -> "operator '" <> show op <> "'"
  TkConSym op -> "operator '" <> show op <> "'"
  _ -> show tk

tokenSatisfy :: String -> (LexToken -> Maybe a) -> TokParser a
tokenSatisfy label f =
  MP.label label $ do
    tok <- lookAhead anySingle
    case f tok of
      Just out -> out <$ anySingle
      Nothing -> fail label

moduleNameParser :: TokParser Text
moduleNameParser = do
  mTok <- MP.optional (lookAhead anySingle)
  case mTok of
    Nothing ->
      MP.customFailure
        UnexpectedTokenExpecting
          { unexpectedFound = Nothing,
            unexpectedExpecting = "module name"
          }
    Just tok ->
      case lexTokenKind tok of
        TkConId ident | isModuleName ident -> ident <$ anySingle
        TkQConId ident | isModuleName ident -> ident <$ anySingle
        _ ->
          MP.customFailure
            UnexpectedTokenExpecting
              { unexpectedFound = Just (mkFoundToken tok),
                unexpectedExpecting = "module name"
              }

identifierTextParser :: TokParser Text
identifierTextParser =
  tokenSatisfy "identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkConId ident -> Just ident
      TkQVarId ident -> Just ident
      TkQConId ident -> Just ident
      _ -> Nothing

lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId ident -> Just ident
      _ -> Nothing

constructorIdentifierParser :: TokParser Text
constructorIdentifierParser =
  tokenSatisfy "constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just ident
      TkQConId ident -> Just ident
      _ -> Nothing

binderNameParser :: TokParser Text
binderNameParser =
  identifierTextParser
    <|> parens operatorTextParser

operatorTextParser :: TokParser Text
operatorTextParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just op
      TkConSym op -> Just op
      TkQVarSym op -> Just op
      TkQConSym op -> Just op
      _ -> Nothing

-- | Parse an infix operator name (varop) for function definitions.
-- Per Haskell Report section 4.4.3, funlhs uses 'varop' which is:
--   varop → varsym | ` varid `
-- This excludes constructor operators (consym) and qualified operators.
-- Note: Whitespace-sensitive lexing (GHC proposal 0229) now distinguishes
-- TkVarSym "!" (infix operator) from TkPrefixBang (bang pattern), so we
-- can accept all VarSym operators here.
infixOperatorNameParser :: TokParser Text
infixOperatorNameParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just op
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- varIdTextParser
      expectedTok TkSpecialBacktick
      pure op
    varIdTextParser =
      tokenSatisfy "variable identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just name
          _ -> Nothing

stringTextParser :: TokParser Text
stringTextParser =
  tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

withSpan :: TokParser (SourceSpan -> a) -> TokParser a
withSpan parser = do
  start <- MP.getSourcePos
  out <- parser
  out . sourceSpanFromPositions start <$> MP.getSourcePos

sourceSpanFromPositions :: SourcePos -> SourcePos -> SourceSpan
sourceSpanFromPositions start end =
  SourceSpan
    { sourceSpanStartLine = MP.unPos (sourceLine start),
      sourceSpanStartCol = MP.unPos (sourceColumn start),
      sourceSpanEndLine = MP.unPos (sourceLine end),
      sourceSpanEndCol = MP.unPos (sourceColumn end)
    }

parens :: TokParser a -> TokParser a
parens parser = do
  expectedTok TkSpecialLParen
  res <- parser
  expectedTok TkSpecialRParen
  pure res

braces :: TokParser a -> TokParser a
braces parser = do
  expectedTok TkSpecialLBrace
  res <- parser
  expectedTok TkSpecialRBrace
  pure res

skipSemicolons :: TokParser ()
skipSemicolons = MP.skipMany (expectedTok TkSpecialSemicolon)

bracedSemiSep :: TokParser a -> TokParser [a]
bracedSemiSep parser =
  braces $ do
    skipSemicolons
    parser `MP.sepEndBy` expectedTok TkSpecialSemicolon

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 parser =
  braces $ do
    skipSemicolons
    parser `MP.sepEndBy1` expectedTok TkSpecialSemicolon

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 parser = MP.some (parser <* skipSemicolons)

constraintParserWith :: TokParser Type -> TokParser Constraint
constraintParserWith typeAtomParser =
  MP.try parenthesizedConstraintParser <|> bareConstraintParser
  where
    bareConstraintParser = withSpan $ do
      className <- constructorIdentifierParser
      args <- MP.many typeAtomParser
      pure $ \span' ->
        Constraint
          { constraintSpan = span',
            constraintClass = className,
            constraintArgs = args
          }
    parenthesizedConstraintParser = withSpan $ do
      constraint <- parens (constraintParserWith typeAtomParser)
      pure (`CParen` constraint)

constraintsParserWith :: TokParser Type -> TokParser [Constraint]
constraintsParserWith typeAtomParser =
  MP.try parenthesizedConstraintsParser <|> fmap pure (constraintParserWith typeAtomParser)
  where
    parenthesizedConstraintsParser = withSpan $ do
      constraints <- parens (constraintParserWith typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma)
      pure $ \span' ->
        case constraints of
          [constraint] -> [CParen span' constraint]
          _ -> constraints

contextParserWith :: TokParser Type -> TokParser [Constraint]
contextParserWith = constraintsParserWith

functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, Text, [Pattern])
functionHeadParserWith fullPatternParser prefixPatternParser =
  MP.try parenthesizedInfixHeadParser <|> MP.try infixHeadParser <|> prefixHeadParser
  where
    prefixHeadParser = do
      name <- binderNameParser
      pats <- MP.many prefixPatternParser
      pure (MatchHeadPrefix, name, pats)

    infixHeadParser = do
      lhsPat <- fullPatternParser
      op <- infixOperatorNameParser
      rhsPat <- fullPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat])

    parenthesizedInfixHeadParser = do
      expectedTok TkSpecialLParen
      lhsPat <- fullPatternParser
      op <- infixOperatorNameParser
      rhsPat <- fullPatternParser
      expectedTok TkSpecialRParen
      tailPats <- MP.many prefixPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)

functionBindValue :: SourceSpan -> MatchHeadForm -> Text -> [Pattern] -> Rhs -> ValueDecl
functionBindValue span' headForm name pats rhs =
  FunctionBind
    span'
    name
    [ Match
        { matchSpan = span',
          matchHeadForm = headForm,
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: SourceSpan -> MatchHeadForm -> Text -> [Pattern] -> Rhs -> Decl
functionBindDecl span' headForm name pats rhs =
  DeclValue span' (functionBindValue span' headForm name pats rhs)

renderKeyword :: LexTokenKind -> String
renderKeyword keyword =
  case keyword of
    TkKeywordModule -> "'module'"
    TkKeywordWhere -> "'where'"
    TkKeywordDo -> "'do'"
    TkKeywordData -> "'data'"
    TkKeywordImport -> "'import'"
    TkKeywordQualified -> "'qualified'"
    TkKeywordAs -> "'as'"
    TkKeywordHiding -> "'hiding'"
    TkKeywordCase -> "'case'"
    TkKeywordOf -> "'of'"
    TkKeywordLet -> "'let'"
    TkKeywordIn -> "'in'"
    TkKeywordIf -> "'if'"
    TkKeywordThen -> "'then'"
    TkKeywordElse -> "'else'"
    _ -> "keyword"

isModuleName :: Text -> Bool
isModuleName name =
  case T.splitOn "." name of
    [] -> False
    segments -> all isConstructorIdentifier segments

isConstructorIdentifier :: Text -> Bool
isConstructorIdentifier txt =
  case T.uncons txt of
    Just (c, _) -> isUpper c
    Nothing -> False
