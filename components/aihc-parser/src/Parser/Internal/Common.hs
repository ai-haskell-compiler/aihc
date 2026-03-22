{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Common
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
    stringTextParser,
    withSpan,
    sourceSpanFromPositions,
    markSingleParenConstraint,
    parens,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep1,
    constraintParserWith,
    constraintsParserWith,
    contextParserWith,
    functionBindValue,
    functionBindDecl,
  )
where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
import Parser.Lexer (LexToken (..), LexTokenKind (..))
import Parser.Types (TokStream)
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

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
  TkReservedTilde -> "operator '~'"
  TkReservedDoubleArrow -> "operator '=>'"
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
moduleNameParser =
  tokenSatisfy "module name" $ \tok ->
    case lexTokenKind tok of
      TkConId ident | isModuleName ident -> Just ident
      TkQConId ident | isModuleName ident -> Just ident
      _ -> Nothing

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

markSingleParenConstraint :: [Constraint] -> [Constraint]
markSingleParenConstraint constraints =
  case constraints of
    [constraint] -> [constraint {constraintParen = True}]
    _ -> constraints

parens :: TokParser a -> TokParser a
parens parser = do
  expectedTok TkSpecialLParen
  res <- parser
  expectedTok TkSpecialRParen
  pure res

skipSemicolons :: TokParser ()
skipSemicolons = MP.skipMany (expectedTok TkSpecialSemicolon)

bracedSemiSep :: TokParser a -> TokParser [a]
bracedSemiSep parser = do
  expectedTok TkSpecialLBrace
  skipSemicolons
  items <- parser `MP.sepEndBy` expectedTok TkSpecialSemicolon
  expectedTok TkSpecialRBrace
  pure items

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 parser = do
  expectedTok TkSpecialLBrace
  skipSemicolons
  items <- parser `MP.sepEndBy1` expectedTok TkSpecialSemicolon
  expectedTok TkSpecialRBrace
  pure items

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 parser = MP.some (parser <* skipSemicolons)

constraintParserWith :: TokParser Type -> TokParser Constraint
constraintParserWith typeAtomParser = withSpan $ do
  className <- constructorIdentifierParser
  args <- MP.many typeAtomParser
  pure $ \span' ->
    Constraint
      { constraintSpan = span',
        constraintClass = className,
        constraintArgs = args,
        constraintParen = False
      }

constraintsParserWith :: TokParser Type -> TokParser [Constraint]
constraintsParserWith typeAtomParser =
  MP.try (parens (markSingleParenConstraint <$> (constraintParserWith typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma)))
    <|> fmap pure (constraintParserWith typeAtomParser)

contextParserWith :: TokParser Type -> TokParser [Constraint]
contextParserWith = constraintsParserWith

functionBindValue :: SourceSpan -> Text -> [Pattern] -> Rhs -> ValueDecl
functionBindValue span' name pats rhs =
  FunctionBind
    span'
    name
    [ Match
        { matchSpan = span',
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: SourceSpan -> Text -> [Pattern] -> Rhs -> Decl
functionBindDecl span' name pats rhs =
  DeclValue span' (functionBindValue span' name pats rhs)

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
