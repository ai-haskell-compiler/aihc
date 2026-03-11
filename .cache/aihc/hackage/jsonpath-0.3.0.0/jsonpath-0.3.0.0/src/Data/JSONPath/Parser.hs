{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.JSONPath.Parser (jsonPathElement, jsonPath) where

import qualified Data.Char as Char
import Data.Functor
import Data.Functor.Identity
import Data.JSONPath.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec as P
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.ParsecT Void Text Identity

jsonPath :: Parser a -> Parser [JSONPathElement]
jsonPath endParser = do
  _ <- optional $ char '$'
  manyTill jsonPathElement (hidden $ lookAhead endParser)

jsonPathElement :: Parser JSONPathElement
jsonPathElement =
  ignoreSurroundingSpace $
    try anyChild
      <|> try keyChild
      <|> try slice
      <|> try indexChild
      <|> try union
      <|> try filterParser
      <|> try search
      <|> searchBeginningWithSlice

indexChild :: Parser JSONPathElement
indexChild = IndexChild <$> inSqBr indexChildWithoutBrackets

indexChildWithoutBrackets :: Parser Int
indexChildWithoutBrackets = ignoreSurroundingSpace $ L.signed space L.decimal

slice :: Parser JSONPathElement
slice =
  uncurry3 Slice
    <$> inSqBr sliceWithoutBrackets

sliceWithoutBrackets :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceWithoutBrackets = do
  (,,)
    <$> parseStart
    <*> parseEnd
    <*> parseStep
  where
    parseStart :: Parser (Maybe Int)
    parseStart =
      ignoreSurroundingSpace (optional (L.signed space L.decimal))
        <* char ':'

    parseEnd =
      ignoreSurroundingSpace $ optional $ L.signed space L.decimal

    parseStep =
      optional (char ':')
        *> ignoreSurroundingSpace (optional (L.signed space L.decimal))

keyChild :: Parser JSONPathElement
keyChild = KeyChild <$> (try sqBrKeyChild <|> dotKeyChild)

sqBrKeyChild :: Parser Text
sqBrKeyChild =
  inSqBr $ ignoreSurroundingSpace quotedString

dotKeyChild :: Parser Text
dotKeyChild = char '.' *> takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_')

anyChild :: Parser JSONPathElement
anyChild = ignoreSurroundingSpace $ AnyChild <$ (void (string ".*") <|> void (inSqBr (char '*')))

union :: Parser JSONPathElement
union =
  inSqBr $
    Union <$> do
      firstElement <- unionElement
      restElements <- some (char ',' *> unionElement)
      pure (firstElement : restElements)

unionElement :: Parser UnionElement
unionElement =
  try (uncurry3 UESlice <$> sliceWithoutBrackets)
    <|> try (UEIndexChild <$> indexChildWithoutBrackets)
    <|> UEKeyChild <$> ignoreSurroundingSpace quotedString

filterParser :: Parser JSONPathElement
filterParser = inSqBr $ do
  _ <- ignoreSurroundingSpace $ char '?'
  Filter <$> filterExpr (ignoreSurroundingSpace (char ']'))

filterExpr :: Parser a -> Parser FilterExpr
filterExpr endParser =
  try (orFilterExpr endParser)
    <|> try (andFilterExpr endParser)
    <|> basicFilterExpr endParser

basicFilterExpr :: Parser a -> Parser FilterExpr
basicFilterExpr endParser = do
  maybeNot <- optional (char '!')
  expr <-
    try (comparisionFilterExpr endParser)
      <|> try (existsFilterExpr endParser)
      <|> (inParens (filterExpr closingParen) <* lookAhead endParser)
  case maybeNot of
    Nothing -> pure expr
    Just _ -> pure $ Not expr

comparisionFilterExpr :: Parser a -> Parser FilterExpr
comparisionFilterExpr endParser = do
  expr <-
    ComparisonExpr
      <$> comparable condition
      <*> condition
      <*> comparable endParser
  _ <- lookAhead endParser
  pure expr

existsFilterExpr :: Parser a -> Parser FilterExpr
existsFilterExpr endParser =
  ExistsExpr <$> singularPath endParser

singularPath :: Parser a -> Parser SingularPath
singularPath endParser =
  SingularPath
    <$> beginningPoint
    <*> manyTill singularPathElement (lookAhead endParser)

singularPathElement :: Parser SingularPathElement
singularPathElement =
  (Key <$> try dotKeyChild)
    <|> (Key <$> try sqBrKeyChild)
    <|> Index <$> inSqBr indexChildWithoutBrackets

orFilterExpr :: Parser a -> Parser FilterExpr
orFilterExpr endParser = do
  let orOperator = ignoreSurroundingSpace $ string "||"
  e1 <-
    -- If there is an '&&' operation, it should take precedence over the '||'
    try (andFilterExpr orOperator)
      <|> basicFilterExpr orOperator
  _ <- orOperator
  Or e1 <$> filterExpr endParser

andFilterExpr :: Parser a -> Parser FilterExpr
andFilterExpr endParser = do
  let andOperator = ignoreSurroundingSpace $ string "&&"
  e1 <- basicFilterExpr andOperator
  _ <- andOperator
  And e1 <$> filterExpr endParser

search :: Parser JSONPathElement
search = do
  _ <- char '.'
  _ <- lookAhead (char '.')
  Search <$> some jsonPathElement

searchBeginningWithSlice :: Parser JSONPathElement
searchBeginningWithSlice = do
  _ <- string ".."
  _ <- lookAhead (char '[')
  Search <$> some jsonPathElement

beginningPoint :: Parser BeginningPoint
beginningPoint =
  try (char '$' $> Root)
    <|> (char '@' $> CurrentObject)

condition :: Parser Condition
condition =
  ignoreSurroundingSpace $
    string "==" $> Equal
      <|> string "!=" $> NotEqual
      <|> string "<=" $> SmallerThanOrEqual
      <|> string ">=" $> GreaterThanOrEqual
      <|> string ">" $> GreaterThan
      <|> string "<" $> SmallerThan

comparable :: Parser a -> Parser Comparable
comparable endParser = do
  CmpNumber <$> L.scientific
    <|> CmpString <$> quotedString
    <|> CmpBool <$> bool
    <|> CmpNull <$ string "null"
    <|> CmpPath <$> singularPath endParser

bool :: Parser Bool
bool =
  True <$ string "true"
    <|> False <$ string "false"

ignoreSurroundingSpace :: Parser a -> Parser a
ignoreSurroundingSpace p = space *> p <* space

inSqBr :: Parser a -> Parser a
inSqBr p = openingSqBr *> p <* closingSqBr

openingSqBr :: Parser Char
openingSqBr = ignoreSurroundingSpace (char '[')

closingSqBr :: Parser Char
closingSqBr = ignoreSurroundingSpace (char ']')

inParens :: Parser a -> Parser a
inParens p = openingParen *> p <* closingParen

openingParen :: Parser Char
openingParen = ignoreSurroundingSpace (char '(')

closingParen :: Parser Char
closingParen = ignoreSurroundingSpace (char ')')

quotedString :: Parser Text
quotedString = ignoreSurroundingSpace $ Text.pack <$> (inQuotes '"' <|> inQuotes '\'')
  where
    inQuotes quoteChar =
      char quoteChar *> manyTill L.charLiteral (char quoteChar)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
