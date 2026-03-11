{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.AST.Parser
  ( parseText
   -- * EDN document
  , parseDoc
    -- * Single value
  , parseTagged
  , parseValue
    -- * Primitive parsers
  , parseDiscard
  , parseNil
  , parseBool
  , parseNumber
  , parseKeyword
  , parseSymbol
  , parseCollections
    -- * Character classes
  , tagChars
  , keywordInitialChars
  , keywordChars
  , symbolInitialChars
  , symbolChars
    -- ** Basic characters
  , digitChars
  , lowerChars
  , upperChars
  , miscChars
  ) where

import Control.Applicative ((<|>))
import Data.Char (chr)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Data.EDN.AST.Types (Parser, Tagged(..), TaggedValue, Value(..))

import qualified Data.EDN.AST.Lexer as L
import qualified Data.EDN.AST.Types as EDN

parseText
  :: String -- ^ Source name, for megaparsec error reports
            -- e.g. @/path/to/file.edn@ or @<stdin>@
  -> Text   -- ^ EDN document body
  -> Either String TaggedValue
parseText sourceName =
  either (Left . P.errorBundlePretty) Right .
    P.parse parseDoc sourceName

parseDoc :: Parser TaggedValue
parseDoc = do
  L.dropWS
  parseTagged <* P.eof

parseTagged :: Parser TaggedValue
parseTagged = P.choice
  [ P.try withNS
  , P.try withoutNS
  , withoutTag
  ]
  where
    withNS = Tagged
      <$> (P.char '#' *> parseIdent)
      <*> (P.char '/' *> parseIdent)
      <*> (L.dropWS *> parseValue)

    withoutNS = Tagged
      <$> pure mempty
      <*> (P.char '#' *> parseIdent)
      <*> (L.dropWS *> parseValue)

    withoutTag = NoTag
      <$> parseValue

    parseIdent :: Parser Text
    parseIdent =
      P.takeWhile1P (Just "tag") (`elem` tagChars)

parseValue :: Parser Value
parseValue = do
  P.skipMany parseDiscard
  P.choice
    [ parseNil
    , parseBool
    , parseNumber
    , parseSymbol
    , parseCharacter
    , parseString
    , P.try parseKeyword
    , parseCollections
    ]

parseDiscard :: Parser ()
parseDiscard = do
  _ <- P.try $ L.symbol "#_"
  () <$ parseValue

parseCollections :: Parser Value
parseCollections = L.lexeme . P.try $ do
  start <- P.oneOf ['#', '{', '[', '(']
  case start of
    '#' -> do
      _ <- P.char '{'
      EDN.mkSet <$> parseItemsTill '}' parseTagged
    '{' -> do
      let pairs = (,) <$> parseTagged <*> parseTagged
      EDN.mkMap <$> parseItemsTill '}' pairs
    '[' ->
      EDN.mkVec <$> parseItemsTill ']' parseTagged
    '(' ->
      EDN.mkList <$> parseItemsTill ')' parseTagged
    _ ->
      error "assert: start is one of the collection openers"
  where
    parseItemsTill end p = do
      L.dropWS
      P.manyTill (L.dropWS *> p) (L.dropWS *> P.char end)

parseNil :: Parser Value
parseNil = Nil <$ L.symbol "nil"

parseBool :: Parser Value
parseBool = P.choice
  [ Boolean True  <$ L.symbol "true"
  , Boolean False <$ L.symbol "false"
  ]

parseSymbol :: Parser Value
parseSymbol = P.label "symbol" . L.lexeme $ do
  initial <- P.eitherP (P.char '/') (P.oneOf symbolInitialChars)
  (ns, name) <- case initial of
    Left _slash ->
      pure ("", "/")
    Right char ->
      P.try (withNS char) <|> withoutNS char
  pure $ Symbol ns name
  where
    withNS :: Char -> Parser (Text, Text)
    withNS nsInitial = do
      ns <- P.takeWhileP (Just "symbol namespace") (`elem` symbolChars)
      _ <- P.char '/'
      nameInitial <- P.oneOf symbolInitialChars
      name <- P.takeWhileP (Just "symbol name") (`elem` symbolChars)
      pure (Text.cons nsInitial ns, Text.cons nameInitial name)

    withoutNS :: Char -> Parser (Text, Text)
    withoutNS nameInitial = do
      name <- P.takeWhileP (Just "symbol name") (`elem` symbolChars)
      pure ("", Text.cons nameInitial name)

parseCharacter :: Parser Value
parseCharacter = do
  _ <- P.char '\\'
  fmap Character (P.try unicode <|> named <|> L.lexeme P.printChar)
  where
    unicode = P.label "hex-encoded unicode character" $ do
      _ <- P.char 'u'
      fmap chr L.hexadecimal

    named = P.label "whitespace character name" $ P.choice
      [ '\n' <$ L.symbol "newline"
      , '\r' <$ L.symbol "return"
      , ' '  <$ L.symbol "space"
      , '\t' <$ L.symbol "tab"
      ]

parseString :: Parser Value
parseString = L.lexeme $
  P.between (P.char '"') (P.char '"') $
    String . Text.pack <$> P.many (escaped <|> plain)
  where
    escaped :: Parser Char
    escaped = do
      _ <- P.char '\\'
      c <- P.anySingle
      pure $ case c of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        -- 'u' -> error "TODO: unicode escapes \u1234"
        _   -> c
    plain = P.anySingleBut '"'

parseNumber :: Parser Value
parseNumber = P.try parseFloating <|> P.try parseInteger
  where
    parseFloating = Floating <$> L.floating
    parseInteger = Integer <$> L.integer

parseKeyword :: Parser Value
parseKeyword = P.label "keyword" . L.lexeme $ do
  c <- P.char ':' *> P.satisfy (`elem` keywordInitialChars)
  cs <- P.takeWhileP Nothing (`elem` keywordChars)
  pure $ Keyword (Text.cons c cs)

tagChars :: [Char]
tagChars = mconcat
  [ lowerChars
  , upperChars
  , digitChars
  , "-"
  ]

keywordInitialChars :: [Char]
keywordInitialChars = mconcat
  [ lowerChars
  , upperChars
  , miscChars
  ]

keywordChars :: [Char]
keywordChars = mconcat
  [ keywordInitialChars
  , digitChars
  , "/#:"
  ]

symbolInitialChars :: [Char]
symbolInitialChars = mconcat
  [ lowerChars
  , upperChars
  , miscChars
  ]

symbolChars :: [Char]
symbolChars = mconcat
  [ symbolInitialChars
  , digitChars
  , "#:"
  ]

digitChars :: [Char]
digitChars = ['0' .. '9']

upperChars :: [Char]
upperChars = ['A' .. 'Z']

lowerChars :: [Char]
lowerChars = ['a' .. 'z']

miscChars :: [Char]
miscChars = ".*<>!?$%&=+_-"
