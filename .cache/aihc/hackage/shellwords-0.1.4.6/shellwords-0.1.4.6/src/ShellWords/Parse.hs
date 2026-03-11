{-# LANGUAGE OverloadedStrings #-}

module ShellWords.Parse
  ( -- * Splitting shell words
    parse
  , parseText

    -- * Low-level parser
  , Parser
  , runParser
  , parser
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.Char
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Compat hiding (parse, runParser)

type Parser = Parsec Void String

parse :: String -> Either String [String]
parse = runParser parser

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . Megaparsec.parse (p <* eof) "<input>"

-- | Parse and return @'Text'@ values
parseText :: Text -> Either String [Text]
parseText = fmap (map pack) . parse . unpack

parser :: Parser [String]
parser = space *> shellword `sepEndBy1` space1 <* space

shellword :: Parser String
shellword = fmap concat $ some $ bare <|> quoted

-- | A plain value, here till an (unescaped) space or quote
bare :: Parser String
bare = some go
 where
  go =
    escaped '\\'
      <|> escapedSpace
      <|> escapedAnyOf (reserved <> quotes)
      <|> satisfy ((&&) <$> not . isSpace <*> (`notElem` (reserved <> quotes)))
      <?> "non white space / non reserved character / non quote"

-- | A balanced, single- or double-quoted string
quoted :: Parser String
quoted = do
  q <- oneOf quotes
  manyTill (escaped '\\' <|> escaped q <|> anyToken) $ char q

escaped :: Char -> Parser Char
escaped c = c <$ (escapedSatisfy (== c) <?> "escaped" <> show c)

escapedSpace :: Parser Char
escapedSpace = escapedSatisfy isSpace <?> "escaped white space"

escapedAnyOf :: [Char] -> Parser Char
escapedAnyOf cs = escapedSatisfy (`elem` cs) <?> "escaped one of " <> cs

escapedSatisfy :: (Char -> Bool) -> Parser Char
escapedSatisfy p = try $ string "\\" *> satisfy p

anyToken :: Parser Char
anyToken = satisfy $ const True

reserved :: [Char]
reserved = "();"

quotes :: [Char]
quotes = "\'\""
