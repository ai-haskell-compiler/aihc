{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{- |
Module      : Text.GridTable
Copyright   : © 2022-2023 Posit Software, PBC
License     : MIT
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Parse reStructuredText-style grid tables.
-}

module Text.GridTable.Parse
  ( gridTable
  , tableLine
  ) where

import Prelude hiding (lines)
import Data.Text (Text)
import Text.GridTable.ArrayTable
import Text.GridTable.Trace (traceLines)
import Text.Parsec
import qualified Data.Text as T

-- | Parses a grid table.
gridTable :: Stream s m Char => ParsecT s u m (ArrayTable [Text])
gridTable = try $ do
  firstLine <- (:) <$> char '+'
                   <*> (mconcat <$> many1 (gridPart '-'))
                   <* skipSpaces
                   <* newline
  lines <- many1 tableLine
  case traceLines (T.pack firstLine : lines) of
    Nothing -> fail "tracing failed"
    Just gt -> return gt

skipSpaces :: Stream s m Char => ParsecT s u m ()
skipSpaces = skipMany (satisfy $ \c -> c == '\t' || c == ' ')

-- | Parses a line that's part of a table. The line must start with
-- either a plus @+@ or a pipe @|@.
tableLine :: Stream s m Char
          => ParsecT s u m Text
tableLine = try $ do
  let borderChar = char '+' <|> char '|'
  firstChar <- borderChar
  rest <- manyTill (noneOf "\n\r") newline
  return $ T.stripEnd $ T.pack (firstChar : rest)

gridPart :: Stream s m Char
         => Char -> ParsecT s u m String
gridPart ch = do
  leftColon <- option id ((:) <$> char ':')
  dashes <- many1 (char ch)
  rightColon <- option id ((:) <$> char ':')
  plus <- char '+'
  return . leftColon . (dashes ++) . rightColon $ [plus]
