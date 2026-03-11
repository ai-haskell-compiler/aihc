-- |
--
-- Module      : Network.URI.Template.Internal.Parse
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.Parse
  ( -- * Parsing
    Parser
  , parse
  , parseOpt

    -- * Errors
  , ParseError
  , errorBundlePretty

    -- * Re-exports
  , module Text.Megaparsec
  , module Text.Megaparsec.Char

    -- * Extensions
  , quoted
  , restOfLine
  ) where

import Prelude

import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError, errorBundlePretty, parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

errorBundlePretty :: ParseError -> String
errorBundlePretty = Megaparsec.errorBundlePretty

parse :: Parser a -> Text -> Either ParseError a
parse p = Megaparsec.parse p "<uri>"

parseOpt :: Parser a -> String -> Either String a
parseOpt p s = case Megaparsec.parse p "<input>" $ pack s of
  Left err ->
    Left
      $ unlines
        [ "Unable to parse option"
        , "input: " <> s
        , "error:"
        , Megaparsec.errorBundlePretty err
        ]
  Right a -> Right a

-- | NOTE: doesn't handle escaping
quoted :: Parser Text
quoted = pack <$> (char '"' *> manyTill anySingle (char '"'))

restOfLine :: Parser Text
restOfLine = pack <$> manyTill anySingle (lookAhead $ void eol <|> eof)
