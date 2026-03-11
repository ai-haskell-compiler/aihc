{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.NarInfo.Parser
  ( -- * Parser
    parseNarInfo
  , parseNarInfoWith
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser)
import Nix.NarInfo.Types

import qualified Control.Applicative
import qualified Data.Char
import qualified Data.Set
import qualified Data.Text
import qualified Data.Attoparsec.Text

parseNarInfo :: Parser (NarInfo FilePath Text Text)
parseNarInfo = parseNarInfoWith pathParse textParse hashParse
  where
    textParse = Data.Attoparsec.Text.takeWhile1 (not . Data.Char.isSpace)
    pathParse _hasPrefix = Data.Text.unpack <$> textParse
    hashParse = textParse

parseNarInfoWith :: (Ord fp)
                 => (Bool -> Parser fp) -- True when path prefix is present
                 -> Parser txt
                 -> Parser hash
                 -> Parser (NarInfo fp txt hash)
parseNarInfoWith pathParser textParser hashParser = do
  storePath   <- keyPath "StorePath"
  url         <- key     "URL"
  compression <- key     "Compression"
  fileHash    <- keyHash "FileHash"
  fileSize    <- keyNum  "FileSize"
  narHash     <- keyHash "NarHash"
  narSize     <- keyNum  "NarSize"

  references  <- Data.Set.fromList <$> (parseKey "References" $
    (pathParser False) `Data.Attoparsec.Text.sepBy` Data.Attoparsec.Text.char ' ')

  deriver     <- optKey "Deriver"
  system      <- optKey "System"
  sig         <- optKey "Sig"
  ca          <- optKey "Ca"

  return $ NarInfo {..}
  where
    parseKey key parser = do
      Data.Attoparsec.Text.string key
      Data.Attoparsec.Text.string ": "
      out <- parser
      Data.Attoparsec.Text.char '\n'
      return out

    key = flip parseKey textParser
    optKey = Control.Applicative.optional . key
    keyNum x = parseKey x Data.Attoparsec.Text.decimal
    keyPath x = parseKey x (pathParser True)
    keyHash x = parseKey x hashParser
