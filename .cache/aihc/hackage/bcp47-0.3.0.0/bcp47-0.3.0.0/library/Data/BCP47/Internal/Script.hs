{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Script
  ( Script (Script)
  , scriptFromText
  , scriptToText
  , scriptP
  )
where

import Data.BCP47.Internal.Arbitrary (Arbitrary, alphaString, arbitrary)
import Data.BCP47.Internal.CIText (CIText)
import qualified Data.BCP47.Internal.CIText as CI
import Data.BCP47.Internal.Parser (asciiLetter, complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Script subtags
--
-- Script subtags are used to indicate the script or writing system
-- variations that distinguish the written forms of a language or its
-- dialects.
newtype Script = Script {unScript :: CIText}
  deriving stock (Show, Eq, Ord)

scriptToText :: Script -> Text
scriptToText = CI.original . unScript

instance Arbitrary Script where
  arbitrary = Script . CI.pack <$> alphaString 4

-- | Parse a 'Script' subtag from 'Text'
scriptFromText :: Text -> Either Text Script
scriptFromText =
  first (pack . errorBundlePretty) . parse scriptP "scriptFromText"

-- | BCP-47 script parser
--
-- @@
--  script        = 4ALPHA              ; ISO 15924 code
-- @@
scriptP :: Parsec Void Text Script
scriptP = complete $ Script . CI.pack <$> count 4 asciiLetter
