{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.Variant
  ( Variant (Variant)
  , variantFromText
  , variantToText
  , variantP
  )
where

import Control.Applicative ((<|>))
import Data.BCP47.Internal.Arbitrary
  ( Arbitrary
  , alphaNumString
  , arbitrary
  , choose
  , numChar
  , oneof
  )
import Data.BCP47.Internal.CIText (CIText)
import qualified Data.BCP47.Internal.CIText as CI
import Data.BCP47.Internal.Parser (asciiDigit, asciiLetterDigit, complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, count', parse, try)
import Text.Megaparsec.Error (errorBundlePretty)

-- | BCP-47 variant parser
--
-- @@
-- variant       = 5*8alphanum         ; registered variants
--               / (DIGIT 3alphanum)
-- @@
variantP :: Parsec Void Text Variant
variantP =
  complete $
    Variant
      . CI.pack
      <$> (try (count' 5 8 asciiLetterDigit) <|> digitPrefixed)
 where
  digitPrefixed = do
    x <- asciiDigit
    xs <- count 3 asciiLetterDigit
    pure $ x : xs

-- | Variant subtags
--
-- Variant subtags are used to indicate additional, well-recognized
-- variations that define a language or its dialects that are not
-- covered by other available subtags.
newtype Variant = Variant {unVariant :: CIText}
  deriving stock (Show, Eq, Ord)

variantToText :: Variant -> Text
variantToText = CI.original . unVariant

instance Arbitrary Variant where
  arbitrary = oneof [alphaNum, digitPrefixed]
   where
    alphaNum = do
      len <- choose (5, 8)
      chars <- alphaNumString len
      pure . Variant $ CI.pack chars
    digitPrefixed = do
      prefix <- numChar
      chars <- alphaNumString 3
      pure . Variant $ CI.pack $ prefix : chars

-- | Parse a 'Variant' subtag from 'Text'
variantFromText :: Text -> Either Text Variant
variantFromText =
  first (pack . errorBundlePretty) . parse variantP "variantFromText"
