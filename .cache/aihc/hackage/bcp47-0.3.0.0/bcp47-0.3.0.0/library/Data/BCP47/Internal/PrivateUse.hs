{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BCP47.Internal.PrivateUse
  ( PrivateUse (PrivateUse)
  , privateUseFromText
  , privateUseToText
  , privateUseP
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.BCP47.Internal.Arbitrary
  ( Arbitrary
  , alphaNumString
  , arbitrary
  , choose
  )
import Data.BCP47.Internal.CIText (CIText)
import qualified Data.BCP47.Internal.CIText as CI
import Data.BCP47.Internal.Parser (asciiLetterDigit, complete)
import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count', parse, some)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Private Use subtags
--
-- Private use subtags are used to indicate distinctions in language
-- that are important in a given context by private agreement.
newtype PrivateUse = PrivateUse {unPrivateUse :: CIText}
  deriving stock (Show, Eq, Ord)

privateUseToText :: PrivateUse -> Text
privateUseToText = CI.original . unPrivateUse

instance Arbitrary PrivateUse where
  arbitrary = do
    len <- choose (1, 8)
    chars <- alphaNumString len
    pure . PrivateUse $ CI.pack chars

-- | Parse a 'PrivateUse' subtag from 'Text'
privateUseFromText :: Text -> Either Text (Set PrivateUse)
privateUseFromText =
  first (pack . errorBundlePretty) . parse privateUseP "privateUseFromText"

-- | BCP-47 private use parser
--
-- @@
-- privateuse    = "x" 1*("-" (1*8alphanum))
-- @@
privateUseP :: Parsec Void Text (Set PrivateUse)
privateUseP = complete $ do
  void $ char 'x' <|> char 'X'
  rest <- some (char '-' *> count' 1 8 asciiLetterDigit)
  pure $ Set.fromList $ PrivateUse . CI.pack <$> rest
