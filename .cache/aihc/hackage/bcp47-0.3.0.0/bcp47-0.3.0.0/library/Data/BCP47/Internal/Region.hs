module Data.BCP47.Internal.Region
  ( Country
  , regionToText
  , regionFromText
  , regionP
  )
where

import Control.Applicative ((<|>))
import Country (Country, alphaTwoUpper, decodeAlphaTwo, decodeNumeric)
import Data.BCP47.Internal.Parser (asciiDigit, asciiLetter, complete)
import Data.Bifunctor (first)
import Data.Text (Text, pack, toUpper)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, parse, try, (<?>))
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Read (readEither)

regionToText :: Country -> Text
regionToText = alphaTwoUpper

-- | Parse a region subtag from 'Text'
--
-- >>> regionFromText $ pack "zw"
-- Right zimbabwe
--
-- >>> regionFromText $ pack "ZW"
-- Right zimbabwe
--
-- >>> regionFromText $ pack "Zw"
-- Right zimbabwe
--
-- >>> regionFromText $ pack "zW"
-- Right zimbabwe
--
-- >>> regionFromText $ pack "012"
-- Right algeria
--
-- >>> regionFromText $ pack "asdf"
-- Left "regionFromText:1:3:\n  |\n1 | asdf\n  |   ^\nunexpected 'd'\nexpecting 2 or 3 character country code\n"
regionFromText :: Text -> Either Text Country
regionFromText =
  first (pack . errorBundlePretty) . parse regionP "regionFromText"

-- | BCP-47 region parser
--
-- @@
-- region        = 2ALPHA              ; ISO 3166-1 code
--               / 3DIGIT              ; UN M.49 code
-- @@
regionP :: Parsec Void Text Country
regionP =
  try (complete asciiLetter2)
    <|> try (complete num3)
    <?> "2 or 3 character country code"
 where
  asciiLetter2 = do
    code <- pack <$> count 2 asciiLetter
    let region = decodeAlphaTwo $ toUpper code
    unwrap "Invalid 2 character country code" region

  num3 = do
    code <- count 3 asciiDigit
    region <- decodeNumeric <$> either fail pure (readEither code)
    unwrap "Invalid 3 character country code" region

  unwrap :: String -> Maybe a -> Parsec Void Text a
  unwrap message = maybe (fail message) pure
