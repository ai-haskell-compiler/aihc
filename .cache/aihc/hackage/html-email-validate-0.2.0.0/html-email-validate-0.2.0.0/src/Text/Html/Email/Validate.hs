{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Text.Html.Email.Validate
    ( -- * Validating
      isValidEmail
      -- * Parsing
    , EmailAddress(..)
    , emailToText
    , parseEmail
    , emailParser
    ) where

import           Control.Applicative
import           Control.Monad (when)
import           Data.Either (isRight)
import           Data.Text (Text, intercalate)
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.Monoid ((<>))
import qualified Text.Read as Read
import           Data.Data (Data, Typeable)
import           GHC.Generics (Generic)

-- | Represents an email address
data EmailAddress = EmailAddress { localPart  :: Text
                                 , domainPart :: Text 
                                 } deriving (Eq, Ord, Data, Typeable, Generic)

instance Show EmailAddress where
    show = T.unpack . emailToText

instance Read EmailAddress where
    readListPrec = Read.readListPrecDefault
    readPrec = Read.parens $ do
        text <- Read.readPrec
        either (const Read.pfail) return $ parseOnly emailParser text

-- | Convert to text.
--
--   >>> emailToText $ EmailAddress "name" "example.com"
--   "name@example.com
emailToText :: EmailAddress -> Text
emailToText EmailAddress{..} = localPart <> T.singleton '@' <> domainPart

-- | Validates given email. Email shouldn't have trailing or preceding spaces
--  
--   >>> :set -XOverloadedStrings
--   >>> isValidEmail "name@example.com"
--   True
--   >>> isValidEmail "name@example..com"
--   False
isValidEmail :: Text -> Bool
isValidEmail = isRight . parseEmail

-- | Parce an email. Error messages aren't very helpful.
parseEmail :: Text -> Either String EmailAddress
parseEmail = parseOnly emailParser

-- | Attoparsec parser.
emailParser :: Parser EmailAddress
emailParser = EmailAddress <$> (local <* char '@') 
                           <*> (domain <* endOfInput)
    
local :: Parser Text
local = takeWhile1 (inClass "A-Za-z0-9!#$%&'*+/=?^_`{|}~.-")

domain :: Parser Text
domain = intercalate "." <$> label `sepBy1` char '.'

label :: Parser Text
label = do
    lbl <- intercalate "-" <$> takeWhile1 (inClass "A-Za-z0-9") `sepBy1` char '-'
    when (T.length lbl > 63) $ fail "Label is too long"
    return lbl
