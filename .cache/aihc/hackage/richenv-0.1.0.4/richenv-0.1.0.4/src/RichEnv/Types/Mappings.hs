-- | This module contains the 'Mappings' type, which is used to store environment variable name mappings, and its typeclass instances.
module RichEnv.Types.Mappings (Mappings (Mappings, unMappings), fromList) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A list of key-value pairs representing environment variable name mappings. The internal representation is a 'HashMap Text Text', where the key is the final variable name and the value is the current one which will be replaced.
newtype Mappings = Mappings {unMappings :: HM.HashMap Text Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON Mappings where
  parseJSON :: Value -> Parser Mappings
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Mappings where
  toJSON :: Mappings -> Value
  toJSON = toJSON . unMappings

instance Semigroup Mappings where
  (<>) :: Mappings -> Mappings -> Mappings
  (<>) (Mappings a) (Mappings b) = Mappings (a <> b)

instance Monoid Mappings where
  mempty :: Mappings
  mempty = Mappings mempty

-- | Build a 'Mappings' object from a list of key-value pairs.
fromList :: [(Text, Text)] -> Mappings
fromList = Mappings . HM.fromList
