-- | This module contains the 'Prefixes' type, which is used to store environment variable name prefix mappings, and its typeclass instances.
module RichEnv.Types.Prefixes (Prefixes (Prefixes, unPrefixes), fromList) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A list of key-value pairs representing environment variable name prefix mappings. The internal representation is a 'HashMap Text [Text]', where the key is the final prefix and the value is the list of prefixes that will be replaced.
newtype Prefixes = Prefixes {unPrefixes :: HM.HashMap Text [Text]}
  deriving stock (Eq, Show, Generic)

instance FromJSON Prefixes where
  parseJSON :: Value -> Parser Prefixes
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Prefixes where
  toJSON :: Prefixes -> Value
  toJSON = toJSON . unPrefixes

instance Semigroup Prefixes where
  (<>) :: Prefixes -> Prefixes -> Prefixes
  (<>) (Prefixes a) (Prefixes b) = Prefixes (a <> b)

instance Monoid Prefixes where
  mempty :: Prefixes
  mempty = Prefixes mempty

-- | Build a 'Prefixes' object from a list of key-value pairs.
fromList :: [(Text, [Text])] -> Prefixes
fromList = Prefixes . HM.fromList
