-- | This module contains the 'Values' type, which stores environment variable names and values, and its typeclass instances.
module RichEnv.Types.Values (Values (Values, unValues), fromList) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A list of key-value pairs representing environment variables. The internal representation is a 'HashMap Text Text', where the key is the variable name and the value is the variable value.
newtype Values = Values {unValues :: HM.HashMap Text Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON Values where
  parseJSON :: Value -> Parser Values
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Values where
  toJSON :: Values -> Value
  toJSON = toJSON . unValues

instance Semigroup Values where
  (<>) :: Values -> Values -> Values
  (<>) (Values a) (Values b) = Values (a <> b)

instance Monoid Values where
  mempty :: Values
  mempty = Values mempty

-- | Build a 'Values' object from a list of key-value pairs.
fromList :: [(Text, Text)] -> Values
fromList = Values . HM.fromList
