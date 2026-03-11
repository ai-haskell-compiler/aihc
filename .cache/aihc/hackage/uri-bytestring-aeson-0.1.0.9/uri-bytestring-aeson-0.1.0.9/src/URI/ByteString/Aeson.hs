{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI.ByteString.Aeson () where

import           Data.Aeson
import qualified Data.ByteString as BS
import           URI.ByteString
import           Data.Text.Encoding
import           Control.Monad

class ParseJSONURI a where
  parseJSONURI :: BS.ByteString -> Either URIParseError (URIRef a)

instance ParseJSONURI Absolute where
  parseJSONURI = parseURI laxURIParserOptions

instance ParseJSONURI Relative where
  parseJSONURI = parseRelativeRef laxURIParserOptions

instance ParseJSONURI a => FromJSON (URIRef a) where
  parseJSON = parseJSON >=> either (fail . show) pure . parseJSONURI . encodeUtf8

instance ToJSON (URIRef f) where
  toJSON = toJSON . decodeUtf8 . serializeURIRef'
