{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.UUID.Typed
  ( UUID (..),
    uuidText,
    uuidString,
    uuidASCIIBytes,
    uuidLazyASCIIBytes,
    nextRandomUUID,
    parseUUIDText,
    parseUUIDString,
    parseUUIDAsciiBytes,
    parseUUIDLazyAsciiBytes,
    jsonParseUUID,
    textJSONParseUUID,
  )
where

import Autodocodec
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Binary
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Hashable
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Validity
import Data.Validity.UUID ()
import Foreign.Storable
import GHC.Generics
import System.Random
import Web.HttpApiData

newtype UUID a
  = UUID
      { unUUID :: UUID.UUID
      }
  deriving stock (Eq, Ord, Generic, Show, Read, Data)
  deriving newtype (Storable, Binary, NFData, Hashable, Random, Validity)
  deriving (FromJSON, ToJSON) via (Autodocodec (UUID a))

-- | See 'UUID.toText'
uuidText :: UUID a -> Text
uuidText (UUID uuid) = UUID.toText uuid

-- | See 'UUID.toString'
uuidString :: UUID a -> String
uuidString (UUID uuid) = UUID.toString uuid

-- | See 'UUID.toASCIIBytes'
uuidASCIIBytes :: UUID a -> SB.ByteString
uuidASCIIBytes (UUID uuid) = UUID.toASCIIBytes uuid

-- | See 'UUID.toLazyASCIIBytes'
uuidLazyASCIIBytes :: UUID a -> LB.ByteString
uuidLazyASCIIBytes (UUID uuid) = UUID.toLazyASCIIBytes uuid

-- | See 'UUID.nextRandom'
nextRandomUUID :: MonadIO m => m (UUID a)
nextRandomUUID = liftIO $ UUID <$> UUID.nextRandom

-- | See 'UUID.fromText'
parseUUIDText :: Text -> Maybe (UUID a)
parseUUIDText = fmap UUID . UUID.fromText

-- | See 'UUID.fromString'
parseUUIDString :: String -> Maybe (UUID a)
parseUUIDString = fmap UUID . UUID.fromString

-- | See 'UUID.parseUUIDAsciiBytes'
parseUUIDAsciiBytes :: SB.ByteString -> Maybe (UUID a)
parseUUIDAsciiBytes = fmap UUID . UUID.fromASCIIBytes

-- | See 'UUID.parseUUIDLazyAsciiBytes'
parseUUIDLazyAsciiBytes :: LB.ByteString -> Maybe (UUID a)
parseUUIDLazyAsciiBytes = fmap UUID . UUID.fromLazyASCIIBytes

instance FromJSONKey (UUID a) where
  fromJSONKey = FromJSONKeyTextParser textJSONParseUUID

instance ToJSONKey (UUID a) where
  toJSONKey = toJSONKeyText (UUID.toText . unUUID)

jsonParseUUID :: Value -> Parser (UUID a)
jsonParseUUID = withText "UUID" textJSONParseUUID

instance HasCodec (UUID a) where
  codec = bimapCodec f uuidText codec
    where
      f t =
        case UUID.fromText t of
          Nothing -> Left "Invalid Text when parsing UUID"
          Just u -> Right $ UUID u

textJSONParseUUID :: Text -> Parser (UUID a)
textJSONParseUUID t =
  case UUID.fromText t of
    Nothing -> fail "Invalid Text when parsing UUID"
    Just u -> pure $ UUID u

instance FromHttpApiData (UUID a) where
  parseUrlPiece t =
    case UUID.fromText t of
      Nothing -> Left $ "Invalid UUID in Url Piece: " <> t
      Just uuid -> pure $ UUID uuid

instance ToHttpApiData (UUID a) where
  toUrlPiece (UUID uuid) = UUID.toText uuid
