{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ByteString.Aeson.Orphans where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSON LBS.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode . LBS.toStrict

instance FromJSON LBS.ByteString where
    parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSONKey ByteString
instance FromJSONKey ByteString
