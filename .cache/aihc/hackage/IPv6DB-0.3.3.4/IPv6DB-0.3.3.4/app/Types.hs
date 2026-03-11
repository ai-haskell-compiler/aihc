{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types where

import           Data.Aeson           as A
import qualified Data.ByteString      as BS
import           Data.Text.Encoding
import qualified Data.Vector          as V
import           Database.Redis
import           Prelude              hiding (error)

import           Network.IPv6DB.Types

newtype Env = Env { redisConn :: Connection }

data RedisResponse
  = RedisOk
  | RedisError
      { entry :: !Entry
      , error :: !BS.ByteString
      }
   deriving (Eq, Show)

instance ToJSON RedisResponse where
  toJSON RedisError{entry=Entry{..},..} =
    object
      [ "list"    .= list
      , "address" .= address
      , "error"   .= decodeUtf8 error
      ]
  toJSON _ = Null

newtype RedisErrors = RedisErrors [RedisResponse] deriving (Eq, Show)

instance ToJSON RedisErrors where
  toJSON (RedisErrors rrs) =
    object
      [ ("errors", Array $ V.fromList $ toJSON <$> filter (/= RedisOk) rrs) ]

