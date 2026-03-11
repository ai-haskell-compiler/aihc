{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module System.CloudFoundry.Environment.Internal.VcapApplicationDecoder
  ( VcapApplication(..)
  , decode
  ) where

import GHC.Generics

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import System.CloudFoundry.Environment.Internal.Types

data VcapApplication = VcapApplication
  { appId           :: String
  , applicationUris :: [String]
  , cfApi           :: String
  , host            :: String
  , instanceId      :: String
  , index           :: Int
  , limits          :: Limits
  , appName         :: String
  -- , port            :: Int
  , spaceId         :: String
  , spaceName       :: String
  , version         :: String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON VcapApplication where
  parseJSON = Aeson.withObject "VcapApplication" $ \o -> do
    appId <- o .: "application_id"
    applicationUris <- o .: "application_uris"
    cfApi <- o .: "cf_api"
    host <- o .: "host"
    instanceId <- o .: "instance_id"
    index <- o .: "instance_index"
    limits <- o .: "limits"
    appName <- o .: "name"
    spaceId <- o .: "space_id"
    spaceName <- o .: "space_name"
    version <- o .: "version"
    return VcapApplication{..}

decode :: String -> Either String VcapApplication
decode = Aeson.eitherDecode . LazyByteString.pack
