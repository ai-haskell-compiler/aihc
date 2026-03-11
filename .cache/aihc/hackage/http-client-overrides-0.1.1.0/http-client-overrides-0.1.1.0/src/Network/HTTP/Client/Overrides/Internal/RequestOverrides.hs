{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Client.Overrides.Internal.RequestOverrides
    ( matches
    , overrideRequest
    ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP

import Network.HTTP.Client.Overrides.Internal.Types

--------------------
-- Request overrides
--------------------

matches :: Request -> URL -> Bool
request `matches` url =
       maybe True (== HTTP.secure request) (secure url)
    && maybe True (== HTTP.host request) (host url)
    && maybe True (== HTTP.port request) (port url)
    && maybe True (`BS.isPrefixOf` HTTP.path request) (path url)

overrideRequest :: Request -> RequestOverride -> Request
overrideRequest request requestOverride = request
    { HTTP.secure = fromMaybe (HTTP.secure request) $ secure url
    , HTTP.host = fromMaybe (HTTP.host request) $ host url
    , HTTP.port = fromMaybe (HTTP.port request) $ port url
    , HTTP.path = fromMaybe (HTTP.path request) $ substitutedPath
    }
  where
    url = override requestOverride
    substitutedPath :: Maybe BS.ByteString
    substitutedPath = do
      let oldPrefix = fromMaybe "/" . path $ match requestOverride
      oldSuffix <- BS.stripPrefix oldPrefix $ HTTP.path request
      newPrefix <- path url
      return $ newPrefix <> oldSuffix
