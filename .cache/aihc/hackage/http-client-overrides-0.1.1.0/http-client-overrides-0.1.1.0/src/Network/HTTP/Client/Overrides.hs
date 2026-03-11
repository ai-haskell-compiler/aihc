{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Client.Overrides (
    -- ** HTTP client overrides
      withHttpClientOverrides
    , withHttpClientOverridesThrow
    , withHttpClientOverridesFile
    , httpClientOverrides
    , parseConfigFile
    -- ** Types
    , ConfigFile(..)
    , Config(..)
    , LogOptions(..)
    , LogFormat(..)
    , RequestOverride(..)
    , URL(..)
    ) where

import Data.Either (either)
import Data.List (find)
import GHC.Stack (HasCallStack)
import System.Environment (lookupEnv)

import Data.Yaml (ParseException, decodeFileEither, prettyPrintParseException)
import Network.HTTP.Client (ManagerSettings, Response, Request)
import qualified Network.HTTP.Client as HTTP

import Network.HTTP.Client.Overrides.Internal.Logger
import Network.HTTP.Client.Overrides.Internal.RequestOverrides
import Network.HTTP.Client.Overrides.Internal.Types

------------------------
-- HTTP client overrides
------------------------

-- | If the @HTTP_CLIENT_OVERRIDES@ environment variable is set, this function
-- reads the specified file as a 'ConfigFile' and applies the overrides to
-- 'ManagerSettings'.
withHttpClientOverrides :: HasCallStack => ManagerSettings -> IO (Either ParseException ManagerSettings)
withHttpClientOverrides manager = do
    filepath <- lookupEnv "HTTP_CLIENT_OVERRIDES"
    case filepath of
        Nothing ->
            return $ return manager
        Just f -> do
            configFile <- parseConfigFile f
            return $ fmap (\x -> withHttpClientOverridesFile x manager) configFile

-- | If the @HTTP_CLIENT_OVERRIDES@ environment variable is set, this function
-- reads the specified file as a 'ConfigFile' and applies the overrides to
-- 'ManagerSettings'. Throws an exception if the config file can't be parsed.
withHttpClientOverridesThrow :: HasCallStack => ManagerSettings -> IO ManagerSettings
withHttpClientOverridesThrow manager = do
    result <- withHttpClientOverrides manager
    return . either parseError id $ result
  where
    parseError e = error $ errorMsg ++ prettyPrintParseException e
    errorMsg = "Failed to parse HTTP client overrides config: "

-- | Parses a file as a 'ConfigFile'. Use this function if you need control
-- over how the config file is loaded.
parseConfigFile :: FilePath -> IO (Either ParseException ConfigFile)
parseConfigFile filepath = decodeFileEither filepath

-- | Applies the overrides from a 'ConfigFile' to 'ManagerSettings'.
withHttpClientOverridesFile :: ConfigFile -> ManagerSettings -> ManagerSettings
withHttpClientOverridesFile configFile = case configFile of
    V1 config -> httpClientOverrides config

-- | Overrides 'ManagerSettings' using an HTTP client override 'Config'. Use
-- this function if you want to define overrides directly in your source code
-- rather than from a config file.
httpClientOverrides :: Config -> ManagerSettings -> ManagerSettings
httpClientOverrides config manager = manager
    { HTTP.managerModifyRequest = modifyRequest config
    , HTTP.managerModifyResponse = modifyResponse config
    }

-------------------
-- Helper functions
-------------------

modifyRequest :: Config -> Request -> IO Request
modifyRequest config r = do
    r' <- maybe (return r) overriddenRequest $ matchRequest config r
    logRequest config r'
    return r'
  where
    overriddenRequest requestOverride = do
        let r' = overrideRequest r requestOverride
        logRequestOverride config requestOverride r r'
        return r'

matchRequest :: Config -> Request -> Maybe RequestOverride
matchRequest config request =
    find (\x -> request `matches` match x) $ requestOverrides config

modifyResponse :: Config -> Response body -> IO (Response body)
modifyResponse config r = do
    logResponse config r
    return r
