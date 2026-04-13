{-# LANGUAGE OverloadedStrings #-}

-- | Resolve package versions from Hackage.
module Aihc.Hackage.VersionResolver
  ( getLatestVersion,
  )
where

import Control.Exception (displayException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Distribution.Package (packageId, pkgVersion)
import Distribution.PackageDescription (packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Network.HTTP.Client (HttpException, Manager, Request (responseTimeout), httpLbs, newManager, parseRequest, responseBody, responseStatus, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Fetch the latest version of a package from Hackage.
--
-- Downloads the .cabal file for the package and parses the version from it.
getLatestVersion :: Maybe Manager -> String -> IO (Either String String)
getLatestVersion mManager packageName = do
  manager <- case mManager of
    Just m -> pure m
    Nothing -> newManager tlsManagerSettings
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  requestResult <- try (parseRequest url)
  case requestResult of
    Left err -> pure (Left ("Failed to build Hackage request: " ++ displayException (err :: HttpException)))
    Right request -> do
      fetchResult <- try (fetchCabalFile manager request)
      case fetchResult of
        Left err -> pure (Left ("Failed to fetch package metadata from Hackage: " ++ displayException (err :: HttpException)))
        Right cabalBytes ->
          case runParseResult (parseGenericPackageDescription (LBS.toStrict cabalBytes :: BS.ByteString)) of
            (_, Left (_, errs)) -> pure (Left ("Failed to parse Hackage cabal file: " ++ show errs))
            (_, Right gpd) ->
              let ver = pkgVersion (packageId (packageDescription gpd))
               in pure (Right (prettyShow ver))

-- | Fetch a .cabal file from Hackage with a 30-second timeout.
fetchCabalFile :: Manager -> Request -> IO LBS.ByteString
fetchCabalFile manager request = do
  let request' = request {responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)}
  response <- httpLbs request' manager
  let status = statusCode (responseStatus response)
  if status >= 200 && status < 300
    then pure (responseBody response)
    else ioError (userError ("HTTP " ++ show status ++ " for " ++ show request))
