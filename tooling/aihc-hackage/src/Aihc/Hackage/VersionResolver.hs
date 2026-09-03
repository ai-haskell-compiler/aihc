{-# LANGUAGE OverloadedStrings #-}

-- | Resolve package versions from Hackage.
module Aihc.Hackage.VersionResolver
  ( getLatestVersion,
    parsePreferredVersions,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (mapMaybe)
import Distribution.Package (packageId, pkgVersion)
import Distribution.PackageDescription (packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Version (Version)
import Network.HTTP.Client (HttpException, Manager, Request (requestHeaders, responseTimeout), httpLbs, newManager, parseRequest, responseBody, responseStatus, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Fetch the latest non-deprecated version of a package from Hackage.
--
-- Hackage publishes the set of preferred (non-deprecated) versions for a
-- package; the newest of those is used when available. If that metadata cannot
-- be fetched or is empty, this falls back to the version recorded in the
-- package's latest .cabal file, which may be a deprecated one.
getLatestVersion :: Maybe Manager -> String -> IO (Either String String)
getLatestVersion mManager packageName = do
  manager <- case mManager of
    Just m -> pure m
    Nothing -> newManager tlsManagerSettings
  preferred <- latestPreferredVersion manager packageName
  case preferred of
    Just version -> pure (Right (prettyShow version))
    Nothing -> latestUploadedVersion manager packageName

-- | Ask Hackage for the newest version that has not been deprecated.
--
-- Returns 'Nothing' when the metadata is unavailable so the caller can fall
-- back to the latest uploaded version.
latestPreferredVersion :: Manager -> String -> IO (Maybe Version)
latestPreferredVersion manager packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/preferred"
  fetchResult <- try $ do
    request <- parseRequest url
    let jsonRequest = request {requestHeaders = ("Accept", "application/json") : requestHeaders request}
    fetchUrl manager jsonRequest
  pure $ case fetchResult of
    Left err -> ignoreFailure err
    Right body ->
      case parsePreferredVersions (LBS.toStrict body) of
        [] -> Nothing
        versions -> Just (maximum versions)
  where
    ignoreFailure :: SomeException -> Maybe Version
    ignoreFailure _ = Nothing

-- | Fetch the version recorded in a package's latest .cabal file.
latestUploadedVersion :: Manager -> String -> IO (Either String String)
latestUploadedVersion manager packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  requestResult <- try (parseRequest url)
  case requestResult of
    Left err -> pure (Left ("Failed to build Hackage request: " ++ displayException (err :: HttpException)))
    Right request -> do
      fetchResult <- try (fetchUrl manager request)
      case fetchResult of
        Left err -> pure (Left ("Failed to fetch package metadata from Hackage: " ++ displayException (err :: HttpException)))
        Right cabalBytes ->
          case runParseResult (parseGenericPackageDescription (LBS.toStrict cabalBytes :: BS.ByteString)) of
            (_, Left (_, errs)) -> pure (Left ("Failed to parse Hackage cabal file: " ++ show errs))
            (_, Right gpd) ->
              let ver = pkgVersion (packageId (packageDescription gpd))
               in pure (Right (prettyShow ver))

-- | Extract the @normal-version@ entries from Hackage's preferred-version JSON.
--
-- Unparseable payloads yield an empty list rather than an error.
parsePreferredVersions :: BS.ByteString -> [Version]
parsePreferredVersions body =
  case BS.breakSubstring "\"normal-version\"" body of
    (_, rest)
      | BS.null rest -> []
      | otherwise ->
          case BSC.break (== '[') rest of
            (_, afterOpen)
              | BS.null afterOpen -> []
              | otherwise ->
                  let (array, _) = BSC.break (== ']') (BS.drop 1 afterOpen)
                   in mapMaybe (simpleParsec . BSC.unpack) (quotedStrings array)

-- | Split a JSON array body into its quoted string elements.
quotedStrings :: BS.ByteString -> [BS.ByteString]
quotedStrings input =
  case BSC.break (== '"') input of
    (_, rest)
      | BS.null rest -> []
      | otherwise ->
          let (value, remainder) = BSC.break (== '"') (BS.drop 1 rest)
           in value : quotedStrings (BS.drop 1 remainder)

-- | Fetch a URL from Hackage with a 30-second timeout.
fetchUrl :: Manager -> Request -> IO LBS.ByteString
fetchUrl manager request = do
  let request' = request {responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)}
  response <- httpLbs request' manager
  let status = statusCode (responseStatus response)
  if status >= 200 && status < 300
    then pure (responseBody response)
    else ioError (userError ("HTTP " ++ show status ++ " for " ++ show request))
