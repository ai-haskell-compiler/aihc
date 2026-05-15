{-# LANGUAGE ScopedTypeVariables #-}

-- | Hackage package index fetching and parsing.
module Aihc.Hackage.Index
  ( HackageIndexMode (..),
    loadHackageIndex,
    loadHackageIndexUpdatedSince,
    parseHackageIndex,
    parseHackageIndexUpdatedSince,
  )
where

import Aihc.Hackage.Cache (hackageIndexCacheFile)
import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, displayException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import Network.HTTP.Client (Manager, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.FilePath.Posix (splitDirectories)

-- | Controls whether the cached index may be refreshed from Hackage.
data HackageIndexMode
  = UseCachedHackageIndex
  | UpdateHackageIndex
  | OfflineHackageIndex
  deriving (Eq, Show)

-- | Load Hackage's package index.
--
-- Existing cached data is reused unless 'UpdateHackageIndex' is requested.
-- If no cache exists, the index is fetched unless 'OfflineHackageIndex' is
-- requested.
loadHackageIndex :: Maybe Manager -> HackageIndexMode -> IO (Either String [PackageSpec])
loadHackageIndex mManager mode =
  fmap (>>= parseHackageIndex) (loadHackageIndexBytes mManager mode)

-- | Load Hackage's package index, keeping only packages whose latest version
-- was uploaded at or after the given Unix timestamp.
loadHackageIndexUpdatedSince :: Maybe Manager -> HackageIndexMode -> Int64 -> IO (Either String [PackageSpec])
loadHackageIndexUpdatedSince mManager mode cutoff =
  fmap (>>= parseHackageIndexUpdatedSince cutoff) (loadHackageIndexBytes mManager mode)

loadHackageIndexBytes :: Maybe Manager -> HackageIndexMode -> IO (Either String LBS.ByteString)
loadHackageIndexBytes mManager mode = do
  cacheFile <- hackageIndexCacheFile
  hasCache <- doesFileExist cacheFile
  case (mode, hasCache) of
    (OfflineHackageIndex, False) ->
      pure (Left "Hackage index missing from cache in offline mode")
    (UseCachedHackageIndex, True) ->
      Right <$> LBS.readFile cacheFile
    (OfflineHackageIndex, True) ->
      Right <$> LBS.readFile cacheFile
    (UpdateHackageIndex, _) ->
      fetchAndCache cacheFile
    (UseCachedHackageIndex, False) ->
      fetchAndCache cacheFile
  where
    fetchAndCache cacheFile = do
      manager <- maybe (newManager tlsManagerSettings) pure mManager
      fetched <- httpGetLBS manager "https://hackage.haskell.org/01-index.tar.gz"
      case fetched of
        Left err -> pure (Left err)
        Right bytes -> do
          LBS.writeFile cacheFile bytes
          pure (Right bytes)

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions.
parseHackageIndex :: LBS.ByteString -> Either String [PackageSpec]
parseHackageIndex =
  parseHackageIndexWith (const True)

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions
-- whose latest package entry was uploaded at or after the given Unix timestamp.
parseHackageIndexUpdatedSince :: Int64 -> LBS.ByteString -> Either String [PackageSpec]
parseHackageIndexUpdatedSince cutoff =
  parseHackageIndexWith (\(_, uploadedAt) -> uploadedAt >= cutoff)

parseHackageIndexWith :: ((Version, Int64) -> Bool) -> LBS.ByteString -> Either String [PackageSpec]
parseHackageIndexWith keep bytes =
  case collectEntries Map.empty (Tar.read (GZip.decompress bytes)) of
    Left err -> Left err
    Right packages
      | Map.null packages -> Left "No package versions found in Hackage index"
      | otherwise ->
          Right
            [ PackageSpec name (prettyShow version)
            | (name, (version, uploadedAt)) <- Map.toAscList packages,
              keep (version, uploadedAt)
            ]
  where
    collectEntry packages entry =
      case packageVersionFromEntryPath (Tar.entryPath entry) of
        Nothing -> packages
        Just (name, version) ->
          Map.insertWith newerVersion name (version, Tar.entryTime entry) packages

    newerVersion new old =
      if fst new > fst old then new else old

    collectEntries packages (Tar.Next entry rest) =
      collectEntries (collectEntry packages entry) rest
    collectEntries packages Tar.Done = Right packages
    collectEntries _ (Tar.Fail err) = Left (show err)

packageVersionFromEntryPath :: FilePath -> Maybe (String, Version)
packageVersionFromEntryPath path =
  case splitDirectories path of
    [name, rawVersion, cabalFile]
      | cabalFile == name ++ ".cabal",
        ".cabal" `isSuffixOf` cabalFile,
        Just version <- simpleParsec rawVersion ->
          Just (name, version)
    _ -> Nothing

httpGetLBS :: Manager -> String -> IO (Either String LBS.ByteString)
httpGetLBS manager url = do
  result <- try $ do
    request <- parseRequest url
    response <- httpLbs request manager
    let status = statusCode (responseStatus response)
    if status >= 200 && status < 300
      then pure (Right (responseBody response))
      else pure (Left ("HTTP " ++ show status ++ " for " ++ url))
  case result of
    Left (err :: SomeException) -> pure (Left (displayException err))
    Right r -> pure r
