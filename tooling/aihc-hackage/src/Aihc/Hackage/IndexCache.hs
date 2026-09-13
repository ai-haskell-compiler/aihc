{-# LANGUAGE ScopedTypeVariables #-}

-- | A cached copy of the Hackage package index, used to resolve the newest
-- non-deprecated version of a package.
--
-- Hackage serves a per-package @\/preferred@ endpoint, but asking it once per
-- package makes resolving a dependency graph a series of round trips and
-- leaves the compiler unable to resolve anything offline. The index tarball
-- carries the same information for every package at once, so this module
-- fetches it once, derives the newest preferred version of each package from
-- it, and answers later questions from that derived file.
--
-- The index is large, so the derived file is what is read on the common path.
-- The tarball is kept beside it only so a refresh can be told apart from a
-- first download.
module Aihc.Hackage.IndexCache
  ( HackageIndex,
    PreferredVersions,
    IndexOptions (..),
    defaultIndexOptions,
    newHackageIndex,
    indexPreferredVersion,
    refreshHackageIndex,
    getIndexCacheDir,
    hackageIndexUrl,
    renderPreferredVersions,
    parsePreferredVersionsCache,
  )
where

import Aihc.Hackage.Cache (getHackageCacheDir)
import Aihc.Hackage.Index (latestPreferredVersions, parsePreferredRanges)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Distribution.Pretty (prettyShow)
import Network.HTTP.Client (Manager, Request (responseTimeout), brRead, newManager, parseRequest, responseBody, responseStatus, responseTimeoutMicro, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withBinaryFile)

-- | Where the Hackage index tarball is fetched from.
hackageIndexUrl :: String
hackageIndexUrl = "https://hackage.haskell.org/01-index.tar.gz"

-- | How the index cache may be used.
data IndexOptions = IndexOptions
  { -- | Refetch the index when the derived file is older than this.
    indexMaxAge :: !NominalDiffTime,
    -- | Allow fetching the index. A cached copy is still read when this is off.
    indexAllowNetwork :: !Bool,
    indexVerbose :: !Bool,
    indexManager :: !(Maybe Manager)
  }

-- | Refetch a day-old index, over the network, with progress on stderr.
defaultIndexOptions :: IndexOptions
defaultIndexOptions =
  IndexOptions
    { indexMaxAge = 24 * 60 * 60,
      indexAllowNetwork = True,
      indexVerbose = True,
      indexManager = Nothing
    }

-- | A handle on the cached index.
--
-- The index is read on the first question asked of it and kept for the rest of
-- the process, so a build that resolves no Hackage versions never touches it.
data HackageIndex = HackageIndex
  { hackageIndexOptions :: !IndexOptions,
    hackageIndexVersions :: !(IORef (Maybe PreferredVersions))
  }

-- | Create a handle that reads the index when it is first needed.
newHackageIndex :: IndexOptions -> IO HackageIndex
newHackageIndex opts = HackageIndex opts <$> newIORef Nothing

-- | The newest non-deprecated version of a package.
indexPreferredVersion :: HackageIndex -> String -> IO (Either String String)
indexPreferredVersion index name = do
  cached <- readIORef (hackageIndexVersions index)
  versions <- case cached of
    Just versions -> pure versions
    Nothing -> do
      versions <- loadPreferredVersions (hackageIndexOptions index)
      writeIORef (hackageIndexVersions index) (Just versions)
      pure versions
  pure $ case Map.lookup (BSC.pack name) versions of
    Just version -> Right (BSC.unpack version)
    Nothing ->
      Left
        ( "Package not found in the Hackage index: "
            ++ name
            ++ "\nThe index may be out of date; remove the cache to refetch it."
        )

-- | XDG cache directory holding the index and the versions derived from it.
--
-- @~\/.cache\/aihc\/hackage-index@
getIndexCacheDir :: IO FilePath
getIndexCacheDir = do
  cacheBase <- getHackageCacheDir
  pure (cacheBase ++ "-index")

-- | Read the derived preferred versions, refetching the index when stale.
--
-- A refresh that fails falls back to a stale derived file, so a broken network
-- degrades resolution to yesterday's answers rather than failing the build.
loadPreferredVersions :: IndexOptions -> IO PreferredVersions
loadPreferredVersions opts = do
  cacheDir <- getIndexCacheDir
  let derivedFile = cacheDir </> preferredVersionsFileName
  stale <- isStale opts derivedFile
  when stale $
    if indexAllowNetwork opts
      then do
        refreshed <- try (refreshHackageIndex opts)
        case refreshed of
          Right () -> pure ()
          Left (err :: SomeException) -> do
            cached <- doesFileExist derivedFile
            if cached
              then
                hPutStrLn
                  stderr
                  ( "Warning: could not update the Hackage index, using the cached copy: "
                      ++ displayException err
                  )
              else ioError (userError ("Failed to fetch the Hackage index: " ++ displayException err))
      else do
        cached <- doesFileExist derivedFile
        unless cached $
          ioError (userError "The Hackage index is missing from the cache and the network is disabled")
  parsePreferredVersionsCache <$> BS.readFile derivedFile

-- | Whether the derived file is missing or older than the configured age.
isStale :: IndexOptions -> FilePath -> IO Bool
isStale opts derivedFile = do
  exists <- doesFileExist derivedFile
  if not exists
    then pure True
    else do
      modified <- getModificationTime derivedFile
      now <- getCurrentTime
      pure (diffUTCTime now modified > indexMaxAge opts)

-- | Fetch the Hackage index and derive the preferred version of each package.
refreshHackageIndex :: IndexOptions -> IO ()
refreshHackageIndex opts = do
  cacheDir <- getIndexCacheDir
  createDirectoryIfMissing True cacheDir
  when (indexVerbose opts) $
    hPutStrLn stderr "Updating the Hackage index..."
  manager <- case indexManager opts of
    Just m -> pure m
    Nothing -> newManager tlsManagerSettings
  request <- parseRequest hackageIndexUrl
  -- The index is a large download, so it is streamed to a file rather than
  -- held in memory, and read back from there twice.
  let indexFile = cacheDir </> indexFileName
  downloadToFile manager request {responseTimeout = responseTimeoutMicro (300 * 1000 * 1000)} indexFile
  ranges <- either (ioError . userError) pure . parsePreferredRanges =<< LBS.readFile indexFile
  versions <- either (ioError . userError) pure . latestPreferredVersions ranges =<< LBS.readFile indexFile
  let derivedFile = cacheDir </> preferredVersionsFileName
  writeFileAtomically
    derivedFile
    BS.writeFile
    (renderPreferredVersions (Map.mapKeys BSC.pack (Map.map (BSC.pack . prettyShow) versions)))
  -- Only what was derived is kept. Refreshing refetches the whole index, so
  -- keeping a gigabyte of tarball around would buy nothing until the refresh
  -- is made incremental.
  removeFile indexFile
  when (indexVerbose opts) $
    hPutStrLn stderr ("Hackage index: " ++ show (Map.size versions) ++ " packages")

-- | Stream a response body to a file, replacing whatever was there.
downloadToFile :: Manager -> Request -> FilePath -> IO ()
downloadToFile manager request path =
  withResponse request manager $ \response -> do
    let status = statusCode (responseStatus response)
    when (status < 200 || status >= 300) $
      ioError (userError ("HTTP " ++ show status ++ " for " ++ hackageIndexUrl))
    let temporary = path ++ ".tmp"
    withBinaryFile temporary WriteMode $ \handle ->
      let copyChunks = do
            chunk <- brRead (responseBody response)
            unless (BS.null chunk) $ do
              BS.hPut handle chunk
              copyChunks
       in copyChunks
    renameFile temporary path

-- | Write through a temporary file so an interrupted write leaves no
-- half-written cache behind.
writeFileAtomically :: FilePath -> (FilePath -> a -> IO ()) -> a -> IO ()
writeFileAtomically path write contents = do
  let temporary = path ++ ".tmp"
  write temporary contents
  renameFile temporary path

indexFileName :: FilePath
indexFileName = "01-index.tar.gz"

preferredVersionsFileName :: FilePath
preferredVersionsFileName = "preferred-versions.txt"

-- | The newest preferred version of each package, by package name.
--
-- The names and versions stay as bytes: the derived file holds twenty thousand
-- of each, and turning them all into 'String' to answer a handful of lookups
-- allocated two orders of magnitude more than the file is large.
type PreferredVersions = Map.Map BSC.ByteString BSC.ByteString

-- | Render the derived versions as one @name version@ line per package.
renderPreferredVersions :: PreferredVersions -> BS.ByteString
renderPreferredVersions versions =
  BSC.unlines [BSC.unwords [name, version] | (name, version) <- Map.toAscList versions]

-- | Read back what 'renderPreferredVersions' wrote, ignoring malformed lines.
parsePreferredVersionsCache :: BS.ByteString -> PreferredVersions
parsePreferredVersionsCache contents =
  Map.fromList
    [ (name, version)
    | line <- BSC.lines contents,
      [name, version] <- [BSC.words line]
    ]
