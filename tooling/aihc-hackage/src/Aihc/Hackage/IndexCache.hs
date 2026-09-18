{-# LANGUAGE ScopedTypeVariables #-}

-- | A cached copy of the Hackage package index, used to find the versions
-- of a package and read their cabal files.
--
-- Hackage serves per-package endpoints, but asking it once per package makes
-- resolving a dependency graph a series of round trips and leaves the
-- compiler unable to resolve anything offline. The index tarball carries the
-- same information for every package at once, so this module fetches it
-- once, keeps it uncompressed beside a derived table of every cabal entry,
-- and answers later questions from those two files.
--
-- The derived table is what is read on the common path: one line per cabal
-- entry naming the package, the version, the revision, and where in the
-- tarball the entry sits. A cabal file is then read from the tarball at
-- that offset, so the solver looks at the versions it tries and nothing
-- else.
module Aihc.Hackage.IndexCache
  ( HackageIndex,
    IndexOptions (..),
    defaultIndexOptions,
    newHackageIndex,
    IndexVersion (..),
    indexPackageVersions,
    indexReadCabalFile,
    indexPreferredVersion,
    indexState,
    refreshHackageIndex,
    getIndexCacheDir,
    hackageIndexUrl,

    -- * The derived table
    IndexTable (..),
    renderIndexTable,
    parseIndexTable,
    indexTableFromScan,
    indexTableVersions,
  )
where

import Aihc.Hackage.Cache (getHackageCacheDir)
import Aihc.Hackage.Index (IndexEntry (..), IndexScan (..), readIndexEntry, scanIndex)
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, withinRange)
import Network.HTTP.Client (Manager, Request (responseTimeout), brRead, newManager, parseRequest, responseBody, responseStatus, responseTimeoutMicro, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode, WriteMode), hPutStrLn, stderr, withBinaryFile)

-- | Where the Hackage index tarball is fetched from.
hackageIndexUrl :: String
hackageIndexUrl = "https://hackage.haskell.org/01-index.tar.gz"

-- | How the index cache may be used.
data IndexOptions = IndexOptions
  { -- | Refetch the index when the derived table is older than this.
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
    hackageIndexTable :: !(IORef (Maybe IndexTable))
  }

-- | Create a handle that reads the index when it is first needed.
newHackageIndex :: IndexOptions -> IO HackageIndex
newHackageIndex opts = HackageIndex opts <$> newIORef Nothing

-- | One version of a package as the index lists it.
data IndexVersion = IndexVersion
  { indexVersionVersion :: !Version,
    -- | Every revision of its cabal file, oldest first. Never empty.
    indexVersionRevisions :: ![IndexEntry],
    -- | The maintainer's @preferred-versions@ exclude this version.
    indexVersionDeprecated :: !Bool
  }
  deriving (Eq, Show)

loadTable :: HackageIndex -> IO IndexTable
loadTable index = do
  cached <- readIORef (hackageIndexTable index)
  case cached of
    Just table -> pure table
    Nothing -> do
      table <- loadIndexTable (hackageIndexOptions index)
      writeIORef (hackageIndexTable index) (Just table)
      pure table

-- | The versions of a package, newest first, or 'Nothing' for a package the
-- index does not know.
indexPackageVersions :: HackageIndex -> String -> IO (Maybe [IndexVersion])
indexPackageVersions index name = do
  table <- loadTable index
  pure (indexTableVersions table name)

-- | The cabal file of one version at one revision, or at the latest
-- revision when none is given, with the revision that was read.
indexReadCabalFile :: HackageIndex -> String -> Version -> Maybe Int -> IO (Either String (Int, BS.ByteString))
indexReadCabalFile index name version wantedRevision = do
  table <- loadTable index
  case [v | v <- concat (indexTableVersions table name), indexVersionVersion v == version] of
    [] -> pure (Left ("The Hackage index has no version " <> prettyShow version <> " of " <> name))
    indexVersion : _ ->
      let revisions = indexVersionRevisions indexVersion
          chosen = case wantedRevision of
            Nothing -> Just (last revisions)
            Just wanted -> case [entry | entry <- revisions, indexEntryRevision entry == wanted] of
              entry : _ -> Just entry
              [] -> Nothing
       in case chosen of
            Nothing ->
              pure
                ( Left
                    ( "The Hackage index has no revision "
                        <> maybe "" show wantedRevision
                        <> " of "
                        <> name
                        <> "-"
                        <> prettyShow version
                        <> "; the newest is "
                        <> show (indexEntryRevision (last revisions))
                    )
                )
            Just entry -> do
              cacheDir <- getIndexCacheDir
              contents <- withBinaryFile (cacheDir </> indexFileName) ReadMode (`readIndexEntry` entry)
              pure (Right (indexEntryRevision entry, contents))

-- | The newest non-deprecated version of a package.
indexPreferredVersion :: HackageIndex -> String -> IO (Either String String)
indexPreferredVersion index name = do
  versions <- indexPackageVersions index name
  pure $ case versions of
    Nothing ->
      Left
        ( "Package not found in the Hackage index: "
            ++ name
            ++ "\nThe index may be out of date; remove the cache to refetch it."
        )
    Just candidates ->
      case [v | v <- candidates, not (indexVersionDeprecated v)] <> candidates of
        version : _ -> Right (prettyShow (indexVersionVersion version))
        [] -> Left ("The Hackage index lists no version of " ++ name)

-- | The moment the cached index describes, in Unix seconds.
indexState :: HackageIndex -> IO Int64
indexState index = indexTableState <$> loadTable index

-- | XDG cache directory holding the index and the table derived from it.
--
-- @~\/.cache\/aihc\/hackage-index@
getIndexCacheDir :: IO FilePath
getIndexCacheDir = do
  cacheBase <- getHackageCacheDir
  pure (cacheBase ++ "-index")

-- | Read the derived table, refetching the index when stale.
--
-- A refresh that fails falls back to a stale table, so a broken network
-- degrades resolution to yesterday's answers rather than failing the build.
loadIndexTable :: IndexOptions -> IO IndexTable
loadIndexTable opts = do
  cacheDir <- getIndexCacheDir
  let tableFile = cacheDir </> indexTableFileName
  stale <- isStale opts cacheDir
  when stale $
    if indexAllowNetwork opts
      then do
        refreshed <- try (refreshHackageIndex opts)
        case refreshed of
          Right () -> pure ()
          Left (err :: SomeException) -> do
            cached <- cacheComplete cacheDir
            if cached
              then
                hPutStrLn
                  stderr
                  ( "Warning: could not update the Hackage index, using the cached copy: "
                      ++ displayException err
                  )
              else ioError (userError ("Failed to fetch the Hackage index: " ++ displayException err))
      else do
        cached <- cacheComplete cacheDir
        unless cached $
          ioError (userError "The Hackage index is missing from the cache and the network is disabled")
  contents <- BS.readFile tableFile
  either (ioError . userError . (("Invalid Hackage index table " <> tableFile <> ": ") <>)) pure (parseIndexTable contents)

-- | Both files of the cache are present.
cacheComplete :: FilePath -> IO Bool
cacheComplete cacheDir = do
  table <- doesFileExist (cacheDir </> indexTableFileName)
  tarball <- doesFileExist (cacheDir </> indexFileName)
  pure (table && tarball)

-- | Whether the cache is incomplete or older than the configured age.
isStale :: IndexOptions -> FilePath -> IO Bool
isStale opts cacheDir = do
  complete <- cacheComplete cacheDir
  if not complete
    then pure True
    else do
      modified <- getModificationTime (cacheDir </> indexTableFileName)
      now <- getCurrentTime
      pure (diffUTCTime now modified > indexMaxAge opts)

-- | Fetch the Hackage index, keep it uncompressed, and derive the table of
-- its cabal entries.
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
  -- held in memory, then decompressed file to file. The uncompressed
  -- tarball is what stays: the cabal files are read from it by offset.
  let compressedFile = cacheDir </> indexFileName <> ".gz"
      indexFile = cacheDir </> indexFileName
  downloadToFile manager request {responseTimeout = responseTimeoutMicro (300 * 1000 * 1000)} compressedFile
  writeFileAtomically indexFile (\path contents -> LBS.writeFile path (GZip.decompress contents)) =<< LBS.readFile compressedFile
  removeFile compressedFile
  scan <- either (ioError . userError) pure . scanIndex =<< LBS.readFile indexFile
  let table = indexTableFromScan scan
  writeFileAtomically (cacheDir </> indexTableFileName) BS.writeFile (renderIndexTable table)
  -- The derived file of the previous cache layout, which held only the
  -- newest preferred version of each package.
  legacy <- doesFileExist (cacheDir </> "preferred-versions.txt")
  when legacy $ removeFile (cacheDir </> "preferred-versions.txt")
  when (indexVerbose opts) $
    hPutStrLn stderr ("Hackage index: " ++ show (Map.size (indexTableEntries table)) ++ " packages")

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
indexFileName = "01-index.tar"

indexTableFileName :: FilePath
indexTableFileName = "index.txt"

-- | The derived table: every cabal entry of every package, and the ranges
-- the maintainers prefer.
--
-- The names and versions stay as bytes: the table holds hundreds of
-- thousands of them, and turning them all into 'String' to answer a handful
-- of lookups allocated two orders of magnitude more than the file is large.
-- A version is parsed when its package is asked for.
data IndexTable = IndexTable
  { -- | By package name: the cabal entries in index order, each as the
    -- version text, the revision, and the block offset.
    indexTableEntries :: !(Map.Map BSC.ByteString [(BSC.ByteString, Int, Word)]),
    -- | By package name: the preferred range as its text.
    indexTableRanges :: !(Map.Map BSC.ByteString BSC.ByteString),
    indexTableState :: !Int64
  }
  deriving (Eq, Show)

indexTableFromScan :: IndexScan -> IndexTable
indexTableFromScan scan =
  IndexTable
    { indexTableEntries =
        Map.fromList
          [ (BSC.pack name, [(BSC.pack (prettyShow (indexEntryVersion entry)), indexEntryRevision entry, fromIntegral (indexEntryOffset entry)) | entry <- entries])
          | (name, entries) <- Map.toList (scanEntries scan)
          ],
      indexTableRanges = Map.fromList [(BSC.pack name, BSC.pack (prettyShow range)) | (name, range) <- Map.toList (scanPreferredRanges scan)],
      indexTableState = scanIndexState scan
    }

-- | The versions of a package from the table, newest first and each with
-- its revisions oldest first, or 'Nothing' for an unknown package.
indexTableVersions :: IndexTable -> String -> Maybe [IndexVersion]
indexTableVersions table name = do
  entries <- Map.lookup key (indexTableEntries table)
  let range = Map.lookup key (indexTableRanges table) >>= simpleParsec . BSC.unpack :: Maybe VersionRange
      byVersion =
        Map.fromListWith
          (flip (<>))
          [ (version, [IndexEntry version revision (fromIntegral offset)])
          | (versionText, revision, offset) <- entries,
            Just version <- [simpleParsec (BSC.unpack versionText)]
          ]
  pure
    [ IndexVersion
        { indexVersionVersion = version,
          indexVersionRevisions = sortOn indexEntryRevision revisions,
          indexVersionDeprecated = maybe False (not . withinRange version) range
        }
    | (version, revisions) <- sortOn (Down . fst) (Map.toList byVersion)
    ]
  where
    key = BSC.pack name

-- | Render the table as lines: a header, the index state, one @name
-- version revision offset@ line per cabal entry, and one @!name range@ line
-- per preferred range.
renderIndexTable :: IndexTable -> BS.ByteString
renderIndexTable table =
  BSC.unlines
    ( [BSC.pack tableHeader, BSC.pack ("state " <> show (indexTableState table))]
        <> [ BSC.unwords [name, version, BSC.pack (show revision), BSC.pack (show offset)]
           | (name, entries) <- Map.toAscList (indexTableEntries table),
             (version, revision, offset) <- entries
           ]
        <> [ BSC.cons '!' name <> BSC.pack " " <> range
           | (name, range) <- Map.toAscList (indexTableRanges table)
           ]
    )

-- | Read back what 'renderIndexTable' wrote.
parseIndexTable :: BS.ByteString -> Either String IndexTable
parseIndexTable contents =
  case BSC.lines contents of
    header : stateLine : rest
      | header == BSC.pack tableHeader,
        [stateWord, stateText] <- BSC.words stateLine,
        stateWord == BSC.pack "state",
        Just (state, remaining) <- BSC.readInteger stateText,
        BS.null remaining ->
          Right
            IndexTable
              { indexTableEntries = Map.fromAscListWith (flip (<>)) (mapMaybe entryLine rest),
                indexTableRanges = Map.fromList (mapMaybe rangeLine rest),
                indexTableState = fromIntegral state
              }
    _ -> Left ("expected the header " <> show tableHeader)
  where
    entryLine line =
      case BSC.words line of
        [name, version, revisionText, offsetText]
          | Just (revision, r1) <- BSC.readInt revisionText,
            BS.null r1,
            Just (offset, r2) <- BSC.readInteger offsetText,
            BS.null r2 ->
              Just (name, [(version, revision, fromIntegral offset)])
        _ -> Nothing
    rangeLine line =
      case BSC.uncons line of
        Just ('!', body) ->
          let (name, range) = BSC.break (== ' ') body
           in Just (name, BSC.drop 1 range)
        _ -> Nothing

tableHeader :: String
tableHeader = "aihc-hackage-index 1"
