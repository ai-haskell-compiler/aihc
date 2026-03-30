{-# LANGUAGE LambdaCase #-}

-- | Tarball generation for Stackage packages.
module Aihc.Parser.Bench.Tarball
  ( -- * Types
    TarballEntry (..),
    GenerateResult (..),
    PackageSpec (..),

    -- * Generation
    generateTarball,
    generateTarballEntries,

    -- * Filtering
    checkPackageFilters,
    FilterReason (..),

    -- * Tarball I/O
    writeTarball,
    streamTarballEntries,
  )
where

import Aihc.Parser.Bench.CLI (FilterOptions (..), GenerateOptions (..))
import Aihc.Parser.Bench.Parsers (ParseResult (..), parseWithAihc, parseWithGhc, parseWithHse)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumProcessors)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
  )
import System.FilePath (makeRelative, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand, readProcess)

-- | A package specification.
data PackageSpec = PackageSpec
  { pkgName :: !String,
    pkgVersion :: !String
  }
  deriving (Eq, Show)

-- | An entry to be written to the tarball.
data TarballEntry = TarballEntry
  { entryPackage :: !PackageSpec,
    entryFilePath :: !FilePath,
    entryContents :: !Text,
    entryByteSize :: !Int
  }
  deriving (Show)

-- | Why a package was filtered out.
data FilterReason
  = FilterAihcFailed !FilePath !String
  | FilterHseFailed !FilePath !String
  | FilterGhcFailed !FilePath !String
  | FilterNoHaskellFiles
  | FilterDownloadFailed !String
  deriving (Eq, Show)

-- | Result of tarball generation.
data GenerateResult = GenerateResult
  { resultTotalPackages :: !Int,
    resultIncludedPackages :: !Int,
    resultTotalFiles :: !Int,
    resultTotalBytes :: !Integer,
    resultFilteredOut :: ![(PackageSpec, FilterReason)]
  }
  deriving (Show)

-- | Generate a tarball from a Stackage snapshot.
generateTarball :: GenerateOptions -> IO (Either String GenerateResult)
generateTarball opts = do
  snapshotResult <- loadStackageSnapshot (genSnapshot opts) (genOffline opts)
  case snapshotResult of
    Left err -> pure (Left err)
    Right packages -> do
      when (genVerbose opts) $
        hPutStrLn stderr $
          "Loaded " ++ show (length packages) ++ " packages from " ++ genSnapshot opts

      jobs <- maybe getNumProcessors pure (genJobs opts)

      -- Process packages and collect entries/failures
      results <- processPackages opts jobs packages

      let (successes, failures) = partitionResults results
          allEntries = concat successes
          totalFiles = length allEntries
          totalBytes = sum (map (fromIntegral . entryByteSize) allEntries)
          summary =
            GenerateResult
              { resultTotalPackages = length packages,
                resultIncludedPackages = length successes,
                resultTotalFiles = totalFiles,
                resultTotalBytes = totalBytes,
                resultFilteredOut = failures
              }

      if genDryRun opts
        then do
          when (genVerbose opts) $ do
            hPutStrLn stderr "\nDry run - packages that would be included:"
            mapM_
              ( \case
                  e : _ -> hPutStrLn stderr ("  " ++ formatPackage (entryPackage e))
                  [] -> pure ()
              )
              (take 10 successes)
          pure (Right summary)
        else do
          let outputPath = case genOutput opts of
                Just p -> p
                Nothing -> "stackage-" ++ sanitizeName (genSnapshot opts) ++ ".tar.gz"
          when (genVerbose opts) $
            hPutStrLn stderr $
              "Writing " ++ show totalFiles ++ " files to " ++ outputPath
          writeTarball outputPath allEntries
          pure (Right summary)

-- | Process all packages in parallel.
processPackages :: GenerateOptions -> Int -> [PackageSpec] -> IO [Either (PackageSpec, FilterReason) [TarballEntry]]
processPackages opts jobs packages = do
  -- Process in chunks to avoid overwhelming the system
  let chunks = chunksOf jobs packages
  results <- forM chunks $ \chunk -> mapConcurrently (processPackage opts) chunk
  pure (concat results)

-- | Process a single package.
processPackage :: GenerateOptions -> PackageSpec -> IO (Either (PackageSpec, FilterReason) [TarballEntry])
processPackage opts pkg = do
  downloadResult <- try $ downloadPackage (genOffline opts) pkg
  case downloadResult of
    Left (err :: SomeException) ->
      pure (Left (pkg, FilterDownloadFailed (displayException err)))
    Right pkgDir -> do
      hsFiles <- findHaskellFiles pkgDir
      if null hsFiles
        then pure (Left (pkg, FilterNoHaskellFiles))
        else do
          -- Read all files
          entries <- forM hsFiles $ \file -> do
            contents <- readTextFileLenient file
            let relPath = formatPackage pkg </> makeRelative pkgDir file
            pure
              TarballEntry
                { entryPackage = pkg,
                  entryFilePath = relPath,
                  entryContents = contents,
                  entryByteSize = BS.length (encodeUtf8 contents)
                }

          -- Check filters
          filterResult <- checkPackageFilters (genFilters opts) entries
          case filterResult of
            Just reason -> pure (Left (pkg, reason))
            Nothing -> pure (Right entries)

-- | Check if a package passes all filters.
checkPackageFilters :: FilterOptions -> [TarballEntry] -> IO (Maybe FilterReason)
checkPackageFilters opts entries
  | not (filterAihc opts || filterHse opts || filterGhc opts) = pure Nothing
  | otherwise = go entries
  where
    go [] = pure Nothing
    go (e : es) = do
      let source = entryContents e
          path = entryFilePath e

      -- Check aihc filter
      aihcResult <-
        if filterAihc opts
          then case parseWithAihc source of
            ParseSuccess -> pure Nothing
            ParseFailure err -> pure (Just (FilterAihcFailed path err))
          else pure Nothing

      case aihcResult of
        Just reason -> pure (Just reason)
        Nothing -> do
          -- Check hse filter
          hseResult <-
            if filterHse opts
              then case parseWithHse source of
                ParseSuccess -> pure Nothing
                ParseFailure err -> pure (Just (FilterHseFailed path err))
              else pure Nothing

          case hseResult of
            Just reason -> pure (Just reason)
            Nothing -> do
              -- Check ghc filter
              ghcResult <-
                if filterGhc opts
                  then case parseWithGhc source of
                    ParseSuccess -> pure Nothing
                    ParseFailure err -> pure (Just (FilterGhcFailed path err))
                  else pure Nothing

              case ghcResult of
                Just reason -> pure (Just reason)
                Nothing -> go es

-- | Generate tarball entries without writing (for testing).
generateTarballEntries :: GenerateOptions -> IO (Either String ([TarballEntry], GenerateResult))
generateTarballEntries opts = do
  result <- generateTarball (opts {genDryRun = True})
  case result of
    Left err -> pure (Left err)
    Right summary -> pure (Right ([], summary))

-- | Write entries to a gzipped tarball.
writeTarball :: FilePath -> [TarballEntry] -> IO ()
writeTarball path entries = do
  now <- getPOSIXTime
  let tarEntries = mapMaybe (entryToTarEntry (round now)) entries
      tarData = Tar.write tarEntries
  LBS.writeFile path (GZip.compress tarData)

-- | Convert an entry to a tar entry.
entryToTarEntry :: Tar.EpochTime -> TarballEntry -> Maybe Tar.Entry
entryToTarEntry mtime e =
  case Tar.toTarPath False (entryFilePath e) of
    Left _ -> Nothing
    Right tarPath ->
      let content = LBS.fromStrict (encodeUtf8 (entryContents e))
       in Just $
            (Tar.fileEntry tarPath content)
              { Tar.entryTime = mtime,
                Tar.entryOwnership =
                  Tar.Ownership
                    { Tar.ownerName = "aihc",
                      Tar.groupName = "aihc",
                      Tar.ownerId = 1000,
                      Tar.groupId = 1000
                    }
              }

-- | Stream entries from a gzipped tarball.
streamTarballEntries :: FilePath -> IO [TarballEntry]
streamTarballEntries path = do
  compressed <- LBS.readFile path
  let entries = Tar.read (GZip.decompress compressed)
  pure (extractEntries entries)

extractEntries :: Tar.Entries Tar.FormatError -> [TarballEntry]
extractEntries (Tar.Next entry rest) =
  case Tar.entryContent entry of
    Tar.NormalFile content _ ->
      let filePath = Tar.entryPath entry
          text = decodeUtf8With lenientDecode (LBS.toStrict content)
          pkg = parsePackageFromPath filePath
       in TarballEntry
            { entryPackage = pkg,
              entryFilePath = filePath,
              entryContents = text,
              entryByteSize = fromIntegral (LBS.length content)
            }
            : extractEntries rest
    _ -> extractEntries rest
extractEntries Tar.Done = []
extractEntries (Tar.Fail _) = []

-- | Parse package name from tarball path.
parsePackageFromPath :: FilePath -> PackageSpec
parsePackageFromPath path =
  case break (== '/') path of
    (pkgVer, _) ->
      case breakOnLast '-' pkgVer of
        (name, ver) -> PackageSpec name ver

breakOnLast :: Char -> String -> (String, String)
breakOnLast c s =
  let (after, before) = break (== c) (reverse s)
   in case before of
        [] -> (s, "")
        _ : rest -> (reverse rest, reverse after)

-- | Load a Stackage snapshot.
loadStackageSnapshot :: String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshot snapshot offline = do
  cacheFile <- snapshotCacheFile snapshot
  hasCache <- doesFileExist cacheFile
  if hasCache
    then do
      cachedBody <- readFile cacheFile
      pure (parseSnapshotConstraints cachedBody)
    else
      if offline
        then pure (Left ("Snapshot missing from cache in offline mode: " ++ snapshot))
        else do
          let url = "https://www.stackage.org/" ++ snapshot ++ "/cabal.config"
          fetched <- try (readProcess "curl" ["-s", "-f", url] "")
          case fetched of
            Left (err :: SomeException) -> pure (Left (displayException err))
            Right body ->
              case parseSnapshotConstraints body of
                Left parseErr -> pure (Left parseErr)
                Right specs -> do
                  writeFile cacheFile body
                  pure (Right specs)

snapshotCacheFile :: String -> IO FilePath
snapshotCacheFile snapshot = do
  base <- getXdgDirectory XdgCache "aihc"
  let dir = base </> "stackage"
      file = sanitizeName snapshot ++ "-cabal.config"
  createDirectoryIfMissing True dir
  pure (dir </> file)

sanitizeName :: String -> String
sanitizeName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'

parseSnapshotConstraints :: String -> Either String [PackageSpec]
parseSnapshotConstraints content = do
  let section = constraintLines (lines content)
      entries = map trim (splitComma (concat section))
      specs = mapMaybe parseConstraint entries
  if null specs
    then Left "No package constraints found"
    else Right specs

constraintLines :: [String] -> [String]
constraintLines ls =
  case break (isPrefixOf "constraints:" . trimLeft) ls of
    (_, []) -> []
    (_, firstRaw : restRaw) ->
      let firstLine = trimLeft firstRaw
          start = [drop 12 firstLine]
          cont = [trimLeft line | line <- takeWhile isConstraintContinuation restRaw]
       in start <> cont

isConstraintContinuation :: String -> Bool
isConstraintContinuation line =
  case line of
    c : _ -> isSpace c
    [] -> False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseConstraint :: String -> Maybe PackageSpec
parseConstraint entry
  | null entry = Nothing
  | "--" `isPrefixOf` trim entry = Nothing
  | otherwise =
      case breakOn "==" entry of
        Just (name, ver) -> Just (PackageSpec (trim name) (trim ver))
        Nothing ->
          let ws = words entry
           in case ws of
                [_, "installed"] -> Nothing
                _ -> Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case findNeedle needle haystack of
    Nothing -> Nothing
    Just i ->
      let (left, right) = splitAt i haystack
       in Just (left, drop (length needle) right)

findNeedle :: String -> String -> Maybe Int
findNeedle needle = go 0
  where
    go _ [] = Nothing
    go i xs
      | needle `isPrefixOf` xs = Just i
      | otherwise = go (i + 1) (drop 1 xs)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Download a package from Hackage.
downloadPackage :: Bool -> PackageSpec -> IO FilePath
downloadPackage offline pkg = do
  cacheDir <- getCacheDir
  let pkgDir = cacheDir </> formatPackage pkg
      markerFile = pkgDir </> ".complete"
  markerExists <- doesFileExist markerFile
  if markerExists
    then pure pkgDir
    else
      if offline
        then ioError (userError ("Package missing from cache in offline mode: " ++ formatPackage pkg))
        else do
          createDirectoryIfMissing True cacheDir
          let url =
                "https://hackage.haskell.org/package/"
                  ++ formatPackage pkg
                  ++ "/"
                  ++ formatPackage pkg
                  ++ ".tar.gz"
          let tarball = cacheDir </> formatPackage pkg ++ ".tar.gz"
          let tempDir = cacheDir </> formatPackage pkg ++ ".tmp"
          callCommand ("curl -s -f -o " ++ show tarball ++ " " ++ show url)
          createDirectoryIfMissing True tempDir
          callCommand ("tar -xzf " ++ show tarball ++ " -C " ++ show tempDir)
          removeFile tarball
          pkgDirExists <- doesDirectoryExist pkgDir
          when pkgDirExists $ removeDirectoryRecursive pkgDir
          renameDirectory tempDir pkgDir
          writeFile markerFile ""
          pure pkgDir

getCacheDir :: IO FilePath
getCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

formatPackage :: PackageSpec -> String
formatPackage pkg = pkgName pkg ++ "-" ++ pkgVersion pkg

-- | Find all Haskell source files in a directory.
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let fullPath = dir </> entry
    isDir <- doesDirectoryExist fullPath
    if isDir
      then
        if entry `elem` [".git", "dist", "dist-newstyle", ".stack-work"]
          then pure []
          else findHaskellFiles fullPath
      else
        if takeExtension entry `elem` [".hs", ".lhs"]
          then pure [fullPath]
          else pure []
  pure (concat paths)

-- | Read a file as Text with lenient UTF-8 decoding.
readTextFileLenient :: FilePath -> IO Text
readTextFileLenient path = do
  bytes <- BS.readFile path
  pure (decodeUtf8With lenientDecode bytes)

-- | Partition results into successes and failures.
partitionResults :: [Either (PackageSpec, FilterReason) [TarballEntry]] -> ([[TarballEntry]], [(PackageSpec, FilterReason)])
partitionResults = go [] []
  where
    go successes failures [] = (reverse successes, reverse failures)
    go successes failures (Left f : rest) = go successes (f : failures) rest
    go successes failures (Right s : rest) = go (s : successes) failures rest

-- | Split a list into chunks.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest
