{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tarball generation for Stackage packages.
module Aihc.Parser.Bench.Tarball
  ( -- * Types
    TarballEntry (..),
    GenerateResult (..),
    PackageSpec (..),
    PackageInfo (..),
    isCabalEntry,
    isHaskellEntry,

    -- * Generation
    generateTarball,
    generateTarballEntries,

    -- * Filtering
    checkPackageFilters,
    FilterReason (..),

    -- * Tarball I/O
    writeTarball,
    streamTarball,
  )
where

import Aihc.Parser.Bench.CLI (FilterOptions (..), GenerateOptions (..))
import Aihc.Parser.Bench.Parsers (ParseResult (..), parseWithAihcExts, parseWithGhcExts, parseWithHseExts)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version qualified as DV
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
  ( BuildInfo,
    Executable,
    FlagName,
    Library,
    autogenModules,
    buildInfo,
    buildable,
    condExecutables,
    condLibrary,
    condSubLibraries,
    defaultExtensions,
    defaultLanguage,
    exeModules,
    exposedModules,
    flagDefault,
    flagName,
    hsSourceDirs,
    libBuildInfo,
    modulePath,
    oldExtensions,
    otherModules,
  )
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.CondTree
  ( CondBranch (CondBranch),
    CondTree (condTreeComponents, condTreeData),
  )
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion, withinRange)
import Network.HTTP.Client (Manager, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath (makeRelative, normalise, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.Info (compilerName, compilerVersion)

-- | A package specification.
data PackageSpec = PackageSpec
  { pkgName :: !String,
    pkgVersion :: !String
  }
  deriving (Eq, Show)

-- | An entry to be written to the tarball.
-- This can be either a Haskell source file or a .cabal file.
data TarballEntry = TarballEntry
  { entryPackage :: !PackageSpec,
    entryFilePath :: !FilePath,
    entryContents :: !Text,
    entryByteSize :: !Int,
    -- | Extension names from cabal file (empty for .cabal files themselves)
    entryExtensions :: ![String],
    -- | Default language from cabal file (Nothing for .cabal files)
    entryLanguage :: !(Maybe String)
  }
  deriving (Show)

-- | Check if an entry is a .cabal file
isCabalEntry :: TarballEntry -> Bool
isCabalEntry e = ".cabal" `isSuffixOf` entryFilePath e

-- | Check if an entry is a Haskell source file
isHaskellEntry :: TarballEntry -> Bool
isHaskellEntry e =
  let ext = takeExtension (entryFilePath e)
   in ext == ".hs" || ext == ".lhs"

-- | Why a package was filtered out.
data FilterReason
  = FilterAihcFailed !FilePath !String
  | FilterHseFailed !FilePath !String
  | FilterGhcFailed !FilePath !String
  | FilterNoHaskellFiles
  | FilterDownloadFailed !String
  | FilterCabalParseFailed !String
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

-- | Package info extracted from .cabal files in the tarball
data PackageInfo = PackageInfo
  { packageInfoSpec :: !PackageSpec,
    -- | Map from relative file paths to (extensions, language)
    packageInfoFiles :: !(Map.Map FilePath ([String], Maybe String))
  }
  deriving (Show)

-- | Generate a tarball from a Stackage snapshot.
generateTarball :: GenerateOptions -> IO (Either String GenerateResult)
generateTarball opts = do
  -- Create a single HTTP manager to reuse for all requests
  manager <- newManager tlsManagerSettings
  snapshotResult <- loadStackageSnapshot manager (genSnapshot opts) (genOffline opts)
  case snapshotResult of
    Left err -> pure (Left err)
    Right packages -> do
      when (genVerbose opts) $
        hPutStrLn stderr $
          "Loaded " ++ show (length packages) ++ " packages from " ++ genSnapshot opts

      -- Process packages and collect entries/failures
      results <- processPackages manager opts packages

      let (successes, failures) = partitionResults results
          allEntries = concat successes
          -- Count only Haskell files, not .cabal files
          totalFiles = length (filter isHaskellEntry allEntries)
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
              "Writing " ++ show totalFiles ++ " Haskell files to " ++ outputPath
          writeTarball outputPath allEntries
          pure (Right summary)

-- | Process all packages sequentially.
processPackages :: Manager -> GenerateOptions -> [PackageSpec] -> IO [Either (PackageSpec, FilterReason) [TarballEntry]]
processPackages manager opts packages = forM packages (processPackage manager opts)

-- | Process a single package.
processPackage :: Manager -> GenerateOptions -> PackageSpec -> IO (Either (PackageSpec, FilterReason) [TarballEntry])
processPackage manager opts pkg = do
  downloadResult <- try $ downloadPackage manager (genOffline opts) pkg
  case downloadResult of
    Left (err :: SomeException) ->
      pure (Left (pkg, FilterDownloadFailed (displayException err)))
    Right pkgDir -> do
      -- Find and read .cabal file
      cabalResult <- try $ findAndReadCabalFile pkgDir pkg
      case cabalResult of
        Left (err :: SomeException) ->
          pure (Left (pkg, FilterCabalParseFailed (displayException err)))
        Right (cabalEntry, fileInfoMap) -> do
          hsFiles <- findHaskellFiles pkgDir
          if null hsFiles
            then pure (Left (pkg, FilterNoHaskellFiles))
            else do
              -- Read all files with their extension info from the cabal file
              entries <- forM hsFiles $ \file -> do
                contents <- readTextFileLenient file
                let relPath = formatPackage pkg </> makeRelative pkgDir file
                    -- Look up extension info for this file
                    info = Map.lookup (makeRelative pkgDir file) fileInfoMap
                    exts = maybe [] fst info
                    lang = info >>= snd
                pure
                  TarballEntry
                    { entryPackage = pkg,
                      entryFilePath = relPath,
                      entryContents = contents,
                      entryByteSize = BS.length (encodeUtf8 contents),
                      entryExtensions = exts,
                      entryLanguage = lang
                    }

              -- Check filters (only for Haskell files)
              filterResult <- checkPackageFilters (genFilters opts) entries
              case filterResult of
                Just reason -> pure (Left (pkg, reason))
                -- Include the .cabal file along with the Haskell files
                Nothing -> pure (Right (cabalEntry : entries))

-- | Find and read the .cabal file, returning the entry and a map of file paths to their extensions
findAndReadCabalFile :: FilePath -> PackageSpec -> IO (TarballEntry, Map.Map FilePath ([String], Maybe String))
findAndReadCabalFile pkgDir pkg = do
  cabalFiles <- findCabalFiles pkgDir
  cabalFile <- case cabalFiles of
    [f] -> pure f
    [] -> ioError (userError ("No .cabal file found in " ++ pkgDir))
    (f : _) -> pure f -- Take first one if multiple
  contents <- readTextFileLenient cabalFile
  let relPath = formatPackage pkg </> makeRelative pkgDir cabalFile
      cabalEntry =
        TarballEntry
          { entryPackage = pkg,
            entryFilePath = relPath,
            entryContents = contents,
            entryByteSize = BS.length (encodeUtf8 contents),
            entryExtensions = [],
            entryLanguage = Nothing
          }

  -- Parse the cabal file to get extension info for each source file
  fileInfoMap <- parseCabalForExtensions pkgDir cabalFile
  pure (cabalEntry, fileInfoMap)

-- | Parse a cabal file and return a map from file paths to (extensions, language)
parseCabalForExtensions :: FilePath -> FilePath -> IO (Map.Map FilePath ([String], Maybe String))
parseCabalForExtensions pkgDir cabalFile = do
  cabalBytes <- BS.readFile cabalFile
  let parseResult = Cabal.runParseResult (Cabal.parseGenericPackageDescription cabalBytes)
  case snd parseResult of
    Left errs -> do
      -- Log parse errors instead of silently ignoring
      hPutStrLn stderr $ "Warning: Failed to parse " ++ cabalFile ++ ": " ++ show errs
      pure Map.empty
    Right gpd -> do
      fileInfos <- collectComponentFilesSimple gpd (takeDirectory cabalFile)
      -- Build map from relative paths to their extensions
      pure $
        Map.fromList
          [ (makeRelative pkgDir path, (exts, lang))
          | (path, exts, lang) <- fileInfos
          ]

-- | Simplified version of collectComponentFiles that just gets paths and extensions
collectComponentFilesSimple :: GenericPackageDescription -> FilePath -> IO [(FilePath, [String], Maybe String)]
collectComponentFilesSimple gpd packageRoot = do
  let evalCond = conditionEvaluator gpd
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)

  libraryFiles <- fmap concat (forM libraryTrees (libraryFilesSimple evalCond packageRoot))
  executableFiles <- fmap concat (forM executableTrees (executableFilesSimple evalCond packageRoot))

  pure (libraryFiles <> executableFiles)

libraryFilesSimple :: (Condition ConfVar -> Bool) -> FilePath -> CondTree ConfVar c Library -> IO [(FilePath, [String], Maybe String)]
libraryFilesSimple evalCond packageRoot tree = do
  let library = condTreeData tree
      build = collectMergedBuildInfo evalCond libBuildInfo tree
      moduleNames = exposedModules library <> otherModules build <> autogenModules build
      exts = extractExtensions build
      lang = extractLanguage build
  if not (buildable build)
    then pure []
    else do
      paths <- moduleFilesForBuildInfo packageRoot build moduleNames
      pure [(path, exts, lang) | path <- paths]

executableFilesSimple :: (Condition ConfVar -> Bool) -> FilePath -> CondTree ConfVar c Executable -> IO [(FilePath, [String], Maybe String)]
executableFilesSimple evalCond packageRoot tree = do
  let executable = condTreeData tree
      build = collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = modulePath executable
      exts = extractExtensions build
      lang = extractLanguage build
  if not (buildable build)
    then pure []
    else do
      moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
      mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build]
      pure [(path, exts, lang) | path <- moduleFiles <> mainFiles]

extractExtensions :: BuildInfo -> [String]
extractExtensions bi = nub (map prettyShow (defaultExtensions bi <> oldExtensions bi))

extractLanguage :: BuildInfo -> Maybe String
extractLanguage bi =
  case defaultLanguage bi of
    Just lang -> Just (prettyShow lang)
    Nothing -> Nothing -- Don't default, let the parser decide

collectCondTreeData :: (Condition v -> Bool) -> CondTree v c a -> [a]
collectCondTreeData evalCond tree =
  condTreeData tree : concatMap collectBranch (condTreeComponents tree)
  where
    collectBranch (CondBranch cond thenTree elseTree) =
      if evalCond cond
        then collectCondTreeData evalCond thenTree
        else maybe [] (collectCondTreeData evalCond) elseTree

collectMergedBuildInfo :: (Monoid b) => (Condition v -> Bool) -> (a -> b) -> CondTree v c a -> b
collectMergedBuildInfo evalCond toBuildInfo =
  mconcat . map toBuildInfo . collectCondTreeData evalCond

conditionEvaluator :: GenericPackageDescription -> Condition ConfVar -> Bool
conditionEvaluator gpd = eval
  where
    defaultFlags :: Map.Map FlagName Bool
    defaultFlags =
      Map.fromList [(flagName flag, flagDefault flag) | flag <- genPackageFlags gpd]

    compilerFlavor :: CompilerFlavor
    compilerFlavor =
      case compilerName of
        "ghc" -> GHC
        "ghcjs" -> GHCJS
        other -> OtherCompiler other

    compilerVer = mkVersion (DV.versionBranch compilerVersion)

    eval (Var confVar) =
      case confVar of
        OS os -> os == buildOS
        Arch arch -> arch == buildArch
        PackageFlag flag -> Map.findWithDefault False flag defaultFlags
        Impl flavor range -> flavor == compilerFlavor && withinRange compilerVer range
    eval (Lit b) = b
    eval (CNot c) = not (eval c)
    eval (COr a b) = eval a || eval b
    eval (CAnd a b) = eval a && eval b

moduleFilesForBuildInfo :: FilePath -> BuildInfo -> [ModuleName] -> IO [FilePath]
moduleFilesForBuildInfo packageRoot build modules = do
  let dirs = sourceDirs packageRoot build
      moduleCandidates =
        [ dir </> toFilePath modu <.> ext
        | dir <- dirs,
          modu <- modules,
          ext <- ["hs", "lhs"]
        ]
  dedupeExistingFiles moduleCandidates

sourceDirs :: FilePath -> BuildInfo -> [FilePath]
sourceDirs packageRoot build =
  case map getSymbolicPath (hsSourceDirs build) of
    [] -> [packageRoot]
    dirs -> [packageRoot </> dir | dir <- dirs]

existingPaths :: [FilePath] -> IO [FilePath]
existingPaths candidates = do
  existing <- forM candidates $ \candidate -> do
    fileExists <- doesFileExist candidate
    pure (if fileExists then Just (normalise candidate) else Nothing)
  pure (catMaybes existing)

dedupeExistingFiles :: [FilePath] -> IO [FilePath]
dedupeExistingFiles files = fmap nub (existingPaths files)

-- | Check if a package passes all filters.
-- Returns the first filter failure reason, or Nothing if all pass.
checkPackageFilters :: FilterOptions -> [TarballEntry] -> IO (Maybe FilterReason)
checkPackageFilters opts entries
  | not (filterAihc opts || filterHse opts || filterGhc opts) = pure Nothing
  | otherwise = pure $ listToMaybe $ mapMaybe checkEntry (filter isHaskellEntry entries)
  where
    checkEntry e =
      let source = entryContents e
          path = entryFilePath e
          exts = entryExtensions e
          lang = entryLanguage e
          checks =
            [ if filterAihc opts
                then case parseWithAihcExts exts lang source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterAihcFailed path err)
                else Nothing,
              if filterHse opts
                then case parseWithHseExts exts lang source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterHseFailed path err)
                else Nothing,
              if filterGhc opts
                then case parseWithGhcExts exts lang source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterGhcFailed path err)
                else Nothing
            ]
       in listToMaybe (catMaybes checks)

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

-- | Stream entries from a tarball, supporting both gzipped (.tar.gz) and uncompressed (.tar).
-- Also returns package info maps extracted from .cabal files.
streamTarball :: FilePath -> IO ([TarballEntry], Map.Map String PackageInfo)
streamTarball path = do
  raw <- LBS.readFile path
  let decompressed =
        if ".tar.gz" `isSuffixOf` path || ".tgz" `isSuffixOf` path
          then GZip.decompress raw
          else raw -- Assume uncompressed .tar
      tarEntries = Tar.read decompressed
      (entries, cabalContents) = extractEntriesWithCabal tarEntries

  -- Parse .cabal files to get extension info
  let packageInfos = parseCabalContents cabalContents
  pure (entries, packageInfos)

-- | Extract entries, separating out .cabal file contents for later parsing
extractEntriesWithCabal :: Tar.Entries Tar.FormatError -> ([TarballEntry], [(PackageSpec, FilePath, Text)])
extractEntriesWithCabal = go [] []
  where
    go entries cabals (Tar.Next entry rest) =
      case Tar.entryContent entry of
        Tar.NormalFile content _ ->
          let filePath = Tar.entryPath entry
              text = decodeUtf8With lenientDecode (LBS.toStrict content)
              pkg = parsePackageFromPath filePath
              tarEntry =
                TarballEntry
                  { entryPackage = pkg,
                    entryFilePath = filePath,
                    entryContents = text,
                    entryByteSize = fromIntegral (LBS.length content),
                    -- Extensions will be filled in later from .cabal parsing
                    entryExtensions = [],
                    entryLanguage = Nothing
                  }
           in if ".cabal" `isSuffixOf` filePath
                then go (tarEntry : entries) ((pkg, filePath, text) : cabals) rest
                else go (tarEntry : entries) cabals rest
        _ -> go entries cabals rest
    go entries cabals Tar.Done = (reverse entries, reverse cabals)
    go entries cabals (Tar.Fail _) = (reverse entries, reverse cabals)

-- | Parse .cabal file contents to extract extension info
parseCabalContents :: [(PackageSpec, FilePath, Text)] -> Map.Map String PackageInfo
parseCabalContents cabals =
  Map.fromList
    [ (formatPackage pkg, info)
    | (pkg, cabalPath, content) <- cabals,
      Just info <- [parseSingleCabal pkg cabalPath content]
    ]

-- | Parse a single .cabal file content
parseSingleCabal :: PackageSpec -> FilePath -> Text -> Maybe PackageInfo
parseSingleCabal pkg cabalPath content =
  let cabalBytes = encodeUtf8 content
      parseResult = Cabal.runParseResult (Cabal.parseGenericPackageDescription cabalBytes)
   in case snd parseResult of
        Left _ -> Nothing
        Right gpd ->
          let pkgPrefix = formatPackage pkg ++ "/"
              -- Get the package root from the cabal path
              cabalRelPath = dropPrefix pkgPrefix cabalPath
              pkgRoot =
                case takeDirectory cabalRelPath of
                  "" -> ""
                  d -> d ++ "/"
              fileInfos = extractFileInfoFromGPD gpd pkgRoot
           in Just
                PackageInfo
                  { packageInfoSpec = pkg,
                    packageInfoFiles = fileInfos
                  }
  where
    dropPrefix prefix s =
      if prefix `isPrefixOf` s
        then drop (length prefix) s
        else s

-- | Extract file info from a parsed GenericPackageDescription
extractFileInfoFromGPD :: GenericPackageDescription -> String -> Map.Map FilePath ([String], Maybe String)
extractFileInfoFromGPD gpd pkgRoot =
  let evalCond = conditionEvaluator gpd
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)

      libraryInfos = concatMap (libraryFileInfos evalCond pkgRoot) libraryTrees
      executableInfos = concatMap (executableFileInfos evalCond pkgRoot) executableTrees
   in Map.fromList (libraryInfos <> executableInfos)

libraryFileInfos :: (Condition ConfVar -> Bool) -> String -> CondTree ConfVar c Library -> [(FilePath, ([String], Maybe String))]
libraryFileInfos evalCond pkgRoot tree =
  let library = condTreeData tree
      build = collectMergedBuildInfo evalCond libBuildInfo tree
      moduleNames = exposedModules library <> otherModules build <> autogenModules build
      exts = extractExtensions build
      lang = extractLanguage build
      dirs = case map getSymbolicPath (hsSourceDirs build) of
        [] -> [""]
        ds -> ds
   in if not (buildable build)
        then []
        else
          [ (pkgRoot ++ dir ++ (if null dir then "" else "/") ++ toFilePath modu ++ ext, (exts, lang))
          | dir <- dirs,
            modu <- moduleNames,
            ext <- [".hs", ".lhs"]
          ]

executableFileInfos :: (Condition ConfVar -> Bool) -> String -> CondTree ConfVar c Executable -> [(FilePath, ([String], Maybe String))]
executableFileInfos evalCond pkgRoot tree =
  let executable = condTreeData tree
      build = collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = modulePath executable
      exts = extractExtensions build
      lang = extractLanguage build
      dirs = case map getSymbolicPath (hsSourceDirs build) of
        [] -> [""]
        ds -> ds
   in if not (buildable build)
        then []
        else
          [ (pkgRoot ++ dir ++ (if null dir then "" else "/") ++ toFilePath modu ++ ext, (exts, lang))
          | dir <- dirs,
            modu <- moduleNames,
            ext <- [".hs", ".lhs"]
          ]
            ++ [(pkgRoot ++ dir ++ (if null dir then "" else "/") ++ mainPath, (exts, lang)) | dir <- dirs]

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
loadStackageSnapshot :: Manager -> String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshot manager snapshot offline = do
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
          fetched <- httpGetString manager url
          case fetched of
            Left err -> pure (Left err)
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
downloadPackage :: Manager -> Bool -> PackageSpec -> IO FilePath
downloadPackage manager offline pkg = do
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
          -- Download tarball
          tarballBytes <- httpGetLBS manager url
          case tarballBytes of
            Left err -> ioError (userError ("Failed to download " ++ formatPackage pkg ++ ": " ++ err))
            Right lbs -> do
              -- Extract using Codec.Archive.Tar
              let entries = Tar.read (GZip.decompress lbs)
              pkgDirExists <- doesDirectoryExist pkgDir
              when pkgDirExists $ removeDirectoryRecursive pkgDir
              Tar.unpack cacheDir entries
              writeFile markerFile ""
              pure pkgDir

getCacheDir :: IO FilePath
getCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

formatPackage :: PackageSpec -> String
formatPackage pkg = pkgName pkg ++ "-" ++ pkgVersion pkg

-- | Find all .cabal files in a directory.
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let fullPath = dir </> entry
    isDir <- doesDirectoryExist fullPath
    if isDir
      then pure []
      else
        if ".cabal" `isSuffixOf` entry
          then pure [fullPath]
          else pure []
  pure (concat paths)

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

--------------------------------------------------------------------------------
-- HTTP utilities
--------------------------------------------------------------------------------

-- | Perform an HTTP GET request and return the response body as a String.
-- Uses lenient UTF-8 decoding to handle malformed sequences.
httpGetString :: Manager -> String -> IO (Either String String)
httpGetString manager url = do
  result <- httpGetLBS manager url
  pure $ fmap (TL.unpack . TLE.decodeUtf8With lenientDecode) result

-- | Perform an HTTP GET request and return the response body as lazy ByteString.
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
