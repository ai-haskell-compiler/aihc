{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tarball generation for Stackage packages.
module Aihc.Parser.Bench.Tarball
  ( -- * Types
    TarballEntry (..),
    GenerateResult (..),
    PackageSpec (..),
    PackageInfo (..),
    formatPackage,
    isCabalEntry,
    isHaskellEntry,

    -- * Generation
    generateTarball,
    generateTarballEntries,

    -- * Filtering
    checkPackageFilters,
    FilterReason (..),
    isIncludeEntry,

    -- * Tarball I/O
    writeTarball,
    streamTarball,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Cpp qualified as HackageCpp
import Aihc.Hackage.Download qualified as HD
import Aihc.Hackage.Stackage qualified as HS
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HU
import Aihc.Parser.Bench.CLI (FilterOptions (..), GenerateOptions (..))
import Aihc.Parser.Bench.Parsers (ParseResult (..), collectCppIncludes, parseWithAihcExts, parseWithGhcExts, parseWithHseExts)
import Aihc.Parser.Lex (readModuleHeaderExtensions)
import Aihc.Parser.Syntax
  ( Extension (CPP),
    applyExtensionSetting,
    editionFromExtensionSettings,
    languageEditionExtensions,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, mplus, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription
  ( Executable,
    Library,
    autogenModules,
    buildInfo,
    buildable,
    condExecutables,
    condLibrary,
    condSubLibraries,
    cppOptions,
    exeModules,
    exposedModules,
    hsSourceDirs,
    libBuildInfo,
    modulePath,
    otherModules,
  )
import Distribution.PackageDescription.Parsec qualified as Cabal
import Distribution.Types.CondTree (CondTree (condTreeData))
import Distribution.Types.Condition (Condition)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Utils.Path (getSymbolicPath)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
  )
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

-- | An entry to be written to the tarball.
-- This can be either a Haskell source file or a .cabal file.
data TarballEntry = TarballEntry
  { entryPackage :: !PackageSpec,
    entryFilePath :: !FilePath,
    entryContents :: !Text,
    entryByteSize :: !Int,
    -- | Extension names from cabal file (empty for .cabal files themselves)
    entryExtensions :: ![String],
    -- | CPP options from the @cpp-options@ cabal field (empty for non-Haskell entries)
    entryCppOptions :: ![String],
    -- | Default language from cabal file (Nothing for .cabal files)
    entryLanguage :: !(Maybe String),
    -- | Build dependency package names (empty for non-Haskell entries)
    entryDependencies :: ![Text]
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

-- | Check if an entry is a CPP include file (not .hs/.lhs and not .cabal)
isIncludeEntry :: TarballEntry -> Bool
isIncludeEntry e = not (isCabalEntry e) && not (isHaskellEntry e)

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
    -- | Map from relative file paths to (extensions, cppOptions, language, dependencies)
    packageInfoFiles :: !(Map.Map FilePath ([String], [String], Maybe String, [Text]))
  }
  deriving (Show)

-- | Generate a tarball from a Stackage snapshot.
generateTarball :: GenerateOptions -> IO (Either String GenerateResult)
generateTarball opts = do
  -- Create a single HTTP manager to reuse for all requests
  manager <- newManager tlsManagerSettings
  snapshotResult <- HS.loadStackageSnapshot (Just manager) (genSnapshot opts) (genOffline opts)
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

sanitizeName :: String -> String
sanitizeName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'

-- | Process all packages sequentially.
processPackages :: Manager -> GenerateOptions -> [PackageSpec] -> IO [Either (PackageSpec, FilterReason) [TarballEntry]]
processPackages manager opts packages = forM packages (processPackage manager opts)

-- | Process a single package.
processPackage :: Manager -> GenerateOptions -> PackageSpec -> IO (Either (PackageSpec, FilterReason) [TarballEntry])
processPackage manager opts pkg = do
  let dlOpts =
        HD.defaultDownloadOptions
          { HD.downloadVerbose = False,
            HD.downloadAllowNetwork = not (genOffline opts),
            HD.downloadManager = Just manager
          }
  downloadResult <- try $ HD.downloadPackageWithOptions dlOpts pkg
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
            else
              if genPreprocess opts
                then do
                  -- Preprocess mode: preprocess Haskell files, no include files
                  entries <- forM hsFiles $ \file -> do
                    contents <- HU.readTextFileLenient file
                    let relPath = formatPackage pkg </> makeRelative pkgDir file
                        -- Look up extension info for this file
                        info = Map.lookup (makeRelative pkgDir file) fileInfoMap
                        exts = maybe [] (\(e, _, _, _) -> e) info
                        cppOpts = maybe [] (\(_, c, _, _) -> c) info
                        lang = info >>= \(_, _, l, _) -> l
                        deps = maybe [] (\(_, _, _, d) -> d) info
                    -- Preprocess the source if CPP is enabled
                    let preprocessedContents = preprocessSource pkg exts cppOpts lang deps contents
                    pure
                      TarballEntry
                        { entryPackage = pkg,
                          entryFilePath = relPath,
                          entryContents = preprocessedContents,
                          entryByteSize = BS.length (encodeUtf8 preprocessedContents),
                          entryExtensions = exts,
                          entryCppOptions = [], -- CPP options are now baked in
                          entryLanguage = lang,
                          entryDependencies = deps
                        }

                  -- Check filters (no include map needed, CPP already baked in)
                  filterResult <- checkPackageFiltersNoInclude (genFilters opts) entries
                  case filterResult of
                    Just reason -> pure (Left (pkg, reason))
                    -- Include only the .cabal file and preprocessed Haskell files
                    Nothing -> pure (Right (cabalEntry : entries))
                else do
                  -- Normal mode: collect include files
                  -- Read all files with their extension info from the cabal file
                  entries <- forM hsFiles $ \file -> do
                    contents <- HU.readTextFileLenient file
                    let relPath = formatPackage pkg </> makeRelative pkgDir file
                        -- Look up extension info for this file
                        info = Map.lookup (makeRelative pkgDir file) fileInfoMap
                        exts = maybe [] (\(e, _, _, _) -> e) info
                        cppOpts = maybe [] (\(_, c, _, _) -> c) info
                        lang = info >>= \(_, _, l, _) -> l
                        deps = maybe [] (\(_, _, _, d) -> d) info
                    pure
                      TarballEntry
                        { entryPackage = pkg,
                          entryFilePath = relPath,
                          entryContents = contents,
                          entryByteSize = BS.length (encodeUtf8 contents),
                          entryExtensions = exts,
                          entryCppOptions = cppOpts,
                          entryLanguage = lang,
                          entryDependencies = deps
                        }

                  -- Collect CPP include files for all Haskell entries
                  includeFileMap <- collectIncludeFiles pkgDir pkg entries
                  let includeEntries = buildIncludeEntries pkg includeFileMap

                  -- Check filters (only for Haskell files, with include map for CPP)
                  filterResult <- checkPackageFilters includeFileMap (genFilters opts) entries
                  case filterResult of
                    Just reason -> pure (Left (pkg, reason))
                    -- Include the .cabal file along with the Haskell files and include files
                    Nothing -> pure (Right (cabalEntry : entries ++ includeEntries))

-- | Find and read the .cabal file, returning the entry and a map of file paths to their extensions
findAndReadCabalFile :: FilePath -> PackageSpec -> IO (TarballEntry, Map.Map FilePath ([String], [String], Maybe String, [Text]))
findAndReadCabalFile pkgDir pkg = do
  cabalFiles <- findCabalFilesFlat pkgDir
  cabalFile <- case cabalFiles of
    [f] -> pure f
    [] -> ioError (userError ("No .cabal file found in " ++ pkgDir))
    (f : _) -> pure f -- Take first one if multiple
  contents <- HU.readTextFileLenient cabalFile
  let relPath = formatPackage pkg </> makeRelative pkgDir cabalFile
      cabalEntry =
        TarballEntry
          { entryPackage = pkg,
            entryFilePath = relPath,
            entryContents = contents,
            entryByteSize = BS.length (encodeUtf8 contents),
            entryExtensions = [],
            entryCppOptions = [],
            entryLanguage = Nothing,
            entryDependencies = []
          }

  -- Parse the cabal file to get extension info for each source file
  fileInfoMap <- parseCabalForExtensions pkgDir cabalFile
  pure (cabalEntry, fileInfoMap)

-- | Parse a cabal file and return a map from file paths to (extensions, cppOptions, language, dependencies)
parseCabalForExtensions :: FilePath -> FilePath -> IO (Map.Map FilePath ([String], [String], Maybe String, [Text]))
parseCabalForExtensions pkgDir cabalFile = do
  cabalBytes <- BS.readFile cabalFile
  let parseResult = Cabal.runParseResult (Cabal.parseGenericPackageDescription cabalBytes)
  case snd parseResult of
    Left errs -> do
      -- Log parse errors instead of silently ignoring
      hPutStrLn stderr $ "Warning: Failed to parse " ++ cabalFile ++ ": " ++ show errs
      pure Map.empty
    Right gpd -> do
      fileInfos <- HC.collectComponentFiles gpd (takeDirectory cabalFile)
      -- Build map from relative paths to their (extensions, cppOptions, language, dependencies)
      pure $
        Map.fromList
          [ ( makeRelative pkgDir (HC.fileInfoPath fi),
              (HC.fileInfoExtensions fi, HC.fileInfoCppOptions fi, HC.fileInfoLanguage fi, HC.fileInfoDependencies fi)
            )
          | fi <- fileInfos
          ]

-- | Collect CPP include files for a list of Haskell tarball entries.
-- Returns a map from tarball-relative path to file contents.
collectIncludeFiles :: FilePath -> PackageSpec -> [TarballEntry] -> IO (Map.Map FilePath Text)
collectIncludeFiles pkgDir pkg entries = do
  includeLists <- forM (filter isHaskellEntry entries) $ \e -> do
    let absFile = pkgDir </> makeRelative (formatPackage pkg) (entryFilePath e)
    pairs <- collectCppIncludes absFile (entryExtensions e) (entryCppOptions e) (entryLanguage e) (entryDependencies e) (entryContents e)
    pure [(formatPackage pkg </> makeRelative pkgDir absPath, text) | (absPath, text) <- pairs]
  pure $ Map.fromList (concat includeLists)

-- | Build TarballEntry items from a map of include files.
buildIncludeEntries :: PackageSpec -> Map.Map FilePath Text -> [TarballEntry]
buildIncludeEntries pkg includeMap =
  [ TarballEntry
      { entryPackage = pkg,
        entryFilePath = tarPath,
        entryContents = contents,
        entryByteSize = BS.length (encodeUtf8 contents),
        entryExtensions = [],
        entryCppOptions = [],
        entryLanguage = Nothing,
        entryDependencies = []
      }
  | (tarPath, contents) <- Map.toList includeMap
  ]

-- | Preprocess Haskell source with CPP if enabled.
-- Uses the same logic as the benchmark parsers.
preprocessSource :: PackageSpec -> [String] -> [String] -> Maybe String -> [Text] -> Text -> Text
preprocessSource _pkg cabalExts cppOptions langName deps source =
  let -- Get extension settings from cabal and header
      cabalSettings = mapMaybe (parseExtensionSettingName . T.pack) cabalExts
      headerSettings = readModuleHeaderExtensions source
      allSettings = cabalSettings <> headerSettings
      -- Determine the language edition: LANGUAGE pragmas override cabal language
      headerEdition = editionFromExtensionSettings allSettings
      cabalEdition = langName >>= parseLanguageEdition . T.pack
      effectiveEdition = headerEdition `mplus` cabalEdition
      -- Get base extensions for the edition
      baseExts = maybe [] languageEditionExtensions effectiveEdition
      -- Apply settings to get final extensions
      finalExtensions = foldr applyExtensionSetting baseExts allSettings
      cppEnabled = CPP `elem` finalExtensions
   in if cppEnabled
        then runCppPreprocess cppOptions deps source
        else source

-- | Run CPP preprocessor on source without include resolution.
-- For preprocessing during tarball generation, we inline includes by
-- setting up an empty include map - the includes will be resolved during
-- the benchmark phase when the preprocessed source is parsed.
-- Actually, for --preprocess mode, we want to fully resolve includes now.
-- Since we don't have the include files available here, we just run CPP
-- and let it fail gracefully on missing includes.
runCppPreprocess :: [String] -> [Text] -> Text -> Text
runCppPreprocess cppOptions deps source =
  let minVersionMacros = HackageCpp.minVersionMacroNamesFromDeps deps
      injected = HackageCpp.injectSyntheticCppMacros cppOptions minVersionMacros source
      cfg =
        Cpp.defaultConfig
          { Cpp.configInputFile = "<preprocess>",
            Cpp.configMacros = HackageCpp.cppMacrosFromOptions cppOptions
          }
   in Cpp.resultOutput (go (Cpp.preprocess cfg (TE.encodeUtf8 injected)))
  where
    go (Cpp.Done result) = result
    go (Cpp.NeedInclude _req k) =
      -- For tarball preprocessing, we don't resolve includes separately
      -- They should be inlined by the preprocessor if they exist
      go (k Nothing)

-- | Check if a package passes all filters (without include map).
-- Used when preprocessing is enabled during tarball generation.
checkPackageFiltersNoInclude :: FilterOptions -> [TarballEntry] -> IO (Maybe FilterReason)
checkPackageFiltersNoInclude opts entries
  | not (filterAihc opts || filterHse opts || filterGhc opts) = pure Nothing
  | otherwise = pure $ listToMaybe $ mapMaybe checkEntry (filter isHaskellEntry entries)
  where
    checkEntry e =
      let source = entryContents e
          path = entryFilePath e
          exts = entryExtensions e
          cppOpts = entryCppOptions e
          lang = entryLanguage e
          deps = entryDependencies e
          -- For preprocessed sources, CPP is already baked in, so we parse as-is
          checks =
            [ if filterAihc opts
                then case parseWithAihcExts Map.empty path exts cppOpts lang deps source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterAihcFailed path err)
                else Nothing,
              if filterHse opts
                then case parseWithHseExts Map.empty path exts cppOpts lang deps source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterHseFailed path err)
                else Nothing,
              if filterGhc opts
                then case parseWithGhcExts Map.empty path exts cppOpts lang deps source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterGhcFailed path err)
                else Nothing
            ]
       in listToMaybe (catMaybes checks)

-- | Check if a package passes all filters.
-- Returns the first filter failure reason, or Nothing if all pass.
checkPackageFilters :: Map.Map FilePath Text -> FilterOptions -> [TarballEntry] -> IO (Maybe FilterReason)
checkPackageFilters includeMap opts entries
  | not (filterAihc opts || filterHse opts || filterGhc opts) = pure Nothing
  | otherwise = pure $ listToMaybe $ mapMaybe checkEntry (filter isHaskellEntry entries)
  where
    checkEntry e =
      let source = entryContents e
          path = entryFilePath e
          exts = entryExtensions e
          cppOpts = entryCppOptions e
          lang = entryLanguage e
          deps = entryDependencies e
          checks =
            [ if filterAihc opts
                then case parseWithAihcExts includeMap path exts cppOpts lang deps source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterAihcFailed path err)
                else Nothing,
              if filterHse opts
                then case parseWithHseExts includeMap path exts cppOpts lang deps source of
                  ParseSuccess -> Nothing
                  ParseFailure err -> Just (FilterHseFailed path err)
                else Nothing,
              if filterGhc opts
                then case parseWithGhcExts includeMap path exts cppOpts lang deps source of
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
                    -- Extensions/cpp info will be filled in later from .cabal parsing
                    entryExtensions = [],
                    entryCppOptions = [],
                    entryLanguage = Nothing,
                    entryDependencies = []
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

-- | Extract file info from a parsed GenericPackageDescription.
--
-- This variant works on in-memory tarball paths (with forward slashes) rather
-- than filesystem paths, so it does NOT use the IO-based file-discovery in
-- 'HC.collectComponentFiles'.
extractFileInfoFromGPD :: GenericPackageDescription -> String -> Map.Map FilePath ([String], [String], Maybe String, [Text])
extractFileInfoFromGPD gpd pkgRoot =
  let evalCond = HC.conditionEvaluator gpd
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)

      libraryInfos = concatMap (libraryFileInfos evalCond pkgRoot) libraryTrees
      executableInfos = concatMap (executableFileInfos evalCond pkgRoot) executableTrees
   in Map.fromList (libraryInfos <> executableInfos)

libraryFileInfos :: (Condition ConfVar -> Bool) -> String -> CondTree ConfVar c Library -> [(FilePath, ([String], [String], Maybe String, [Text]))]
libraryFileInfos evalCond pkgRoot tree =
  let library = condTreeData tree
      build = HC.collectMergedBuildInfo evalCond libBuildInfo tree
      moduleNames = exposedModules library <> otherModules build <> autogenModules build
      exts = HC.extractExtensions build
      cppOpts = cppOptions build
      lang = HC.extractLanguage build
      deps = HC.extractDependencies build
      dirs = case map getSymbolicPath (hsSourceDirs build) of
        [] -> [""]
        ds -> ds
   in if not (buildable build)
        then []
        else
          [ (pkgRoot ++ dir ++ (if null dir then "" else "/") ++ toFilePath modu ++ ext, (exts, cppOpts, lang, deps))
          | dir <- dirs,
            modu <- moduleNames,
            ext <- [".hs", ".lhs"]
          ]

executableFileInfos :: (Condition ConfVar -> Bool) -> String -> CondTree ConfVar c Executable -> [(FilePath, ([String], [String], Maybe String, [Text]))]
executableFileInfos evalCond pkgRoot tree =
  let executable = condTreeData tree
      build = HC.collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = modulePath executable
      exts = HC.extractExtensions build
      cppOpts = cppOptions build
      lang = HC.extractLanguage build
      deps = HC.extractDependencies build
      dirs = case map getSymbolicPath (hsSourceDirs build) of
        [] -> [""]
        ds -> ds
   in if not (buildable build)
        then []
        else
          [ (pkgRoot ++ dir ++ (if null dir then "" else "/") ++ toFilePath modu ++ ext, (exts, cppOpts, lang, deps))
          | dir <- dirs,
            modu <- moduleNames,
            ext <- [".hs", ".lhs"]
          ]
            ++ [(pkgRoot ++ dir ++ (if null dir then "" else "/") ++ mainPath, (exts, cppOpts, lang, deps)) | dir <- dirs]

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

-- | Find all .cabal files in a directory (non-recursive, top-level only).
findCabalFilesFlat :: FilePath -> IO [FilePath]
findCabalFilesFlat dir = do
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

-- | Partition results into successes and failures.
partitionResults :: [Either (PackageSpec, FilterReason) [TarballEntry]] -> ([[TarballEntry]], [(PackageSpec, FilterReason)])
partitionResults = go [] []
  where
    go successes failures [] = (reverse successes, reverse failures)
    go successes failures (Left f : rest) = go successes (f : failures) rest
    go successes failures (Right s : rest) = go (s : successes) failures rest
