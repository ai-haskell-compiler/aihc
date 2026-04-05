{-# LANGUAGE OverloadedStrings #-}

module HackageSupport
  ( downloadPackage,
    downloadPackageQuiet,
    downloadPackageQuietWithNetwork,
    findTargetFilesFromCabal,
    FileInfo (..),
    readTextFileLenient,
    resolveIncludeBestEffort,
    diagToText,
    prefixCppErrors,
  )
where

import Aihc.Cpp (Diagnostic (..), IncludeKind (..), IncludeRequest (..), Severity (..))
import qualified Aihc.Parser.Syntax as Syntax
import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Version as DV
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
    cppOptions,
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
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
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
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeFileName, (<.>), (</>))
import System.Info (compilerName, compilerVersion)
import System.Process (callCommand)

downloadPackage :: String -> String -> IO FilePath
downloadPackage = downloadPackageWithLogs True

downloadPackageQuiet :: String -> String -> IO FilePath
downloadPackageQuiet = downloadPackageWithLogs False

downloadPackageQuietWithNetwork :: Bool -> String -> String -> IO FilePath
downloadPackageQuietWithNetwork = downloadPackageWithMode False

downloadPackageWithLogs :: Bool -> String -> String -> IO FilePath
downloadPackageWithLogs withLogs = downloadPackageWithMode withLogs True

downloadPackageWithMode :: Bool -> Bool -> String -> String -> IO FilePath
downloadPackageWithMode withLogs allowNetwork packageName version = do
  cacheDir <- getCacheDir
  let pkgDir = cacheDir </> packageName ++ "-" ++ version
      markerFile = pkgDir </> ".complete"
  markerExists <- doesFileExist markerFile
  if markerExists
    then do
      when withLogs $ putStrLn ("Cache hit: " ++ packageName ++ "-" ++ version)
      pure pkgDir
    else
      if not allowNetwork
        then ioError (userError ("Package missing from cache in offline mode: " ++ packageName ++ "-" ++ version))
        else do
          createDirectoryIfMissing True cacheDir
          when withLogs $ putStrLn ("Downloading " ++ packageName ++ "-" ++ version ++ " from Hackage...")
          let url = "https://hackage.haskell.org/package/" ++ packageName ++ "-" ++ version ++ "/" ++ packageName ++ "-" ++ version ++ ".tar.gz"
          let tarball = cacheDir </> packageName ++ "-" ++ version ++ ".tar.gz"
          let tempDir = cacheDir </> packageName ++ "-" ++ version ++ ".tmp"
          downloadFile url tarball
          extractTarball tarball tempDir
          removeFile tarball
          dirExists <- doesDirectoryExist pkgDir
          when dirExists $ removeDirectoryRecursive pkgDir
          renameDirectory tempDir pkgDir
          writeFile markerFile ""
          pure pkgDir

downloadFile :: String -> FilePath -> IO ()
downloadFile url dest =
  callCommand ("curl -s -f -o " ++ show dest ++ " " ++ show url)

extractTarball :: FilePath -> FilePath -> IO ()
extractTarball tarball destDir = do
  createDirectoryIfMissing True destDir
  callCommand ("tar -xzf " ++ show tarball ++ " -C " ++ show destDir)

getCacheDir :: IO FilePath
getCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

data FileInfo = FileInfo
  { fileInfoPath :: FilePath,
    fileInfoExtensions :: [Syntax.ExtensionSetting],
    fileInfoCppOptions :: [String],
    fileInfoLanguage :: Maybe Syntax.LanguageEdition
  }
  deriving (Show)

findTargetFilesFromCabal :: FilePath -> IO [FileInfo]
findTargetFilesFromCabal extractedRoot = do
  cabalFiles <- findCabalFiles extractedRoot
  cabalFile <-
    case cabalFiles of
      [file] -> pure file
      [] ->
        ioError
          ( userError
              ("No .cabal file found under extracted package root: " ++ extractedRoot)
          )
      files -> pure (chooseBestCabalFile extractedRoot files)
  cabalBytes <- BS.readFile cabalFile
  let (_, parseResult) = runParseResult (parseGenericPackageDescription cabalBytes)
  gpd <-
    case parseResult of
      Right parsed -> pure parsed
      Left (_, errs) ->
        ioError
          ( userError
              ("Failed to parse cabal file " ++ cabalFile ++ ": " ++ show errs)
          )
  collectComponentFiles gpd cabalFile

collectComponentFiles :: GenericPackageDescription -> FilePath -> IO [FileInfo]
collectComponentFiles gpd cabalFile = do
  let packageRoot = takeDirectory cabalFile
      evalCond = conditionEvaluator gpd
      -- We flatten the GPD to get a fixed set of components. This isn't perfect
      -- as it might pick branches that aren't usually active, but it's better
      -- than trying to resolve conditions without a proper environment.
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)

  -- We use the flattened PD to get the actual components but they don't
  -- easily map back to the files. Let's try to extract info from the CondTree
  -- but also carry the BuildInfo.

  libraryFiles <- fmap concat (forM libraryTrees (libraryFilesFor evalCond packageRoot))
  executableFiles <- fmap concat (forM executableTrees (executableFilesFor evalCond packageRoot))

  -- Dedupe by checking the path
  let allFiles = libraryFiles <> executableFiles
  pure (dedupeFiles allFiles)

dedupeFiles :: [FileInfo] -> [FileInfo]
dedupeFiles [] = []
dedupeFiles (f : fs) = f : dedupeFiles (filter (\x -> fileInfoPath x /= fileInfoPath f) fs)

libraryFilesFor :: (Condition ConfVar -> Bool) -> FilePath -> CondTree ConfVar c Library -> IO [FileInfo]
libraryFilesFor evalCond packageRoot tree = do
  let library = condTreeData tree
      build = collectMergedBuildInfo evalCond libBuildInfo tree
      moduleNames = exposedModules library <> otherModules build <> autogenModules build
      exts = extractExtensions build
      cppOpts = cppOptions build
      lang = extractLanguage build
  if not (buildable build)
    then pure []
    else do
      paths <- moduleFilesForBuildInfo packageRoot build moduleNames
      pure [FileInfo path exts cppOpts lang | path <- paths]

executableFilesFor :: (Condition ConfVar -> Bool) -> FilePath -> CondTree ConfVar c Executable -> IO [FileInfo]
executableFilesFor evalCond packageRoot tree = do
  let executable = condTreeData tree
      build = collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = modulePath executable
      exts = extractExtensions build
      cppOpts = cppOptions build
      lang = extractLanguage build
  if not (buildable build)
    then pure []
    else do
      moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
      mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build]
      pure [FileInfo path exts cppOpts lang | path <- moduleFiles <> mainFiles]

extractExtensions :: BuildInfo -> [Syntax.ExtensionSetting]
extractExtensions bi = mapMaybe (Syntax.parseExtensionSettingName . T.pack) (nub (map prettyShow (defaultExtensions bi <> oldExtensions bi)))

extractLanguage :: BuildInfo -> Maybe Syntax.LanguageEdition
extractLanguage bi =
  case defaultLanguage bi of
    Just lang -> Syntax.parseLanguageEdition (T.pack (prettyShow lang))
    Nothing -> Nothing

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

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  entries <- listDirectory dir
  paths <- fmap concat $
    forM entries $ \entry -> do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then
          if ".git" `isPrefixOf` entry
            then pure []
            else findCabalFiles fullPath
        else
          if ".cabal" `isSuffixOf` entry
            then pure [fullPath]
            else pure []
  pure (nub (map normalise paths))

existingPaths :: [FilePath] -> IO [FilePath]
existingPaths candidates = do
  existing <- forM candidates $ \candidate -> do
    fileExists <- doesFileExist candidate
    pure (if fileExists then Just (normalise candidate) else Nothing)
  pure (catMaybes existing)

dedupeExistingFiles :: [FilePath] -> IO [FilePath]
dedupeExistingFiles files = fmap nub (existingPaths files)

chooseBestCabalFile :: FilePath -> [FilePath] -> FilePath
chooseBestCabalFile extractedRoot files =
  case sortOn rank files of
    best : _ -> best
    [] -> error ("chooseBestCabalFile: no .cabal files found under " ++ extractedRoot)
  where
    rank file =
      let rel = splitDirectories (makeRelative extractedRoot file)
          dirParts = case reverse rel of
            _fileName : restRev -> reverse restRev
            [] -> []
          lowerDirParts = map (map toLower) dirParts
          isLikelyFixtureDir = any (`elem` fixtureDirNames) lowerDirParts
       in ( if isLikelyFixtureDir then (1 :: Int) else 0,
            length rel,
            length dirParts,
            scoreByFileName (map toLower (takeFileName file)),
            file
          )

    scoreByFileName fileNameLower
      | "test-" `isPrefixOf` fileNameLower = 1 :: Int
      | "example-" `isPrefixOf` fileNameLower = 1
      | otherwise = 0

    fixtureDirNames =
      [ "test",
        "tests",
        "testing",
        "example",
        "examples",
        "benchmark",
        "benchmarks"
      ]

readTextFileLenient :: FilePath -> IO Text
readTextFileLenient filePath = do
  bytes <- BS.readFile filePath
  pure (decodeUtf8With lenientDecode bytes)

resolveIncludeBestEffort :: FilePath -> FilePath -> IncludeRequest -> IO (Maybe BS.ByteString)
resolveIncludeBestEffort packageRoot currentFile req = do
  firstExisting <- firstExistingPath (includeCandidates packageRoot currentFile req)
  case firstExisting of
    Nothing -> pure Nothing
    Just includeFile -> Just <$> BS.readFile includeFile

includeCandidates :: FilePath -> FilePath -> IncludeRequest -> [FilePath]
includeCandidates packageRoot currentFile req =
  map normalise $ nub [dir </> includePath req | dir <- searchDirs]
  where
    includeDir = takeDirectory (includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      [ packageRoot </> "include",
        packageRoot </> "includes",
        packageRoot </> "cbits",
        packageRoot
      ]
    searchDirs =
      case includeKind req of
        IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (candidate : rest) = do
  let path = if isAbsolute candidate then candidate else normalise candidate
  exists <- doesFileExist path
  if exists
    then pure (Just path)
    else firstExistingPath rest

diagToText :: Diagnostic -> Text
diagToText diag =
  T.pack (diagFile diag)
    <> ":"
    <> T.pack (show (diagLine diag))
    <> ": "
    <> sev
    <> ": "
    <> diagMessage diag
  where
    sev =
      case diagSeverity diag of
        Warning -> "warning"
        Error -> "error"

prefixCppErrors :: Maybe Text -> Text -> Text
prefixCppErrors cppMsg msg =
  case cppMsg of
    Nothing -> msg
    Just cppText -> "cpp diagnostics:\n" <> cppText <> "\n" <> msg
