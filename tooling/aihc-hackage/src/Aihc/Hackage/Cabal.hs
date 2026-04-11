-- | Cabal-file parsing utilities: condition evaluation, component file discovery.
module Aihc.Hackage.Cabal
  ( -- * File info
    FileInfo (..),

    -- * Component file discovery
    collectComponentFiles,

    -- * Condition evaluation
    conditionEvaluator,
    collectCondTreeData,
    collectMergedBuildInfo,

    -- * Extension / language extraction
    extractExtensions,
    extractLanguage,
    extractDependencies,
  )
where

import Aihc.Hackage.Util (existingPaths, moduleFilesForBuildInfo, sourceDirs)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version qualified as DV
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package (unPackageName)
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
    libBuildInfo,
    modulePath,
    oldExtensions,
    otherModules,
  )
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.CondTree
  ( CondBranch (CondBranch),
    CondTree (condTreeComponents, condTreeData),
  )
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags)
import Distribution.Version (mkVersion, withinRange)
import System.FilePath ((</>))
import System.Info (compilerName, compilerVersion)

-- | Information about a Haskell source file discovered via a @.cabal@ file.
data FileInfo = FileInfo
  { fileInfoPath :: FilePath,
    -- | Extension names as strings (e.g. @\"OverloadedStrings\"@, @\"NoImplicitPrelude\"@).
    fileInfoExtensions :: [String],
    -- | CPP options from the @cpp-options@ field.
    fileInfoCppOptions :: [String],
    -- | Default language from the @default-language@ field.
    fileInfoLanguage :: Maybe String,
    -- | Build dependency package names.
    fileInfoDependencies :: [Text]
  }
  deriving (Show)

-- | Collect all source files from a parsed @GenericPackageDescription@.
--
-- Returns deduplicated 'FileInfo' records for every library and executable
-- component whose @buildable@ flag is true.
collectComponentFiles :: GenericPackageDescription -> FilePath -> IO [FileInfo]
collectComponentFiles gpd packageRoot = do
  let evalCond = conditionEvaluator gpd
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)

  libraryFiles <- fmap concat (mapM (libraryFilesFor evalCond packageRoot) libraryTrees)
  executableFiles <- fmap concat (mapM (executableFilesFor evalCond packageRoot) executableTrees)

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
      deps = extractDependencies build
  if not (buildable build)
    then pure []
    else do
      paths <- moduleFilesForBuildInfo packageRoot build moduleNames
      pure [FileInfo path exts cppOpts lang deps | path <- paths]

executableFilesFor :: (Condition ConfVar -> Bool) -> FilePath -> CondTree ConfVar c Executable -> IO [FileInfo]
executableFilesFor evalCond packageRoot tree = do
  let executable = condTreeData tree
      build = collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = modulePath executable
      exts = extractExtensions build
      cppOpts = cppOptions build
      lang = extractLanguage build
      deps = extractDependencies build
  if not (buildable build)
    then pure []
    else do
      moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
      mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build]
      pure [FileInfo path exts cppOpts lang deps | path <- moduleFiles <> mainFiles]

-- | Evaluate cabal conditions using the host compiler and default flag values.
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

-- | Collect all data nodes from a 'CondTree', evaluating conditions.
collectCondTreeData :: (Condition v -> Bool) -> CondTree v c a -> [a]
collectCondTreeData evalCond tree =
  condTreeData tree : concatMap collectBranch (condTreeComponents tree)
  where
    collectBranch (CondBranch cond thenTree elseTree) =
      if evalCond cond
        then collectCondTreeData evalCond thenTree
        else maybe [] (collectCondTreeData evalCond) elseTree

-- | Merge 'BuildInfo' from all active branches of a 'CondTree'.
collectMergedBuildInfo :: (Monoid b) => (Condition v -> Bool) -> (a -> b) -> CondTree v c a -> b
collectMergedBuildInfo evalCond toBuildInfo =
  mconcat . map toBuildInfo . collectCondTreeData evalCond

-- | Extract extension names as strings from a 'BuildInfo'.
extractExtensions :: BuildInfo -> [String]
extractExtensions bi = nub (map prettyShow (defaultExtensions bi <> oldExtensions bi))

-- | Extract the default language as a string from a 'BuildInfo'.
extractLanguage :: BuildInfo -> Maybe String
extractLanguage bi =
  case defaultLanguage bi of
    Just lang -> Just (prettyShow lang)
    Nothing -> Nothing

-- | Extract build dependency package names from a 'BuildInfo'.
extractDependencies :: BuildInfo -> [Text]
extractDependencies bi =
  map (T.pack . unPackageName . depPkgName) (targetBuildDepends bi)
