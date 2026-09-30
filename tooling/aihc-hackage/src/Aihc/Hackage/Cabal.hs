-- | Cabal-file parsing utilities: condition evaluation, component file discovery.
module Aihc.Hackage.Cabal
  ( -- * File info
    FileInfo (..),
    CCompileInfo (..),

    -- * Component file discovery
    ExecutableInfo (..),
    lirSourcesField,
    collectComponentFiles,
    collectExecutablesFor,
    collectExecutablesIn,
    collectLibraryCCompileInfo,
    collectLibraryCCompileInfoFor,
    collectLibraryCCompileInfoIn,
    collectLibraryExposedModules,
    collectLibraryExposedModulesIn,
    collectLibraryFiles,
    collectLibraryFilesFor,
    collectLibraryFilesIn,
    installedLibraryTrees,

    -- * Condition evaluation
    BuildContext (..),
    hostBuildContext,
    buildContextFor,
    conditionEvaluator,
    conditionEvaluatorFor,
    conditionEvaluatorIn,
    packageFlagAssignment,
    targetFlagOverrides,
    collectCondTreeData,
    collectMergedBuildInfo,
    isBuildable,

    -- * Configure build type
    BuildType (..),
    packageBuildType,
    collectLibraryAutogenIncludesFor,
    collectLibraryAutogenIncludesIn,
    applyHookedBuildInfo,
    prependIncludeDirs,

    -- * Build tool dependency extraction
    buildToolDependencyNames,
    packageUsesCustomPreprocessor,

    -- * Preprocessors
    Preprocessor (..),
    filePreprocessor,

    -- * Extension / language extraction
    extractExtensions,
    extractLanguage,
    extractDependencies,
    packageDefaultsToHaskell98,
  )
where

import Aihc.Cabal
  ( Branch (..),
    BuildInfo,
    Component (..),
    ComponentKind (..),
    Condition (..),
    Conditional (..),
    Dependency (..),
    HookedBuildInfo (..),
    LibraryTarget (..),
    Package,
    ToolDependency (..),
    autogenIncludes,
    autogenModules,
    buildTools,
    buildable,
    cSources,
    cabalVersion,
    ccOptions,
    cppOptions,
    cxxOptions,
    cxxSources,
    defaultLanguage,
    dependencies,
    emptyBuildInfo,
    exposedModules,
    extensions,
    extraFields,
    fieldPaths,
    flagDefault,
    flagName,
    ghcOptions,
    includeDirs,
    installIncludes,
    legacyExtensions,
    mainIs,
    mergeBuildInfo,
    otherModules,
    packageComponents,
    packageFlags,
    packageVersion,
    renderDiagnostic,
  )
import Aihc.Cabal qualified as Cabal
import Aihc.Hackage.Package
  ( Arch (..),
    FlagAssignment,
    FlagName,
    OS,
    PackageName,
    Version,
    archName,
    buildArch,
    buildOS,
    mkFlagAssignment,
    mkFlagName,
    mkPackageName,
    osName,
    packageNameOf,
    unFlagAssignment,
    unPackageName,
    versionFromList,
  )
import Aihc.Hackage.PathsModule (generatePathsModule, pathsModuleName)
import Aihc.Hackage.Preprocessor (Preprocessor (..), preprocessorForExtension)
import Aihc.Hackage.Release (GhcRelease (..), emulatedGhc)
import Aihc.Hackage.Util (existingPaths, moduleFilesForBuildInfo, moduleNameFilePath, sourceDirs)
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))

-- | Information about a Haskell source file discovered via a @.cabal@ file.
data FileInfo = FileInfo
  { fileInfoPath :: FilePath,
    -- | Extension names as strings (e.g. @\"OverloadedStrings\"@, @\"NoImplicitPrelude\"@).
    fileInfoExtensions :: [String],
    -- | CPP options from the @cpp-options@ field.
    fileInfoCppOptions :: [String],
    -- | Include search directories from the @include-dirs@ field.
    fileInfoIncludeDirs :: [FilePath],
    -- | Default language from the @default-language@ field.
    fileInfoLanguage :: Maybe String,
    -- | Build dependency package names.
    fileInfoDependencies :: [Text],
    -- | The tool that turns the file into Haskell, when its suffix names one.
    -- The file at 'fileInfoPath' is that tool's input; a plain Haskell
    -- source has none.
    fileInfoPreprocessor :: Maybe Preprocessor
  }
  deriving (Show)

-- | The preprocessor a source file's suffix selects.
filePreprocessor :: FilePath -> Maybe Preprocessor
filePreprocessor path = preprocessorForExtension (drop 1 (takeExtension path))

-- | C compile inputs from the active library @c-sources@, @cxx-sources@,
-- @include-dirs@, @cc-options@, and @cxx-options@ fields.
data CCompileInfo = CCompileInfo
  { cCompileSources :: [FilePath],
    -- | The @cxx-sources@ of the package. They are compiled as C++ with
    -- the @cxx-options@, and a package that has any links the C++ standard
    -- library into every executable that depends on it.
    cCompileCxxSources :: [FilePath],
    -- | The Lir units of the package, from the aihc-specific field
    -- @x-aihc-lir-sources@. They are compiled with the Lir backend of the
    -- target and their objects join the C objects of the package.
    cCompileLirSources :: [FilePath],
    cCompileIncludeDirs :: [FilePath],
    -- | Public headers, relative to the include directories.
    cCompileInstallIncludes :: [FilePath],
    cCompileCcOptions :: [String],
    cCompileCxxOptions :: [String]
  }
  deriving (Eq, Show)

-- | Collect all source files from a parsed package description.
--
-- Returns deduplicated 'FileInfo' records for every library and executable
-- component whose @buildable@ flag is true.
collectComponentFiles :: Package -> FilePath -> IO [FileInfo]
collectComponentFiles package packageRoot = do
  libraryFiles <- collectLibraryFiles package packageRoot
  executableFiles <- collectExecutableFiles package packageRoot
  pure (dedupeFiles (libraryFiles <> executableFiles))

-- | Collect source files from buildable library components only.
collectLibraryFiles :: Package -> FilePath -> IO [FileInfo]
collectLibraryFiles = collectLibraryFilesFor buildOS buildArch

-- | Collect source files from buildable library components for one platform.
collectLibraryFilesFor :: OS -> Arch -> Package -> FilePath -> IO [FileInfo]
collectLibraryFilesFor os arch = collectLibraryFilesIn (buildContextFor os arch)

-- | Collect source files from buildable library components under the
-- conditions of one build context.
collectLibraryFilesIn :: BuildContext -> Package -> FilePath -> IO [FileInfo]
collectLibraryFilesIn context package packageRoot = do
  let evalCond = conditionEvaluatorIn context package
      libraryTrees = installedLibraryTrees evalCond package
  libraryFiles <- fmap concat (mapM (uncurry (libraryFilesFor package evalCond packageRoot)) libraryTrees)
  pure (dedupeFiles libraryFiles)

-- | Collect C compile inputs from buildable library components for the host.
-- The result is an error when a field of the inputs is not valid.
collectLibraryCCompileInfo :: Package -> FilePath -> Either String CCompileInfo
collectLibraryCCompileInfo = collectLibraryCCompileInfoFor buildOS buildArch

-- | Collect C compile inputs from buildable library components for one platform.
collectLibraryCCompileInfoFor :: OS -> Arch -> Package -> FilePath -> Either String CCompileInfo
collectLibraryCCompileInfoFor os arch = collectLibraryCCompileInfoIn (buildContextFor os arch)

-- | Collect C compile inputs from buildable library components under the
-- conditions of one build context.
collectLibraryCCompileInfoIn :: BuildContext -> Package -> FilePath -> Either String CCompileInfo
collectLibraryCCompileInfoIn context package packageRoot =
  mergeCCompileInfo
    <$> sequence
      [ cCompileInfoFromBuild (cabalVersion package) packageRoot build
      | build <- activeLibraryBuildInfos context package
      ]

-- | The merged build information of each buildable library component that
-- an install builds.
activeLibraryBuildInfos :: BuildContext -> Package -> [BuildInfo]
activeLibraryBuildInfos context package =
  [ build
  | (_, tree) <- installedLibraryTrees evalCond package,
    let build = collectMergedBuildInfo evalCond tree,
    isBuildable build
  ]
  where
    evalCond = conditionEvaluatorIn context package

-- | The C compile inputs of one build information. The Cabal format version
-- gives the list rules of the Lir source field.
cCompileInfoFromBuild :: Version -> FilePath -> BuildInfo -> Either String CCompileInfo
cCompileInfoFromBuild spec packageRoot build = do
  lirSources <- extractLirSources spec packageRoot build
  pure
    CCompileInfo
      { cCompileSources = extractCSources packageRoot build,
        cCompileCxxSources = extractCxxSources packageRoot build,
        cCompileLirSources = lirSources,
        cCompileIncludeDirs = extractIncludeDirs packageRoot build,
        cCompileInstallIncludes = installIncludes build,
        cCompileCcOptions = map T.unpack (ccOptions build),
        cCompileCxxOptions = map T.unpack (cxxOptions build)
      }

mergeCCompileInfo :: [CCompileInfo] -> CCompileInfo
mergeCCompileInfo items =
  CCompileInfo
    { cCompileSources = nub (concatMap cCompileSources items),
      cCompileCxxSources = nub (concatMap cCompileCxxSources items),
      cCompileLirSources = nub (concatMap cCompileLirSources items),
      cCompileIncludeDirs = nub (concatMap cCompileIncludeDirs items),
      cCompileInstallIncludes = nub (concatMap cCompileInstallIncludes items),
      cCompileCcOptions = concatMap cCompileCcOptions items,
      cCompileCxxOptions = concatMap cCompileCxxOptions items
    }

-- | How a package is built.
data BuildType = Simple | Configure | Custom | Make | Hooks
  deriving (Eq, Show)

-- | The build type of a package. A missing @build-type@ field defaults the
-- way Cabal defaults it: @Simple@ from @cabal-version@ 2.2, @Custom@
-- before it, and @Custom@ when the file has a @custom-setup@ stanza.
packageBuildType :: Package -> BuildType
packageBuildType package =
  case T.unpack (Cabal.buildType package) of
    "Configure" -> Configure
    "Custom" -> Custom
    "Make" -> Make
    "Hooks" -> Hooks
    _ -> Simple

-- | The headers the active library components declare as @autogen-includes@
-- for one platform: the files a configure script is expected to write. The
-- paths are relative to the include directories.
collectLibraryAutogenIncludesFor :: OS -> Arch -> Package -> [FilePath]
collectLibraryAutogenIncludesFor os arch = collectLibraryAutogenIncludesIn (buildContextFor os arch)

-- | The @autogen-includes@ of the active library components under the
-- conditions of one build context.
collectLibraryAutogenIncludesIn :: BuildContext -> Package -> [FilePath]
collectLibraryAutogenIncludesIn context package =
  nub [path | build <- activeLibraryBuildInfos context package, path <- autogenIncludes build]

-- | Apply the library build information from @<package>.buildinfo@.
-- Read public headers, include directories, C sources, and C and CPP options.
-- Resolve relative paths from the configure output directory.
-- Ignore fields without a consumer, such as @extra-libraries@.
-- The Cabal format version is the version of the package.
applyHookedBuildInfo :: Version -> FilePath -> HookedBuildInfo -> [FileInfo] -> CCompileInfo -> Either String ([FileInfo], CCompileInfo)
applyHookedBuildInfo spec buildRoot hooked files cInfo =
  case hookedLibrary hooked of
    Nothing -> Right (files, cInfo)
    Just build -> do
      hookedInfo <- cCompileInfoFromBuild spec buildRoot build
      pure (map (overlayFile build) files, mergeCCompileInfo [hookedInfo, cInfo])
  where
    overlayFile build file =
      file
        { fileInfoCppOptions = fileInfoCppOptions file <> map T.unpack (cppOptions build),
          fileInfoIncludeDirs = nub (extractIncludeDirs buildRoot build <> fileInfoIncludeDirs file)
        }

-- | Search the given directories before the include directories of a file.
prependIncludeDirs :: [FilePath] -> FileInfo -> FileInfo
prependIncludeDirs directories file =
  file {fileInfoIncludeDirs = nub (directories <> fileInfoIncludeDirs file)}

-- | Collect the public module interface selected by active Cabal conditions.
-- Private @other-modules@ are intentionally absent even though
-- 'collectLibraryFiles' includes their source files for compilation.
collectLibraryExposedModules :: Package -> [Text]
collectLibraryExposedModules = collectLibraryExposedModulesIn hostBuildContext

-- | The exposed modules of the active library components under the
-- conditions of one build context.
collectLibraryExposedModulesIn :: BuildContext -> Package -> [Text]
collectLibraryExposedModulesIn context package =
  nub [moduleName | build <- activeLibraryBuildInfos context package, moduleName <- exposedModules build]

-- | The library components that an install builds under the active
-- conditions. These are the main library and each sub-library that the main
-- library or an executable uses, directly or through a different
-- sub-library. Cabal builds only these components of a dependency, and a
-- sub-library that nothing uses can have dependencies that the plan does not
-- have (the @benchmarks-O2@ library of vector depends on tasty). A package
-- without a main library and without executables gives all its
-- sub-libraries. The main library has no name.
installedLibraryTrees :: (Condition -> Bool) -> Package -> [(Maybe Text, Conditional BuildInfo)]
installedLibraryTrees evalCond package
  | null mainLibrary && null executables = subLibraries
  | otherwise = mainLibrary <> filter (maybe False (`Set.member` used) . fst) subLibraries
  where
    self = Cabal.packageName package
    mainLibrary = [(Nothing, tree) | Component (Library MainLibrary) tree <- packageComponents package]
    subLibraries = [(Just name, tree) | Component (Library (NamedLibrary name)) tree <- packageComponents package]
    executables = [tree | Component (Executable _) tree <- packageComponents package]
    subLibraryTrees = Map.fromList [(name, tree) | (Just name, tree) <- subLibraries]
    rootDependencies = concatMap (activeDependencies . snd) mainLibrary <> concatMap activeDependencies executables
    used = reach Set.empty (ownLibraries rootDependencies)
    reach seen [] = seen
    reach seen (name : rest)
      | name `Set.member` seen = reach seen rest
      | otherwise =
          reach
            (Set.insert name seen)
            (rest <> maybe [] (ownLibraries . activeDependencies) (Map.lookup name subLibraryTrees))
    activeDependencies tree =
      let build = collectMergedBuildInfo evalCond tree
       in if isBuildable build then dependencies build else []
    -- The Cabal parser rewrites a dependency on an internal library name to
    -- a dependency on this package, so this finds each use.
    ownLibraries deps =
      [ library
      | dependency <- deps,
        dependencyPackage dependency == self,
        NamedLibrary library <- NE.toList (dependencyLibraries dependency)
      ]

collectExecutableFiles :: Package -> FilePath -> IO [FileInfo]
collectExecutableFiles package packageRoot = do
  let evalCond = conditionEvaluator package
  executableFiles <-
    fmap
      concat
      (sequence [executableFilesFor package evalCond packageRoot name tree | Component (Executable name) tree <- packageComponents package])
  pure (dedupeFiles executableFiles)

-- | One buildable executable of a package, as the active conditions of one
-- platform select it.
data ExecutableInfo = ExecutableInfo
  { executableInfoName :: String,
    -- | The sources of the executable: its @main-is@ file, its
    -- @other-modules@, and the generated @Paths_@ module when it lists one.
    executableInfoFiles :: [FileInfo],
    -- | The packages in the @build-depends@ of the executable.
    executableInfoDependencies :: [PackageName],
    -- | The @c-sources@, @include-dirs@, and @cc-options@ of the executable.
    executableInfoCCompileInfo :: CCompileInfo
  }
  deriving (Show)

-- | The buildable executables of a package for one platform, in the order
-- the Cabal file declares them.
collectExecutablesFor :: OS -> Arch -> Package -> FilePath -> IO [ExecutableInfo]
collectExecutablesFor os arch = collectExecutablesIn (buildContextFor os arch)

-- | The buildable executables of a package under the conditions of one
-- build context, in the order the Cabal file declares them.
collectExecutablesIn :: BuildContext -> Package -> FilePath -> IO [ExecutableInfo]
collectExecutablesIn context package packageRoot =
  concat <$> sequence [executableInfo name tree | Component (Executable name) tree <- packageComponents package]
  where
    evalCond = conditionEvaluatorIn context package
    executableInfo exeName tree = do
      let build = collectMergedBuildInfo evalCond tree
      files <- executableFilesFor package evalCond packageRoot exeName tree
      cInfo <- either (ioError . userError) pure (cCompileInfoFromBuild (cabalVersion package) packageRoot build)
      pure
        [ ExecutableInfo
            { executableInfoName = T.unpack exeName,
              executableInfoFiles = files,
              executableInfoDependencies = [mkPackageName (T.unpack (dependencyPackage dependency)) | dependency <- dependencies build],
              executableInfoCCompileInfo = cInfo
            }
        | isBuildable build
        ]

-- | Keep the first 'FileInfo' of each path, in order.
--
-- The paths of one package share a long directory prefix, so comparing them
-- pairwise costs far more than the list length suggests: a set keyed on the
-- path keeps this linear.
dedupeFiles :: [FileInfo] -> [FileInfo]
dedupeFiles = go Set.empty
  where
    go _ [] = []
    go seen (f : fs)
      | Set.member (fileInfoPath f) seen = go seen fs
      | otherwise = f : go (Set.insert (fileInfoPath f) seen) fs

libraryFilesFor :: Package -> (Condition -> Bool) -> FilePath -> Maybe Text -> Conditional BuildInfo -> IO [FileInfo]
libraryFilesFor package evalCond packageRoot libName tree = do
  let build = collectMergedBuildInfo evalCond tree
      moduleNames = nub (exposedModules build <> otherModules build <> autogenModules build)
  if not (isBuildable build)
    then pure []
    else do
      paths <- moduleFilesForBuildInfo packageRoot build moduleNames
      generatedPaths <- generatedPathsFiles packageRoot package (maybe "" (("-lib-" <>) . T.unpack) libName) moduleNames
      pure $
        [sourceFileInfo packageRoot build path | path <- paths]
          <> [generatedPathsFileInfo path | path <- generatedPaths]

executableFilesFor :: Package -> (Condition -> Bool) -> FilePath -> Text -> Conditional BuildInfo -> IO [FileInfo]
executableFilesFor package evalCond packageRoot exeName tree = do
  let build = collectMergedBuildInfo evalCond tree
      moduleNames = nub (otherModules build <> autogenModules build)
  if not (isBuildable build)
    then pure []
    else do
      moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
      mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build, mainPath <- maybe [] pure (mainIs build)]
      generatedPaths <- generatedPathsFiles packageRoot package ("-exe-" <> T.unpack exeName) moduleNames
      pure $
        [sourceFileInfo packageRoot build path | path <- moduleFiles <> mainFiles]
          <> [generatedPathsFileInfo path | path <- generatedPaths]

sourceFileInfo :: FilePath -> BuildInfo -> FilePath -> FileInfo
sourceFileInfo packageRoot build path =
  FileInfo
    { fileInfoPath = path,
      fileInfoExtensions = extractExtensions build,
      fileInfoCppOptions = map T.unpack (cppOptions build),
      fileInfoIncludeDirs = extractIncludeDirs packageRoot build,
      fileInfoLanguage = extractLanguage build,
      fileInfoDependencies = extractDependencies build,
      fileInfoPreprocessor = filePreprocessor path
    }

-- | Collect package and executable names referenced by active build-tool fields.
buildToolDependencyNames :: Package -> [Text]
buildToolDependencyNames package =
  nub $
    concatMap buildInfoToolNames (activeComponentBuildInfos package)

-- | Return whether any active source component uses Cabal's Haskell98 default.
--
-- Cabal treats a missing @default-language@ as Haskell98. AIHC does not support
-- Haskell98 as a package language target, so progress tooling filters these
-- packages before parsing their files.
packageDefaultsToHaskell98 :: Package -> Bool
packageDefaultsToHaskell98 =
  any buildInfoDefaultsToHaskell98 . activeSourceComponentBuildInfos

buildInfoDefaultsToHaskell98 :: BuildInfo -> Bool
buildInfoDefaultsToHaskell98 bi =
  case defaultLanguage bi of
    Nothing -> True
    Just lang -> lang == T.pack "Haskell98"

activeSourceComponentBuildInfos :: Package -> [BuildInfo]
activeSourceComponentBuildInfos package =
  [ collectMergedBuildInfo evalCond tree
  | Component kind tree <- packageComponents package,
    isSource kind
  ]
  where
    evalCond = conditionEvaluator package
    isSource kind =
      case kind of
        Library _ -> True
        Executable _ -> True
        _ -> False

-- | Return whether any active component requests a custom GHC preprocessor.
packageUsesCustomPreprocessor :: Package -> Bool
packageUsesCustomPreprocessor package =
  any buildInfoUsesCustomPreprocessor (activeComponentBuildInfos package)

activeComponentBuildInfos :: Package -> [BuildInfo]
activeComponentBuildInfos package =
  [collectMergedBuildInfo evalCond tree | Component _ tree <- packageComponents package]
  where
    evalCond = conditionEvaluator package

buildInfoToolNames :: BuildInfo -> [Text]
buildInfoToolNames bi =
  concat [maybe [toolName tool] (\toolPackageName -> [toolPackageName, toolName tool]) (toolPackage tool) | tool <- buildTools bi]

buildInfoUsesCustomPreprocessor :: BuildInfo -> Bool
buildInfoUsesCustomPreprocessor bi =
  ghcOptionsUseCustomPreprocessor (map T.unpack (ghcOptions bi))

ghcOptionsUseCustomPreprocessor :: [String] -> Bool
ghcOptionsUseCustomPreprocessor opts =
  "-F" `elem` opts && any isPgmFOption opts
  where
    isPgmFOption opt = "-pgmF" `isPrefixOf` opt

generatedPathsFileInfo :: FilePath -> FileInfo
generatedPathsFileInfo path =
  FileInfo
    { fileInfoPath = path,
      fileInfoExtensions = [],
      fileInfoCppOptions = [],
      fileInfoIncludeDirs = [],
      fileInfoLanguage = Nothing,
      fileInfoDependencies = [T.pack "base"],
      fileInfoPreprocessor = Nothing
    }

-- | Write the @Paths_@ module of a component when the component lists it.
-- The suffix names the component in the unit name, as Cabal does.
generatedPathsFiles :: FilePath -> Package -> String -> [Text] -> IO [FilePath]
generatedPathsFiles packageRoot package componentSuffix moduleNames
  | T.pack moduleName `notElem` moduleNames = pure []
  | otherwise = do
      let path = packageRoot </> ".aihc-autogen" </> moduleNameFilePath (T.pack moduleName) <.> "hs"
          version = packageVersion package
          packageId = unPackageName (packageNameOf package) <> "-" <> showPackageVersion
          showPackageVersion = T.unpack (Cabal.renderVersion version)
      createDirectoryIfMissing True (takeDirectory path)
      writeFile
        path
        ( generatePathsModule
            (buildOS, buildArch)
            compilerMajorMinor
            (unPackageName (packageNameOf package))
            version
            (packageId <> componentSuffix)
        )
      pure [path]
  where
    moduleName = pathsModuleName (unPackageName (packageNameOf package))
    compilerMajorMinor =
      case releaseCompilerVersion emulatedGhc of
        major : minor : _ -> (major, minor)
        major : _ -> (major, 0)
        [] -> (0, 0)

-- | What closes the conditions of a Cabal file: the platform the package is
-- built for and the flags the plan decided. Flags the plan did not decide
-- take their defaults.
data BuildContext = BuildContext
  { contextOs :: !OS,
    contextArch :: !Arch,
    -- | The flags the dependency plan decided for the package, on top of
    -- the defaults and the target's overrides.
    contextFlags :: !FlagAssignment
  }
  deriving (Eq, Show)

-- | The host platform with every flag at its default.
hostBuildContext :: BuildContext
hostBuildContext = buildContextFor buildOS buildArch

-- | One platform with every flag at its default.
buildContextFor :: OS -> Arch -> BuildContext
buildContextFor os arch = BuildContext os arch (mkFlagAssignment [])

-- | Evaluate cabal conditions using the emulated compiler and default flag values.
conditionEvaluator :: Package -> Condition -> Bool
conditionEvaluator = conditionEvaluatorIn hostBuildContext

-- | Evaluate cabal conditions for one OS and architecture.
conditionEvaluatorFor :: Package -> OS -> Arch -> Condition -> Bool
conditionEvaluatorFor package os arch = conditionEvaluatorIn (buildContextFor os arch) package

-- | The value of every flag of a package under a build context: the
-- default, unless the target overrides it or the context decided it.
packageFlagAssignment :: BuildContext -> Package -> Map.Map FlagName Bool
packageFlagAssignment context package =
  Map.unions
    [ Map.fromList (unFlagAssignment (contextFlags context)),
      Map.fromList (targetFlagOverrides (contextArch context) (packageNameOf package)),
      Map.fromList [(flagName flag, flagDefault flag) | flag <- packageFlags package]
    ]

-- | Evaluate cabal conditions under one build context.
--
-- "Aihc.Cabal" compares the names with the aliases of Cabal, so
-- @os(darwin)@ is true on macOS, and @arch(arm64)@ is false, as in Cabal.
conditionEvaluatorIn :: BuildContext -> Package -> Condition -> Bool
conditionEvaluatorIn context package =
  Cabal.evaluateCondition environment (packageFlagAssignment context package)
  where
    -- aihc presents itself as the GHC release in "Aihc.Hackage.Release", the
    -- same one the CPP macros describe; the host compiler is irrelevant.
    environment =
      Cabal.Environment
        { Cabal.targetOS = T.pack (osName (contextOs context)),
          Cabal.targetArch = T.pack (archName (contextArch context)),
          Cabal.compiler = T.pack "ghc",
          Cabal.compilerVersion = versionFromList (releaseCompilerVersion emulatedGhc)
        }

-- | The flags of a package a target sets away from their defaults.
--
-- aihc has no way to ask for a flag on the command line, so the few flags a
-- target cannot take at their default are listed here. Both entries are
-- @text@ on the WebAssembly target.
--
-- The target has a libc but no C++ standard library in its sysroot, and
-- @text@ enables its @cxx-sources@ (the simdutf validator) on every
-- architecture but JavaScript, so @simdutf@ is turned off there.
--
-- @pure-haskell@ replaces the rest of the C routines with Haskell ones.
-- @text@ calls them through @size_t@, which is four bytes on wasm32 while an
-- @Int@ is eight (see 'Aihc.Hackage.Headers.haskellWordBytes'), a combination
-- no GHC platform has. @Data.Text.length@ is
-- @negate . measureOff maxBound@, and @measureOff@ hands that bound to
-- @_hs_text_measure_off@ as a @CSize@: on wasm32 @fromIntegral (maxBound ::
-- Int)@ wraps to @0xffffffff@, the C routine subtracts the characters it
-- measured from it, and the difference is no longer a count, so @length@
-- answers 1 for every non-empty string. The Haskell routines count
-- characters directly and never depend on that bound fitting, so the target
-- takes them.
targetFlagOverrides :: Arch -> PackageName -> [(FlagName, Bool)]
targetFlagOverrides arch name
  | arch == Wasm32 && name == mkPackageName "text" =
      [(mkFlagName "simdutf", False), (mkFlagName "pure-haskell", True)]
  | otherwise = []

-- | Collect the data of the active branches of a conditional tree, in
-- source order.
collectCondTreeData :: (Condition -> Bool) -> Conditional a -> [a]
collectCondTreeData evalCond tree =
  unconditionalData : concatMap collectBranch (branches tree)
  where
    unconditionalData = unconditional tree
    collectBranch (Branch cond thenTree elseTree) =
      if evalCond cond
        then collectCondTreeData evalCond thenTree
        else maybe [] (collectCondTreeData evalCond) elseTree

-- | Merge the 'BuildInfo' of all active branches of a conditional tree.
collectMergedBuildInfo :: (Condition -> Bool) -> Conditional BuildInfo -> BuildInfo
collectMergedBuildInfo evalCond =
  foldl mergeBuildInfo emptyBuildInfo . collectCondTreeData evalCond

-- | A component is buildable unless a @buildable: False@ field is active.
isBuildable :: BuildInfo -> Bool
isBuildable = fromMaybe True . buildable

-- | Extract extension names as strings from a 'BuildInfo'.
extractExtensions :: BuildInfo -> [String]
extractExtensions bi = nub (map T.unpack (extensions bi <> legacyExtensions bi))

-- | Extract the default language as a string from a 'BuildInfo'.
extractLanguage :: BuildInfo -> Maybe String
extractLanguage bi = T.unpack <$> defaultLanguage bi

-- | Extract include search directories from a 'BuildInfo'.
extractIncludeDirs :: FilePath -> BuildInfo -> [FilePath]
extractIncludeDirs packageRoot bi =
  nub [packageRoot </> dir | dir <- includeDirs bi]

-- | Extract C source paths from a 'BuildInfo'.
extractCSources :: FilePath -> BuildInfo -> [FilePath]
extractCSources packageRoot bi =
  nub [packageRoot </> path | path <- cSources bi]

-- | Extract C++ source paths from a 'BuildInfo'.
extractCxxSources :: FilePath -> BuildInfo -> [FilePath]
extractCxxSources packageRoot bi =
  nub [packageRoot </> path | path <- cxxSources bi]

-- | The field naming the Lir units of a component. The parser keeps a field
-- it does not know, so the units are listed like @c-sources@ and read from
-- here with the list rules of @c-sources@.
lirSourcesField :: String
lirSourcesField = "x-aihc-lir-sources"

-- | Extract the Lir unit paths from a 'BuildInfo'. The field holds paths
-- separated by whitespace or commas, as @c-sources@ does.
extractLirSources :: Version -> FilePath -> BuildInfo -> Either String [FilePath]
extractLirSources spec packageRoot bi =
  case mapM (fieldPaths spec) (Map.findWithDefault [] (T.pack lirSourcesField) (extraFields bi)) of
    Right paths -> Right (nub [packageRoot </> path | path <- concat paths])
    Left diagnostic -> Left ("Invalid " <> lirSourcesField <> " field: " <> T.unpack (renderDiagnostic diagnostic))

-- | Extract build dependency package names from a 'BuildInfo'.
extractDependencies :: BuildInfo -> [Text]
extractDependencies bi =
  map dependencyPackage (dependencies bi)
