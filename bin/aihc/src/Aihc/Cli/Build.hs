{-# LANGUAGE OverloadedStrings #-}

-- | The @build@ command.
--
-- A Haskell source file is the main module of one executable, which
-- "Aihc.Cli.BuildModule" builds from the source directories and package
-- constraints of the command line. Anything else is a Cabal package: a local
-- directory, or a Hackage release named as @install@ names it. Its
-- executables are found in the Cabal file, and each is built from the
-- sources and the @build-depends@ its own stanza declares. The library of
-- the package, when an executable depends on it, is installed like any
-- other dependency: in place under the build directory for a local package,
-- and into the store for a Hackage release.
module Aihc.Cli.Build
  ( build,
    buildWith,
    runBuild,
  )
where

import Aihc.Cli.BuildModule
  ( ExecutableInputs (..),
    InstalledPackage (..),
    entryModuleText,
    finishExecutable,
    installedPackage,
    plannedPackage,
    requirePackageArchive,
    runBuildModule,
    validateSelectedPackageNames,
  )
import Aihc.Cli.CompilerHeaders (ensureCompilerHeaders)
import Aihc.Cli.Hackage (defaultHackageSource)
import Aihc.Cli.Install
  ( InstallLocations (..),
    ModuleCompileConfig (..),
    ModuleCompileRequest (..),
    buildEnvironmentIdentity,
    cabalPlatformForTarget,
    capiStubOptions,
    compileModules,
    compilePackageCFiles,
    defaultBuildRoot,
    dependencyIncludeDirs,
    installPlanPackages,
    installTargetRoot,
    packageLinkArguments,
    planProgressItems,
    planRequestFor,
    sourceFileModuleName,
  )
import Aihc.Cli.OptimizationPlan (OptimizationPlan (..), optimizationPlan)
import Aihc.Cli.Options (BuildOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.Progress (ProgressEvent (..), ProgressItem (..), ProgressReporter (..), quietProgress, withProgress)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Hackage.Cabal (ExecutableInfo (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Package (mkPackageName, unPackageName)
import Aihc.Native (NativeTarget (..), nativeTargetStoreDirectory)
import Aihc.PackagePlan
  ( PackagePlan (..),
    PlanOrigin (..),
    PlanRequest (..),
    PlannedPackages (..),
    planBuildContext,
    planPackages,
  )
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Monad (forM, forM_, unless, when)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (stderr, stdout)

-- | Build with the progress on stderr, and name each output on stdout.
runBuild :: BuildOptions -> IO ()
runBuild options = do
  outputs <- withProgress stderr (`buildWith` options)
  let label = if buildNoLink options then "bundle: " else "executable: "
  mapM_ (putStrLn . (label <>)) outputs

-- | Build without progress. The verbose messages go to stdout. A library
-- caller, such as a test, uses this entry point.
build :: BuildOptions -> IO [FilePath]
build = buildWith (quietProgress stdout)

-- | Build what the input names and return the paths of the executables, or
-- of their link bundles with @--no-link@. An existing file is a main
-- module; everything else is a package.
buildWith :: ProgressReporter -> BuildOptions -> IO [FilePath]
buildWith reporter options = do
  isFile <- doesFileExist (buildInput options)
  case (isFile, buildExecutables options) of
    (True, _ : _) -> ioError (userError "--executable selects the executables of a package, and a main module is one executable")
    (True, []) -> pure <$> runBuildModule reporter options
    (False, _) -> buildPackage reporter options

-- | Build every executable of the Cabal package the input names, or the
-- executables that @--executable@ selects.
buildPackage :: ProgressReporter -> BuildOptions -> IO [FilePath]
buildPackage reporter options = do
  storeRoot <- maybe defaultStoreRoot pure (buildStoreRoot options)
  currentDirectory <- getCurrentDirectory
  hackageSource <- defaultHackageSource
  (rootPackage, origin, lockDirectory) <- installTargetRoot (buildInput options)
  let target = buildTarget options
      targetDirectory = nativeTargetStoreDirectory target
      (os, arch) = cabalPlatformForTarget target
      report = progressReport reporter
      verbose message = when (buildVerbose options) (report (ProgressLog message))
      -- The selected executables, or every executable when none is named.
      selection = if null (buildExecutables options) then Nothing else Just (nub (buildExecutables options))
  -- The package itself and its siblings resolve locally before the
  -- workspace and Hackage, so an executable that depends on the library
  -- of its own package finds it in the source tree.
  request <- planRequestFor hackageSource (buildPlanOptions options) (os, arch) (maybe [] pure (buildWorkspace options)) lockDirectory verbose
  planned <- planPackages request {requestRoots = [rootPackage], requestExecutables = selection}
  rootPlan <- case plannedRoots planned of
    [plan] -> pure plan
    _ -> ioError (userError "The plan has no root")
  let root = planSourcePath rootPlan
      gpd = planDescription rootPlan
      -- A Hackage release builds its executables under the working
      -- directory: its source tree is the download cache, which is shared
      -- by every build that unpacks the release.
      localBuildRoot =
        fromMaybe
          (if origin == PlanLocal then defaultBuildRoot root else currentDirectory </> ".aihc-target")
          (buildBuildRoot options)
      buildRoot = localBuildRoot </> targetDirectory
      outputDirectory = fromMaybe (buildRoot </> "bin") (buildOutput options)
  buildable <- HackageCabal.collectExecutablesIn (planBuildContext (os, arch) rootPlan) gpd root
  when (null buildable) $
    ioError (userError ("The package " <> unPackageName (planName rootPlan) <> " has no buildable executable"))
  let buildableNames = map executableInfoName buildable
      executables = maybe buildable (\names -> filter ((`elem` names) . executableInfoName) buildable) selection
  forM_ (fromMaybe [] selection) $ \name ->
    unless (name `elem` buildableNames) $
      ioError
        ( userError
            ("The package " <> unPackageName (planName rootPlan) <> " has no buildable executable " <> name <> "; its buildable executables are " <> unwords buildableNames)
        )
  buildIdentity <- buildEnvironmentIdentity target
  headerDirectory <- ensureCompilerHeaders target buildRoot
  let plan = optimizationPlan (buildLto options) (buildOptimization options)
      compileConfig =
        ModuleCompileConfig
          { compileBuildIdentity = buildIdentity,
            compileKeepCore = buildKeepCore options,
            compileKeepGrin = buildKeepGrin options,
            compileKeepLir = buildKeepLir options,
            compileKeepNative = buildKeepNative options,
            compileLint = buildLint options,
            compileCheckPrimBounds = buildCheckPrimBounds options,
            compileLto = planWholeProgram plan,
            compilePasses = planPasses plan,
            compileGrinPointsTo = planGrinPointsTo plan,
            compileNoCode = False,
            compileOptimization = buildOptimization options,
            compileTarget = target,
            compileHeaderDirectory = headerDirectory,
            compileVerbose = verbose,
            compilePrintTimings = const (pure ()),
            compileUseColor = progressColor reporter,
            compileProgress = reporter
          }
      -- The installed packages of an executable are built the way
      -- @install@ builds them. The flags that keep the output of a phase
      -- name the modules of the executable alone, so a dependency already
      -- in the store is never rejected for lacking those outputs.
      dependencyConfig =
        compileConfig
          { compileKeepCore = False,
            compileKeepGrin = False,
            compileKeepLir = False,
            compileKeepNative = False
          }
      locations =
        InstallLocations
          { locationStoreRoot = storeRoot </> targetDirectory,
            locationBuildRoot = buildRoot,
            locationImmutable = False,
            locationReinstall = False
          }
  canonicalRoot <- canonicalizePath root
  -- The plan finds the package being built by its name, which marks
  -- it local. What the user asked for decides instead: a directory is
  -- local, a Hackage release is not.
  executablePlans <- forM executables $ \executable -> do
    let dependencyPackages =
          nub (executableInfoDependencies executable <> map mkPackageName ["aihc-base", "aihc-prim"])
    plans <- mapM (plannedPackage planned) dependencyPackages
    mapM (markRootPlan canonicalRoot origin) plans
  report
    ( ProgressPlan
        ( planProgressItems (concat executablePlans)
            <> [ItemExecutable (T.pack (executableInfoName executable)) | executable <- executables]
        )
    )
  forM (zip executables executablePlans) $ \(executable, rootedPlans) -> do
    let name = executableInfoName executable
        item = ItemExecutable (T.pack name)
    verbose ("Build executable: " <> name)
    installed <- installPlanPackages dependencyConfig locations rootedPlans
    let selected = map installedPackage installed
    validateSelectedPackageNames selected
    mapM_ requirePackageArchive selected
    let outputRoot = buildRoot </> "exe" </> name
        dependencyNames = map (packageManifestName . installedManifest) selected
    -- The main module is the module that the @main-is@ file declares, as
    -- MicroHs builds it. GHC instead needs @-main-is@ for a main module
    -- that is not @Main@.
    mainModule <- maybe (pure "Main") (sourceFileModuleName compileConfig root installed) (executableInfoMainFile executable)
    entryFile <- writeEntryModule outputRoot mainModule dependencyNames
    headerDirs <- dependencyIncludeDirs installed
    let sourceFiles = executableInfoFiles executable <> [entryFile]
        ownCInfo = executableInfoCCompileInfo executable
        cCompileInfo = ownCInfo {HackageCabal.cCompileIncludeDirs = nub (HackageCabal.cCompileIncludeDirs ownCInfo <> headerDirs)}
        compileRequest =
          ModuleCompileRequest
            { compileOutputRoot = outputRoot,
              compilePackageRoot = root,
              -- The entry archive of the target refers to the entry of the
              -- package whose identity is @exe@, so every executable
              -- carries that identity; its name is its own.
              compilePackage = Package (T.pack name) (PackageId "exe"),
              compileSourceFiles = sourceFiles,
              compileDependencies = installed,
              compileCapiStubOptions = capiStubOptions sourceFiles cCompileInfo,
              compileItem = item
            }
    compiled <- compileModules compileConfig compileRequest
    cObjects <- compilePackageCFiles target (buildOptimization options) headerDirectory verbose root outputRoot cCompileInfo
    let output = outputDirectory </> executableFileName target name
    report (ProgressLink item)
    finishExecutable
      compileConfig
      ExecutableInputs
        { executableStoreRoot = storeRoot,
          executableNoLink = buildNoLink options,
          executableOutput = output,
          executableBuildRoot = outputRoot,
          executableModules = compiled,
          executableExtraObjects = cObjects,
          executableCxxStdLib = not (null (HackageCabal.cCompileCxxSources cCompileInfo)),
          executableLibraryArguments = packageLinkArguments target cCompileInfo,
          executablePackages = selected
        }
    report (ProgressDone item)
    pure output

-- | Give the plan of the package being built the origin the user asked for.
-- The dependencies of that plan keep theirs: a plan never names the package
-- at its root again, because the plan of a package with a cycle is an error.
markRootPlan :: FilePath -> PlanOrigin -> PackagePlan -> IO PackagePlan
markRootPlan canonicalRoot origin plan = do
  source <- canonicalizePath (planSourcePath plan)
  pure (if source == canonicalRoot then plan {planOrigin = origin} else plan)

-- | The file an executable is written to. A WebAssembly component carries
-- the suffix its runtimes expect.
executableFileName :: NativeTarget -> String -> FilePath
executableFileName target name =
  case target of
    Wasm32Wasip3 -> name <.> "wasm"
    _ -> name

-- | Write the generated entry module of an executable and describe it the
-- way the Cabal file describes the executable's own sources. The entry
-- calls the function @main@ of the main module.
writeEntryModule :: FilePath -> Text -> [Text] -> IO HackageCabal.FileInfo
writeEntryModule outputRoot mainModule dependencyNames = do
  let path = outputRoot </> "generated" </> "Aihc" </> "Entry.hs"
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (entryModuleText mainModule)
  pure
    HackageCabal.FileInfo
      { HackageCabal.fileInfoPath = path,
        HackageCabal.fileInfoExtensions = [],
        HackageCabal.fileInfoCppOptions = [],
        HackageCabal.fileInfoIncludeDirs = [],
        HackageCabal.fileInfoLanguage = Nothing,
        HackageCabal.fileInfoDependencies = dependencyNames,
        HackageCabal.fileInfoPreprocessor = Nothing
      }
