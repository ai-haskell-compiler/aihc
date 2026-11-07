{-# LANGUAGE OverloadedStrings #-}

-- | The @build@ command: an install with extra steps that link
-- executables.
--
-- A Haskell source file is the main module of one executable, which
-- "Aihc.Cli.BuildModule" finds from the source directories and package
-- constraints of the command line. Anything else is a Cabal package: a local
-- directory, or a Hackage release named as @install@ names it. Its
-- executables are found in the Cabal file, and each is built from the
-- sources and the @build-depends@ its own stanza declares. The library of
-- the package, when an executable depends on it, is installed like any
-- other dependency: in place under the build directory for a local package,
-- and into the store for a Hackage release.
--
-- Either way, the install graph compiles the executables together with the
-- packages below them, and then each executable is linked.
--
-- The @test@ command builds the test suites of a package on the same path:
-- each @exitcode-stdio-1.0@ test suite is an executable with its own
-- @build-depends@, and 'buildTestSuites' links it under the build root.
module Aihc.Cli.Build
  ( build,
    buildWith,
    buildWithConfig,
    runBuild,
    BuiltProgram (..),
    buildTestSuites,
  )
where

import Aihc.Cli.BuildModule (mainModuleExecutable, plannedPackage, writeEntryModule)
import Aihc.Cli.Hackage (defaultHackageSource)
import Aihc.Cli.Install
  ( ExecutableComponent (..),
    InstallLocations (..),
    InstalledPackage (..),
    ModuleCompileConfig (..),
    TargetRoot (..),
    cabalPlatformForTarget,
    defaultBuildRoot,
    installExecutables,
    installTargetRoot,
    newModuleCompileConfig,
    planProgressItems,
    planRequestFor,
    sourceFileModuleName,
  )
import Aihc.Cli.Link (linkCompiledExecutable)
import Aihc.Cli.Options (BuildOptions (..), PlanOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.Progress (ProgressEvent (..), ProgressItem (..), ProgressReporter (..), quietProgress, withProgress)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Hackage.Cabal (ExecutableInfo (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Package (mkPackageName, unPackageName)
import Aihc.Native (NativeTarget (..), PositionIndependence (..), nativeTargetStoreDirectory)
import Aihc.PackagePlan
  ( PackagePlan (..),
    PlanOrigin (..),
    PlanRequest (..),
    PlannedPackages (..),
    planBuildContext,
    planPackages,
  )
import Aihc.PackagePlan.Project (ProjectComponent (..))
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Monad (forM, forM_, unless, when)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath (dropExtension, takeFileName, (<.>), (</>))
import System.IO (hPutStrLn, stderr, stdout)

-- | Build with the progress on stderr, and name each output on stdout.
runBuild :: BuildOptions -> IO ()
runBuild options = do
  when (buildNoPie options) $
    hPutStrLn stderr "warning: --no-pie links each Linux executable at a fixed address. ASLR cannot then move its code and static data. Do not use --no-pie for a program that processes untrusted input."
  outputs <- withProgress stderr (`buildWith` options)
  let label = if buildNoLink options then "bundle: " else "executable: "
  mapM_ (putStrLn . (label <>)) outputs

-- | Whether the executables are position independent. The default is a
-- position-independent executable.
positionIndependence :: BuildOptions -> PositionIndependence
positionIndependence options
  | buildNoPie options = PositionDependent
  | otherwise = PositionIndependent

-- | Build without progress. The verbose messages go to stdout. A library
-- caller, such as a test, uses this entry point.
build :: BuildOptions -> IO [FilePath]
build = buildWith (quietProgress stdout)

-- | One executable of a build: what the install graph compiles, and where
-- the executable or its link bundle goes.
data ExecutableTarget = ExecutableTarget
  { executableName :: !String,
    executableComponent :: !ExecutableComponent,
    executableOutput :: !FilePath
  }

-- | One program that a build linked.
data BuiltProgram = BuiltProgram
  { builtName :: !String,
    -- | The directory of the package the program belongs to, or the
    -- working directory for a main module. A test suite runs there.
    builtPackageRoot :: !FilePath,
    -- | The executable, or its link bundle with @--no-link@.
    builtOutput :: !FilePath
  }
  deriving (Eq, Show)

-- | What a package build makes.
data ProgramKind
  = ProgramExecutable
  | ProgramTestSuite
  deriving (Eq)

-- | Build what the input names and return the paths of the executables, or
-- of their link bundles with @--no-link@. An existing file is a main
-- module; everything else is a package.
buildWith :: ProgressReporter -> BuildOptions -> IO [FilePath]
buildWith reporter options = buildWithConfig reporter options id

-- | 'buildWith' with a change to the compile config that the options give.
-- A tool that reads the output of each phase uses it to set
-- 'compileObserver'.
buildWithConfig :: ProgressReporter -> BuildOptions -> (ModuleCompileConfig -> ModuleCompileConfig) -> IO [FilePath]
buildWithConfig reporter options adjust = do
  isFile <- doesFileExist (buildInput options)
  when (isFile && not (null (buildExecutables options))) $
    ioError (userError "--executable selects the executables of a package, and a main module is one executable")
  map builtOutput <$> linkPrograms reporter options adjust (if isFile then mainModuleTarget else packageTargets ProgramExecutable)

-- | Build the test suites of the package the input names, and return
-- each with the directory it runs in. The options select the test suites
-- as they select executables. A package without a buildable test suite
-- builds nothing.
buildTestSuites :: ProgressReporter -> BuildOptions -> IO [BuiltProgram]
buildTestSuites reporter options = linkPrograms reporter options id (packageTargets ProgramTestSuite)

-- | Compile the programs that the targets name in one install graph, and
-- link each of them.
linkPrograms ::
  ProgressReporter ->
  BuildOptions ->
  (ModuleCompileConfig -> ModuleCompileConfig) ->
  (BuildOptions -> ModuleCompileConfig -> FilePath -> IO (InstallLocations, [ExecutableTarget])) ->
  IO [BuiltProgram]
linkPrograms reporter options adjust targetsOf = do
  -- The time profiler samples from a POSIX timer signal.
  when (buildProfileTime options && buildTarget options == Wasm32Wasip3) $
    ioError (userError "--profile-time needs a POSIX target")
  storeRoot <- maybe defaultStoreRoot pure (buildStoreRoot options)
  let target = buildTarget options
      targetDirectory = nativeTargetStoreDirectory target
      report = progressReport reporter
      verbose message = when (buildVerbose options) (report (ProgressLog message))
      printTimings message = when (buildPrintTimings options) (report (ProgressLog message))
  levelConfig <- newModuleCompileConfig target (storeRoot </> targetDirectory) (buildLto options || buildProfileAllocations options || buildProfileTime options) (buildOptimization options)
  let config =
        adjust
          levelConfig
            { compileKeepCore = buildKeepCore options,
              compileKeepGrin = buildKeepGrin options,
              compileKeepLir = buildKeepLir options,
              compileKeepNative = buildKeepNative options,
              compileLint = buildLint options,
              compileCheckPrimBounds = buildCheckPrimBounds options,
              compileProfileAllocations = buildProfileAllocations options,
              compileProfileTime = buildProfileTime options,
              compileVerbose = verbose,
              compilePrintTimings = printTimings,
              compileUseColor = progressColor reporter,
              compileProgress = reporter
            }
  (locations, targets) <- targetsOf options config (storeRoot </> targetDirectory)
  if null targets
    then pure []
    else do
      let components = map executableComponent targets
      report (ProgressPlan (planProgressItems (concatMap componentDependencies components) <> map componentItem components))
      compiled <- installExecutables config locations components
      forM (zip targets compiled) $ \(executable, compiledExecutable) -> do
        let output = executableOutput executable
            component = executableComponent executable
        report (ProgressLink (componentItem component))
        linkCompiledExecutable config (positionIndependence options) (buildNoLink options) (componentOutputRoot component) output compiledExecutable
        report (ProgressDone (componentItem component))
        pure (BuiltProgram (executableName executable) (componentSourceRoot component) output)

-- | The executable of a main module. Its modules build under the build
-- root, and the packages that the constraints name go into the store. A
-- main module has no Cabal file, so its lock lives in the working
-- directory.
mainModuleTarget :: BuildOptions -> ModuleCompileConfig -> FilePath -> IO (InstallLocations, [ExecutableTarget])
mainModuleTarget options config storeTargetRoot = do
  currentDirectory <- getCurrentDirectory
  hackageSource <- defaultHackageSource (progressReport (compileProgress config) . ProgressLog)
  let target = compileTarget config
      localBuildRoot = fromMaybe (currentDirectory </> ".aihc-target") (buildBuildRoot options)
      buildRoot = localBuildRoot </> nativeTargetStoreDirectory target
      output = fromMaybe (dropExtension (buildInput options)) (buildOutput options)
  request <- planRequestFor hackageSource (buildPlanOptions options) (cabalPlatformForTarget target) Nothing (maybe [] pure (buildWorkspace options)) (Just currentDirectory) (compileVerbose config)
  component <- mainModuleExecutable options currentDirectory buildRoot request
  pure
    ( buildLocations storeTargetRoot buildRoot True,
      [ExecutableTarget (takeFileName output) component output]
    )

-- | Every executable or every test suite of the Cabal package the input
-- names, or the ones that the input and @--executable@ select.
packageTargets :: ProgramKind -> BuildOptions -> ModuleCompileConfig -> FilePath -> IO (InstallLocations, [ExecutableTarget])
packageTargets kind options config storeTargetRoot = do
  currentDirectory <- getCurrentDirectory
  hackageSource <- defaultHackageSource (progressReport (compileProgress config) . ProgressLog)
  TargetRoot rootPackage origin lockDirectory targetComponents project <- installTargetRoot (planProject (buildPlanOptions options)) (buildInput options)
  let target = compileTarget config
      platform = cabalPlatformForTarget target
      (noun, item, artifactDirectory, binDirectory) =
        case kind of
          ProgramExecutable -> ("executable", ItemExecutable, "exe", "bin")
          ProgramTestSuite -> ("test suite", ItemTestSuite, "test", "test-bin")
      -- The components of the input that this build does not make. The
      -- error names the command that makes them.
      (selectedNames, otherNames, otherNoun, otherCommand) =
        case kind of
          ProgramExecutable -> ([name | ProjectExecutable name <- targetComponents], [name | ProjectTestSuite name <- targetComponents], "a test suite", "test")
          ProgramTestSuite -> ([name | ProjectTestSuite name <- targetComponents], [name | ProjectExecutable name <- targetComponents], "an executable", "build")
      -- The selected programs, or every program when none is named.
      named = selectedNames <> buildExecutables options
      selection = if null named then Nothing else Just (nub named)
  forM_ otherNames $ \name ->
    ioError (userError (buildInput options <> " names " <> otherNoun <> ", and this command makes " <> noun <> "s. Use " <> otherCommand <> " for " <> name))
  -- The package itself and its siblings resolve locally before the
  -- workspace and Hackage, so an executable that depends on the library
  -- of its own package finds it in the source tree.
  request <- planRequestFor hackageSource (buildPlanOptions options) platform project (maybe [] pure (buildWorkspace options)) lockDirectory (compileVerbose config)
  planned <-
    planPackages
      request
        { requestRoots = [rootPackage],
          -- The test suites contribute their dependencies in place of the
          -- executables.
          requestExecutables = if kind == ProgramTestSuite then Just [] else selection,
          requestTestSuites = kind == ProgramTestSuite
        }
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
      buildRoot = localBuildRoot </> nativeTargetStoreDirectory target
      outputDirectory = fromMaybe (buildRoot </> binDirectory) (buildOutput options)
      collect = case kind of
        ProgramExecutable -> HackageCabal.collectExecutablesIn
        ProgramTestSuite -> HackageCabal.collectTestSuitesIn
  buildable <- collect (planBuildContext platform rootPlan) gpd root
  -- A package without a test suite has nothing to test, and the command
  -- that tests a project passes over it.
  when (null buildable && kind == ProgramExecutable) $
    ioError (userError ("The package " <> unPackageName (planName rootPlan) <> " has no buildable " <> noun))
  let buildableNames = map executableInfoName buildable
      executables = maybe buildable (\names -> filter ((`elem` names) . executableInfoName) buildable) selection
  forM_ (fromMaybe [] selection) $ \name ->
    unless (name `elem` buildableNames) $
      ioError
        ( userError
            ("The package " <> unPackageName (planName rootPlan) <> " has no buildable " <> noun <> " " <> name <> "; its buildable " <> noun <> "s are " <> unwords buildableNames)
        )
  canonicalRoot <- canonicalizePath root
  targets <- forM executables $ \executable -> do
    let name = executableInfoName executable
        outputRoot = buildRoot </> artifactDirectory </> name
    plans <- mapM (plannedPackage planned) (nub (executableInfoDependencies executable <> map mkPackageName ["aihc-base", "aihc-prim"]))
    -- The plan finds the package being built by its name, which marks
    -- it local. What the user asked for decides instead: a directory is
    -- local, a Hackage release is not.
    rootedPlans <- mapM (markRootPlan canonicalRoot origin) plans
    let component =
          ExecutableComponent
            { -- The entry archive of the target refers to the entry of the
              -- package whose identity is @exe@, so every executable
              -- carries that identity; its name is its own.
              componentPackage = Package (T.pack name) (PackageId "exe"),
              componentSourceRoot = root,
              componentOutputRoot = outputRoot,
              componentDependencies = rootedPlans,
              componentItem = item (T.pack name),
              componentInputs = \packages -> do
                -- The main module is the module that the @main-is@ file
                -- declares, as MicroHs builds it. GHC instead needs
                -- @-main-is@ for a main module that is not @Main@.
                mainModule <- maybe (pure "Main") (sourceFileModuleName config root packages) (executableInfoMainFile executable)
                entry <- writeEntryModule outputRoot mainModule (map (packageManifestName . installedManifest) packages)
                pure (executableInfoFiles executable <> [entry], executableInfoCCompileInfo executable)
            }
    pure (ExecutableTarget name component (outputDirectory </> executableFileName target name))
  pure (buildLocations storeTargetRoot buildRoot False, targets)

-- | Where the packages below the executables go. Nothing is reinstalled:
-- the user named the executables, and a package below them is a
-- dependency.
buildLocations :: FilePath -> FilePath -> Bool -> InstallLocations
buildLocations storeTargetRoot buildRoot immutable =
  InstallLocations
    { locationStoreRoot = storeTargetRoot,
      locationBuildRoot = buildRoot,
      locationImmutable = immutable,
      locationReinstall = False
    }

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
