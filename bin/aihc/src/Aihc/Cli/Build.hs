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
module Aihc.Cli.Build
  ( build,
    runBuild,
  )
where

import Aihc.Cli.BuildModule (mainModuleExecutable, plannedPackage, writeEntryModule)
import Aihc.Cli.Hackage (defaultHackageSource)
import Aihc.Cli.Install
  ( ExecutableComponent (..),
    InstallLocations (..),
    InstalledPackage (..),
    ModuleCompileConfig (..),
    cabalPlatformForTarget,
    defaultBuildRoot,
    installExecutables,
    installTargetRoot,
    newModuleCompileConfig,
    planRequestFor,
    sourceFileModuleName,
  )
import Aihc.Cli.Link (linkCompiledExecutable)
import Aihc.Cli.Options (BuildOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
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
import Data.Text qualified as T
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath (dropExtension, (<.>), (</>))

runBuild :: BuildOptions -> IO ()
runBuild options = do
  outputs <- build options
  let label = if buildNoLink options then "bundle: " else "executable: "
  mapM_ (putStrLn . (label <>)) outputs

-- | One executable of a build: what the install graph compiles, and where
-- the executable or its link bundle goes.
data ExecutableTarget = ExecutableTarget
  { executableComponent :: !ExecutableComponent,
    executableOutput :: !FilePath
  }

-- | Build what the input names and return the paths of the executables, or
-- of their link bundles with @--no-link@. An existing file is a main
-- module; everything else is a package.
build :: BuildOptions -> IO [FilePath]
build options = do
  isFile <- doesFileExist (buildInput options)
  when (isFile && not (null (buildExecutables options))) $
    ioError (userError "--executable selects the executables of a package, and a main module is one executable")
  storeRoot <- maybe defaultStoreRoot pure (buildStoreRoot options)
  let target = buildTarget options
      targetDirectory = nativeTargetStoreDirectory target
      verbose message = when (buildVerbose options) (putStrLn message)
  levelConfig <- newModuleCompileConfig target (storeRoot </> targetDirectory) (buildLto options) (buildOptimization options)
  let config =
        levelConfig
          { compileKeepCore = buildKeepCore options,
            compileKeepGrin = buildKeepGrin options,
            compileKeepLir = buildKeepLir options,
            compileKeepNative = buildKeepNative options,
            compileLint = buildLint options,
            compileCheckPrimBounds = buildCheckPrimBounds options,
            compileVerbose = verbose
          }
  (locations, targets) <-
    (if isFile then mainModuleTarget else packageTargets) options config (storeRoot </> targetDirectory)
  compiled <- installExecutables config locations (map executableComponent targets)
  forM (zip targets compiled) $ \(executable, compiledExecutable) -> do
    let output = executableOutput executable
    linkCompiledExecutable config (buildNoLink options) (componentOutputRoot (executableComponent executable)) output compiledExecutable
    pure output

-- | The executable of a main module. Its modules build under the build
-- root, and the packages that the constraints name go into the store. A
-- main module has no Cabal file, so its lock lives in the working
-- directory.
mainModuleTarget :: BuildOptions -> ModuleCompileConfig -> FilePath -> IO (InstallLocations, [ExecutableTarget])
mainModuleTarget options config storeTargetRoot = do
  currentDirectory <- getCurrentDirectory
  hackageSource <- defaultHackageSource
  let target = compileTarget config
      localBuildRoot = fromMaybe (currentDirectory </> ".aihc-target") (buildBuildRoot options)
      buildRoot = localBuildRoot </> nativeTargetStoreDirectory target
  request <- planRequestFor hackageSource (buildPlanOptions options) (cabalPlatformForTarget target) (maybe [] pure (buildWorkspace options)) (Just currentDirectory) (compileVerbose config)
  component <- mainModuleExecutable options currentDirectory buildRoot request
  pure
    ( buildLocations storeTargetRoot buildRoot True,
      [ExecutableTarget component (fromMaybe (dropExtension (buildInput options)) (buildOutput options))]
    )

-- | Every executable of the Cabal package the input names, or the
-- executables that @--executable@ selects.
packageTargets :: BuildOptions -> ModuleCompileConfig -> FilePath -> IO (InstallLocations, [ExecutableTarget])
packageTargets options config storeTargetRoot = do
  currentDirectory <- getCurrentDirectory
  hackageSource <- defaultHackageSource
  (rootPackage, origin, lockDirectory) <- installTargetRoot (buildInput options)
  let target = compileTarget config
      platform = cabalPlatformForTarget target
      -- The selected executables, or every executable when none is named.
      selection = if null (buildExecutables options) then Nothing else Just (nub (buildExecutables options))
  -- The package itself and its siblings resolve locally before the
  -- workspace and Hackage, so an executable that depends on the library
  -- of its own package finds it in the source tree.
  request <- planRequestFor hackageSource (buildPlanOptions options) platform (maybe [] pure (buildWorkspace options)) lockDirectory (compileVerbose config)
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
      buildRoot = localBuildRoot </> nativeTargetStoreDirectory target
      outputDirectory = fromMaybe (buildRoot </> "bin") (buildOutput options)
  buildable <- HackageCabal.collectExecutablesIn (planBuildContext platform rootPlan) gpd root
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
  canonicalRoot <- canonicalizePath root
  targets <- forM executables $ \executable -> do
    let name = executableInfoName executable
        outputRoot = buildRoot </> "exe" </> name
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
              componentInputs = \packages -> do
                -- The main module is the module that the @main-is@ file
                -- declares, as MicroHs builds it. GHC instead needs
                -- @-main-is@ for a main module that is not @Main@.
                mainModule <- maybe (pure "Main") (sourceFileModuleName config root packages) (executableInfoMainFile executable)
                entry <- writeEntryModule outputRoot mainModule (map (packageManifestName . installedManifest) packages)
                pure (executableInfoFiles executable <> [entry], executableInfoCCompileInfo executable)
            }
    pure (ExecutableTarget component (outputDirectory </> executableFileName target name))
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
