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
    runBuild,
  )
where

import Aihc.Cli.BuildModule
  ( ExecutableInputs (..),
    InstalledPackage (..),
    finishExecutable,
    generatedEntryText,
    installedPackage,
    plannedPackage,
    requirePackageArchive,
    runBuildModule,
    validateSelectedPackageNames,
  )
import Aihc.Cli.CompilerHeaders (ensureCompilerHeaders)
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
    planRequestFor,
  )
import Aihc.Cli.OptimizationPlan (OptimizationPlan (..), optimizationPlan)
import Aihc.Cli.Options (BuildOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Hackage.Cabal (ExecutableInfo (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.IndexCache (defaultIndexOptions, newHackageIndex)
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
import Control.Monad (forM, when)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Package (mkPackageName, unPackageName)
import Distribution.Types.Dependency (depPkgName)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (<.>), (</>))

runBuild :: BuildOptions -> IO ()
runBuild options = do
  outputs <- build options
  let label = if buildNoLink options then "bundle: " else "executable: "
  mapM_ (putStrLn . (label <>)) outputs

-- | Build what the input names and return the paths of the executables, or
-- of their link bundles with @--no-link@. An existing file is a main
-- module; everything else is a package.
build :: BuildOptions -> IO [FilePath]
build options = do
  isFile <- doesFileExist (buildInput options)
  if isFile
    then pure <$> runBuildModule options
    else buildPackage options

-- | Build every executable of the Cabal package the input names.
buildPackage :: BuildOptions -> IO [FilePath]
buildPackage options = do
  storeRoot <- maybe defaultStoreRoot pure (buildStoreRoot options)
  currentDirectory <- getCurrentDirectory
  hackageIndex <- newHackageIndex defaultIndexOptions
  (rootPackage, origin, lockDirectory) <- installTargetRoot (buildInput options)
  let target = buildTarget options
      targetDirectory = nativeTargetStoreDirectory target
      (os, arch) = cabalPlatformForTarget target
      verbose message = when (buildVerbose options) (putStrLn message)
  -- The package itself and its siblings resolve locally before the
  -- workspace and Hackage, so an executable that depends on the library
  -- of its own package finds it in the source tree.
  request <- planRequestFor hackageIndex (buildPlanOptions options) (os, arch) (maybe [] pure (buildWorkspace options)) lockDirectory verbose
  planned <- planPackages request {requestRoots = [rootPackage]}
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
  executables <- HackageCabal.collectExecutablesIn (planBuildContext (os, arch) rootPlan) gpd root
  when (null executables) $
    ioError (userError ("The package " <> unPackageName (planName rootPlan) <> " has no buildable executable"))
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
            compileNoCode = False,
            compileOptimization = buildOptimization options,
            compileTarget = target,
            compileHeaderDirectory = headerDirectory,
            compileVerbose = verbose,
            compilePrintTimings = const (pure ()),
            compileUseColor = False
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
  forM executables $ \executable -> do
    let name = executableInfoName executable
    verbose ("Build executable: " <> name)
    let dependencyPackages =
          nub (map depPkgName (executableInfoDependencies executable) <> map mkPackageName ["aihc-base", "aihc-prim"])
    plans <- mapM (plannedPackage planned) dependencyPackages
    -- The plan finds the package being built by its name, which marks
    -- it local. What the user asked for decides instead: a directory is
    -- local, a Hackage release is not.
    rootedPlans <- mapM (markRootPlan canonicalRoot origin) plans
    installed <- installPlanPackages dependencyConfig locations rootedPlans
    let selected = map installedPackage installed
    validateSelectedPackageNames selected
    mapM_ requirePackageArchive selected
    let outputRoot = buildRoot </> "exe" </> name
        dependencyNames = map (packageManifestName . installedManifest) selected
    entryFile <- writeEntryModule outputRoot dependencyNames
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
              compileCapiStubOptions = capiStubOptions sourceFiles cCompileInfo
            }
    compiled <- compileModules compileConfig compileRequest
    cObjects <- compilePackageCFiles target (buildOptimization options) headerDirectory verbose root outputRoot cCompileInfo
    let output = outputDirectory </> executableFileName target name
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
          executablePackages = selected
        }
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
-- way the Cabal file describes the executable's own sources.
writeEntryModule :: FilePath -> [Text] -> IO HackageCabal.FileInfo
writeEntryModule outputRoot dependencyNames = do
  let path = outputRoot </> "generated" </> "Aihc" </> "Entry.hs"
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path generatedEntryText
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
