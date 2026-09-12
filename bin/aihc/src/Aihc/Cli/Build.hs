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
    dependencyConstraint,
    finishExecutable,
    generatedEntryText,
    implicitConstraint,
    installedPackage,
    planConstraint,
    requirePackageArchive,
    runBuildModule,
    validateSelectedPackageNames,
  )
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
    installPlanPackages,
    networkDependencyResolver,
    resolveInstallTarget,
  )
import Aihc.Cli.Options (BuildOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Hackage.Cabal (ExecutableInfo (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Native (NativeTarget (..), nativeTargetStoreDirectory, wholeProgramLevel)
import Aihc.PackagePlan
  ( PackagePlan (..),
    PlanOrigin (..),
    localDependencyResolverWithFallback,
    packageSpecFromSource,
    parseSourcePackageDescription,
    workspaceDependencyResolver,
  )
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Monad (forM, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
  (root, origin) <- resolveInstallTarget (buildInput options)
  spec <- packageSpecFromSource root
  gpd <- parseSourcePackageDescription root
  let target = buildTarget options
      targetDirectory = nativeTargetStoreDirectory target
      (os, arch) = cabalPlatformForTarget target
      verbose message = when (buildVerbose options) (putStrLn message)
      -- A Hackage release builds its executables under the working
      -- directory: its source tree is the download cache, which is shared
      -- by every build that unpacks the release.
      localBuildRoot =
        fromMaybe
          (if origin == PlanLocal then defaultBuildRoot root else currentDirectory </> ".aihc-target")
          (buildBuildRoot options)
      buildRoot = localBuildRoot </> targetDirectory
      outputDirectory = fromMaybe (buildRoot </> "bin") (buildOutput options)
  executables <- HackageCabal.collectExecutablesFor os arch gpd root
  when (null executables) $
    ioError (userError ("The package " <> pkgName spec <> " has no buildable executable"))
  buildIdentity <- buildEnvironmentIdentity target
  let compileConfig =
        ModuleCompileConfig
          { compileBuildIdentity = buildIdentity,
            compileKeepCore = False,
            compileKeepGrin = False,
            compileKeepNative = False,
            compileLint = buildLint options,
            compileCheckPrimBounds = buildCheckPrimBounds options,
            compileLto = buildLto options || wholeProgramLevel (buildOptimization options),
            compileNoCode = False,
            compileOptimization = buildOptimization options,
            compileTarget = target,
            compileVerbose = verbose,
            compilePrintTimings = const (pure ()),
            compileUseColor = False
          }
      -- The package itself and its siblings resolve locally before the
      -- workspace and Hackage, so an executable that depends on the library
      -- of its own package finds it in the source tree.
      fallback = maybe networkDependencyResolver (workspaceDependencyResolver networkDependencyResolver) (buildWorkspace options)
      resolver = localDependencyResolverWithFallback fallback root spec
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
    let constraints =
          map dependencyConstraint (executableInfoDependencies executable)
            <> map implicitConstraint ["aihc-base", "aihc-prim"]
    plans <- mapM (planConstraint resolver) constraints
    -- The resolver finds the package being built by its name, which marks
    -- it local. What the user asked for decides instead: a directory is
    -- local, a Hackage release is not.
    rootedPlans <- mapM (markRootPlan canonicalRoot origin) plans
    installed <- installPlanPackages compileConfig locations rootedPlans
    let selected = map installedPackage installed
    validateSelectedPackageNames selected
    mapM_ requirePackageArchive selected
    let outputRoot = buildRoot </> "exe" </> name
        dependencyNames = map (packageManifestName . installedManifest) selected
    entryFile <- writeEntryModule outputRoot dependencyNames
    let sourceFiles = executableInfoFiles executable <> [entryFile]
        cCompileInfo = executableInfoCCompileInfo executable
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
    cObjects <- compilePackageCFiles target (buildOptimization options) verbose root outputRoot cCompileInfo
    let output = outputDirectory </> executableFileName target name
    finishExecutable
      compileConfig
      ExecutableInputs
        { executableStoreRoot = storeRoot,
          executableGarbageCollector = buildGarbageCollector options,
          executableNoLink = buildNoLink options,
          executableOutput = output,
          executableBuildRoot = outputRoot,
          executableModules = compiled,
          executableExtraObjects = cObjects,
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
