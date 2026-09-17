{-# LANGUAGE PatternSynonyms #-}

module Aihc.Cli.Install
  ( InstallResult (..),
    InstallLocations (..),
    InstalledPackage (..),
    archiveHasMembers,
    FcModule (..),
    ModuleCompileConfig (..),
    ModuleCompileRequest (..),
    ModuleCompileResult (..),
    ModuleOutputPaths (..),
    backendOptionsKey,
    cabalPlatformForTarget,
    capiStubOptions,
    compileFcModules,
    optimizeFcProgram,
    compileModules,
    compilePackageCFiles,
    moduleOutputPaths,
    buildEnvironmentIdentity,
    defaultBuildRoot,
    install,
    installWith,
    installPlanPackages,
    networkDependencyResolver,
    parsePackageTarget,
    resolveInstallTarget,
    resolvePreferredVersion,
    runInstall,
  )
where

import Aihc.Capi (moduleCapiWrappers, parseDependencyFile, renderCapiStub)
import Aihc.Cli.ArtifactCache (compilerBuildIdentity, executableIdentity, hashChunks, sourceFilesHash)
import Aihc.Cli.Backend (compileGrinTo, nativeSourceExtension, nativeSourceIsLir)
import Aihc.Cli.BuildStamp
  ( BackendStamp (..),
    FileStamp (..),
    ModuleDigests (..),
    PackageDigests (..),
    ResolveStamp (..),
    UnitStamp (..),
    filesMatchStamps,
    packageDigestsPath,
    readStamp,
    stampFiles,
    writeStamp,
  )
import Aihc.Cli.CapiStub (CapiStubOptions (..), capiStubArguments)
import Aihc.Cli.CompilerHeaders (cabalPlatformForTarget, compilerHeaderIdentity, ensureCompilerHeaders, hostPlatformMacros)
import Aihc.Cli.InterfaceTyCons (classInfoTyCons, dataTypeInfoTyCons, interfaceTyCons, tyConInfoTyCons, typeSchemeTyCons, typeTyCons)
import Aihc.Cli.OptimizationPlan (OptimizationPlan (..), optimizationPlan)
import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifactParts)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Cli.TaskGraph
  ( Task (..),
    TaskId (..),
    TaskKind (..),
    TaskTiming,
    renderDuration,
    renderTaskTimeline,
    runTaskGraph,
  )
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeArtifactParts)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..))
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (cabalMacrosHeader)
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.IndexCache (HackageIndex, defaultIndexOptions, indexPreferredVersion, newHackageIndex)
import Aihc.Hackage.Preprocessor (Preprocessor (..), preprocessorEnvironmentVariable, preprocessorToolName)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Native (NativeTarget (..), OptimizationLevel, WasmSysroot (..), backendArchiver, backendCompiler, defaultOptimizationLevel, handwrittenCArguments, hostNativeTarget, nativeTargetStoreDirectory, optimizationArgument, renderOptimizationLevel, wasmSysroot)
import Aihc.PackagePlan
  ( DependencyResolver (..),
    DependencyVersions,
    PackagePlan (..),
    PlanOrigin (..),
    ResolvedSource (..),
    buildPackagePlanWithResolver,
    dependencyVersionsFromManifests,
    localDependencyResolverWithFallback,
    packageSpecFromSource,
  )
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap, renderHumanDiagnostic)
import Aihc.PackagePlan.Source (ParsedInterfaceFile (..), moduleDepsDigest, parseInterfaceBytes)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    Module,
    Name (..),
    SourceSpan,
    moduleName,
    sourceSpanSourceName,
    pattern SourceSpan,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Prim.Wiring (primDerivingReferences, primTcConfig, primTcWiring)
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    ModuleUnit,
    Package (..),
    PackageId (..),
    ResolutionNamespace (..),
    ResolveError (..),
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    collectModuleExportsWithDeps,
    emptyScope,
    extractInterfaceWithDeps,
    lookupImportedModule,
    modulesInPackage,
    resolveWithDeps,
    unionScope,
  )
import Aihc.Tc
  ( ClassInfo (..),
    DataFamilyInstanceInfo (..),
    DerivingReference (..),
    InstanceInfo (..),
    MergeCheck (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcInterface (..),
    TcKinds,
    TcSeverity (..),
    TcTermKey (..),
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    derivingReferenceList,
    emptyTcInterface,
    mergeTcInterfaces,
    mkTcKinds,
    renderFunDepNames,
    renderPred,
    renderTcType,
    tcInterfaceDataFamilyInstances,
    tcInterfaceInstances,
    tcInterfaceTerms,
    tcInterfaceTypeFamilyInstances,
    tcModuleBindings,
    tcModuleDiagnostics,
    tyConKey,
    typecheckModuleSccWithInterface,
    unionTcInterfaces,
  )
import Aihc.Tc.Share (shareTcInterfaces)
import Aihc.Tc.Types (TyCon, kindsCharTyCon, kindsNaturalTyCon, kindsSymbolTyCon, tyConModuleName, tyConName, tyConNamespace, tyConPackageId)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar, takeTMVar)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (IOException, bracket, evaluate, throwIO, try)
import Control.Monad (filterM, foldM, forM, forM_, unless, void, when, zipWithM)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf, nub, partition, sortOn)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (GenericPackageDescription, HookedBuildInfo, emptyHookedBuildInfo, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseHookedBuildInfo, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Version (nullVersion)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getFileSize, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, isRelative, makeRelative, takeDirectory, (<.>), (</>))
import System.IO (Handle, hClose, hIsTerminalDevice, hPutStrLn, openBinaryTempFile, stderr, stdout)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode, readProcess)

data InstallResult = InstallResult
  { -- | The package directory: in the store for an immutable package, in
    -- the build directory for a local one.
    installStorePath :: !FilePath,
    installWrittenModules :: ![Text],
    installReusedModules :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance NFData InstallResult

data SourceModule = SourceModule
  { sourceModulePath :: !FilePath,
    sourceModuleSize :: !Int,
    -- | A digest of everything the parse of this module depended on: its
    -- bytes, the cabal settings that shaped it, and, for a module that runs
    -- CPP, the headers it included and the dependency versions its
    -- @MIN_VERSION_*@ macros reported.
    sourceModuleHash :: !Text,
    -- | The parsed module. The resolve task of its unit takes it, so the
    -- parse tree dies once the unit is resolved (or, when the resolve
    -- artifacts were reused, once the unit is type-checked). Everything the
    -- later phases need from the header is precomputed in the fields below.
    sourceModuleParsed :: !(MVar Module),
    sourceModuleName :: !Text,
    -- | The directory of the module's artifacts, relative to the package.
    sourceModuleDirectory :: !FilePath,
    -- | The package qualifier and name of each import.
    sourceModuleImports :: ![(Maybe Text, Text)],
    sourceModuleExtensions :: ![Extension],
    sourceModuleParseDiagnostics :: [Value]
  }

data InstalledPackage = InstalledPackage
  { installedResult :: !InstallResult,
    installedName :: !Text,
    installedVersion :: !Text,
    -- | The name of the package directory: @NAME-VERSION-FINGERPRINT@ in the
    -- store and @NAME-VERSION@ in a build directory.
    installedIdentity :: !Text,
    -- | Whether the package lives in the store. A store package depends on
    -- store packages only.
    installedImmutable :: !Bool,
    installedManifest :: !PackageManifest,
    installedExports :: !ModuleExports,
    installedTypes :: !(Map.Map Text TcInterface),
    installedScopeHashes :: !(Map.Map Text Text),
    installedTypeHashes :: !(Map.Map Text Text),
    installedInstanceDigest :: !Text,
    installedInstanceFacts :: !TcInterface,
    installedInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider))
  }
  deriving (Generic)

instance NFData InstalledPackage

type InstanceProvider = (PackageId, Text)

data ModuleOutputPaths = ModuleOutputPaths
  { outputFcPath :: !FilePath,
    outputGrinPath :: !FilePath,
    outputCpsGrinPath :: !FilePath,
    outputGcGrinPath :: !FilePath,
    -- | The Lir text of the module. On a target whose object the backend
    -- writes itself this is also the native source of the module.
    outputLirPath :: !FilePath,
    outputNativePath :: !FilePath,
    outputObjectPath :: !FilePath,
    -- | The C wrappers of the module's @capi@ imports, the object they
    -- compile to, and the headers that compile read.
    outputCapiSourcePath :: !FilePath,
    outputCapiObjectPath :: !FilePath,
    outputCapiDependencyPath :: !FilePath
  }

data FcModule = FcModule
  { fcModuleName :: !Text,
    fcProgram :: !Fc.Program
  }

instance NFData FcModule where
  rnf (FcModule name program) = rnf name `seq` rnf program

-- | What the type-check task of a unit checks: the modules as the resolve
-- task resolved them, or the parsed modules when the resolve artifacts were
-- reused and the unit has to be resolved again before it can be checked.
data TypeInput
  = TypeInputResolved ResolveResult
  | TypeInputParsed [ModuleUnit]

-- | The System FC of a unit, as the backend task takes it from the
-- type-check task. The Haskell AST is gone by the time this exists.
data PendingBackend = PendingBackend
  { pendingFcModules :: ![FcModule],
    -- | The rendered C wrappers of each module's @capi@ imports, by module
    -- name; 'Nothing' for a module without any.
    pendingCapiStubs :: ![(Text, Maybe Text)]
  }

newtype UnitId = UnitId Int
  deriving (Eq, Ord, Show, Generic)

instance NFData UnitId

data SourceUnit = SourceUnit
  { sourceUnitId :: !UnitId,
    sourceUnitOrder :: !Int,
    sourceUnitSources :: ![SourceModule],
    sourceUnitDependencies :: ![UnitId]
  }

data ResolveUnitResult = ResolveUnitResult
  { resolveUnitExports :: !ModuleExports,
    resolveUnitScopeHashes :: !(Map.Map Text Text),
    resolveUnitErrors :: ![ResolveError],
    resolveUnitSuccess :: !Bool
  }

data TypeUnitResult = TypeUnitResult
  { typeUnitTypes :: !(Map.Map Text TcInterface),
    typeUnitHashes :: !(Map.Map Text Text),
    typeUnitOwnInstanceInterface :: !TcInterface,
    -- | The digest of the instance facts of the unit. The facts artifact
    -- carries the facts digests of the dependencies, so this digest changes
    -- when an instance anywhere below the unit changes.
    typeUnitFactsDigest :: !Text,
    typeUnitInstanceInterface :: !TcInterface,
    typeUnitDiagnostics :: ![TcDiagnostic],
    typeUnitWritten :: !(Set.Set Text),
    typeUnitReused :: !(Set.Set Text),
    -- | The stamp the backend writes once the objects of the unit exist.
    typeUnitPendingStamp :: !(Maybe PendingStamp),
    typeUnitSuccess :: !Bool
  }

-- | A unit stamp whose artifact files are not all written yet.
data PendingStamp = PendingStamp
  { pendingStampPath :: !FilePath,
    pendingStampInputs :: ![(Text, Text)],
    pendingStampTypes :: !(Map.Map Text Text),
    pendingStampFacts :: !Text,
    -- | The type artifacts and the facts artifact, relative to the package.
    pendingStampFrontendFiles :: ![FilePath]
  }

-- | The channels between the tasks of one unit. The results are read by
-- every dependent; the inputs are taken by the single task that consumes
-- them, so the parse tree, the resolved modules, and the System FC each die
-- as soon as the next phase has them.
data UnitRuntime = UnitRuntime
  { runtimeUnit :: !SourceUnit,
    runtimeResolveResult :: !(TMVar ResolveUnitResult),
    runtimeTypeInput :: !(TMVar TypeInput),
    runtimeTypeResult :: !(TMVar TypeUnitResult),
    runtimeBackendInput :: !(TMVar (Maybe PendingBackend))
  }

data ModuleCompileConfig = ModuleCompileConfig
  { compileBuildIdentity :: !String,
    compileKeepCore :: !Bool,
    compileKeepGrin :: !Bool,
    compileKeepLir :: !Bool,
    compileKeepNative :: !Bool,
    compileLint :: !Bool,
    -- | Check the index of every array primitive, as @--check-prim-bounds@
    -- asks. The checks are part of the generated code of a package.
    compileCheckPrimBounds :: !Bool,
    -- | Stop each module at System FC. @build@ merges the System FC of
    -- the whole program and compiles it once. @--lto@ sets this, and so
    -- does a level that optimizes.
    compileLto :: !Bool,
    -- | The System FC passes of the plan, in order. They run on each
    -- module, or on the merged program of a whole-program build.
    compilePasses :: ![Fc.Pass],
    compileNoCode :: !Bool,
    -- | The level Clang receives for C sources and LLVM output.
    compileOptimization :: !OptimizationLevel,
    compileTarget :: !NativeTarget,
    -- | Where the headers of the target are, for the C compiles of a
    -- package: its @c-sources@, the wrappers of its @capi@ imports and
    -- @hsc2hs@.
    compileHeaderDirectory :: !FilePath,
    compileVerbose :: String -> IO (),
    compilePrintTimings :: String -> IO (),
    compileUseColor :: !Bool
  }

data ModuleCompileRequest = ModuleCompileRequest
  { compileOutputRoot :: !FilePath,
    compilePackageRoot :: !FilePath,
    compilePackage :: !Package,
    compileSourceFiles :: ![HackageCabal.FileInfo],
    -- | The installed packages the modules are compiled against. Only the
    -- modules the sources import are read from them.
    compileDependencies :: ![InstalledPackage],
    -- | Where the capi wrappers of these modules look for their headers.
    compileCapiStubOptions :: !CapiStubOptions
  }

data ModuleCompileResult = ModuleCompileResult
  { -- | The objects of the modules, and of their capi wrappers. A @--lto@
    -- build has wrapper objects only.
    compileObjectPaths :: [FilePath],
    compileModuleNames :: [Text]
  }
  deriving (Eq, Show)

data CompiledPackageModules = CompiledPackageModules
  { compiledSources :: ![SourceModule],
    compiledExports :: !ModuleExports,
    compiledTypes :: !(Map.Map Text TcInterface),
    compiledScopeHashes :: !(Map.Map Text Text),
    compiledTypeHashes :: !(Map.Map Text Text),
    compiledInstanceDigest :: !Text,
    compiledInstanceFacts :: !TcInterface,
    compiledInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider)),
    compiledWritten :: !(Set.Set Text),
    compiledReused :: !(Set.Set Text)
  }

data BackendPhaseTimings = BackendPhaseTimings
  { backendDesugarNs :: !Word64,
    backendGrinNs :: !Word64,
    backendNativeNs :: !Word64,
    backendOtherNs :: !Word64
  }

instance Semigroup BackendPhaseTimings where
  left <> right =
    BackendPhaseTimings
      { backendDesugarNs = backendDesugarNs left + backendDesugarNs right,
        backendGrinNs = backendGrinNs left + backendGrinNs right,
        backendNativeNs = backendNativeNs left + backendNativeNs right,
        backendOtherNs = backendOtherNs left + backendOtherNs right
      }

instance Monoid BackendPhaseTimings where
  mempty = BackendPhaseTimings 0 0 0 0

data PackageTaskContext = PackageTaskContext
  { taskModuleCompileConfig :: !ModuleCompileConfig,
    taskStorePath :: !FilePath,
    taskResolvePackage :: !Package,
    taskPrimIdentity :: !PackageId,
    taskPackageRoot :: !FilePath,
    taskDependencyExports :: !ModuleExports,
    taskDependencyScopeHashes :: !(Map.Map Text Text),
    taskDependencyTypes :: !(Map.Map Text TcInterface),
    taskDependencyTypeHashes :: !(Map.Map Text Text),
    -- | The instance digest of each dependency package, with the modules
    -- it exposes. A unit that imports one of the modules takes the digest
    -- as an input, because the instances the package supplies come from
    -- the package as a whole.
    taskDependencyPackages :: ![(Text, Text, Set.Set Text)],
    taskDependencyInstanceFacts :: !TcInterface,
    taskDependencyInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider)),
    taskCapiStubOptions :: !CapiStubOptions,
    taskBackendPhaseTimings :: !(IORef BackendPhaseTimings)
  }

runInstall :: InstallOptions -> IO ()
runInstall options = do
  result <- install options
  putStrLn ("store: " <> installStorePath result)

-- | Install a package and write the verbose and timing messages to stdout.
install :: InstallOptions -> IO InstallResult
install = installWith stdout

-- | Install a package and write the verbose and timing messages to the given
-- handle. A test gives a file handle here and reads the file. The test must
-- not redirect the process stdout instead: the test runner writes its progress
-- to stdout from other threads, and a redirect would capture that progress.
installWith :: Handle -> InstallOptions -> IO InstallResult
installWith output options = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot options)
  useColor <- hIsTerminalDevice output
  let target = installTarget options
      targetDirectory = nativeTargetStoreDirectory target
  let verbose message = when (installVerbose options) (hPutStrLn output message)
      printTimings message = when (installPrintTimings options) (hPutStrLn output message)
  hackageIndex <- newHackageIndex defaultIndexOptions
  (root, origin) <- resolveInstallTarget hackageIndex (installPackageTarget options)
  spec <- packageSpecFromSource root
  let resolver = localDependencyResolverWithFallback (networkDependencyResolver hackageIndex) root spec
  plan <- buildPackagePlanWithResolver resolver spec
  buildRoot <- maybe (pure (defaultBuildRoot root)) pure (installBuildRoot options)
  buildIdentity <- buildEnvironmentIdentity target
  -- The headers go under the store and not under the build directory,
  -- because an immutable install writes no build directory at all.
  headerDirectory <- ensureCompilerHeaders target (storeRoot </> targetDirectory)
  let levelPlan = optimizationPlan (installLto options) (installOptimization options)
      config =
        ModuleCompileConfig
          { compileBuildIdentity = buildIdentity,
            compileKeepCore = installKeepCore options,
            compileKeepGrin = installKeepGrin options,
            compileKeepLir = False,
            compileKeepNative = installKeepNative options,
            compileLint = installLint options,
            compileCheckPrimBounds = installCheckPrimBounds options,
            compileLto = planWholeProgram levelPlan,
            compilePasses = planPasses levelPlan,
            compileNoCode = installNoCode options,
            compileOptimization = installOptimization options,
            compileTarget = target,
            compileHeaderDirectory = headerDirectory,
            compileVerbose = verbose,
            compilePrintTimings = printTimings,
            compileUseColor = useColor
          }
      locations =
        InstallLocations
          { locationStoreRoot = storeRoot </> targetDirectory,
            locationBuildRoot = buildRoot </> targetDirectory,
            locationImmutable = installImmutable options,
            locationReinstall = installReinstall options
          }
  -- The resolver finds the package being installed by its name, which marks
  -- it local. What the user asked for decides instead: a directory is local,
  -- a Hackage release is not.
  installedResult <$> installPackagePlan config locations plan {planOrigin = origin}

-- | Where a local package builds unless @--build-root@ says otherwise.
defaultBuildRoot :: FilePath -> FilePath
defaultBuildRoot root = root </> ".aihc-target"

-- | Turn the install argument into a local package directory and say where
-- it came from.
--
-- An existing directory is used as-is. Anything else is parsed as a Hackage
-- package name with an optional version (@NAME@ or @NAME-VERSION@) and
-- downloaded from Hackage; without a version the preferred version is used.
resolveInstallTarget :: HackageIndex -> String -> IO (FilePath, PlanOrigin)
resolveInstallTarget index target = do
  isDirectory <- doesDirectoryExist target
  if isDirectory
    then pure (target, PlanLocal)
    else case parsePackageTarget target of
      Nothing ->
        ioError
          ( userError
              (target <> " is not an existing directory nor a Hackage package name (NAME[-VERSION])")
          )
      Just (name, requestedVersion) -> do
        version <- maybe (resolvePreferredVersion index name) pure requestedVersion
        path <-
          HackageDownload.downloadPackageWithOptions
            HackageDownload.defaultDownloadOptions
            PackageSpec {pkgName = name, pkgVersion = version}
        pure (path, PlanHackage)

-- | Split a Hackage target into its package name and optional version.
parsePackageTarget :: String -> Maybe (String, Maybe String)
parsePackageTarget target = do
  packageId <- simpleParsec target :: Maybe CabalPackage.PackageIdentifier
  let version = CabalPackage.pkgVersion packageId
  pure
    ( CabalPackage.unPackageName (CabalPackage.pkgName packageId),
      if version == nullVersion then Nothing else Just (prettyShow version)
    )

-- | The newest non-deprecated version of a package, from the cached Hackage
-- index.
resolvePreferredVersion :: HackageIndex -> String -> IO String
resolvePreferredVersion index name = do
  result <- indexPreferredVersion index name
  either (ioError . userError) pure result

networkDependencyResolver :: HackageIndex -> DependencyResolver
networkDependencyResolver index =
  DependencyResolver
    { resolverResolveVersion = resolvePreferredVersion index,
      resolverSourcePath = fmap (`ResolvedSource` PlanHackage) . HackageDownload.downloadPackageWithOptions HackageDownload.defaultDownloadOptions
    }

-- | Where the packages of a plan go.
--
-- A package that is immutable for this compiler, which is a Hackage release
-- or a core library, is installed into the store under a directory named by
-- its fingerprint and never changed afterwards. A local package builds in
-- place under the build root, where each unit keeps the artifacts of the
-- previous build and replaces only the ones whose inputs changed.
data InstallLocations = InstallLocations
  { locationStoreRoot :: !FilePath,
    locationBuildRoot :: !FilePath,
    -- | Treat local packages as immutable and install them into the store.
    locationImmutable :: !Bool,
    -- | Build the packages the user named again, even where they exist.
    locationReinstall :: !Bool
  }

installPackagePlan :: ModuleCompileConfig -> InstallLocations -> PackagePlan -> IO InstalledPackage
installPackagePlan config locations plan = do
  installed <- newIORef Map.empty
  installPlanNode config locations installed True plan

-- | Install every package of the plans and return the closure, each package
-- once.
installPlanPackages :: ModuleCompileConfig -> InstallLocations -> [PackagePlan] -> IO [InstalledPackage]
installPlanPackages config locations plans = do
  installed <- newIORef Map.empty
  mapM_ (installPlanNode config locations installed True) plans
  Map.elems <$> readIORef installed

installPlanNode :: ModuleCompileConfig -> InstallLocations -> IORef (Map.Map FilePath InstalledPackage) -> Bool -> PackagePlan -> IO InstalledPackage
installPlanNode config locations installed root plan = do
  key <- canonicalizePath (planSourcePath plan)
  known <- Map.lookup key <$> readIORef installed
  case known of
    Just package -> pure package
    Nothing -> do
      -- Only the package the user named is reinstalled.
      let reinstall = root && locationReinstall locations
      dependencies <- mapM (installPlanNode config locations installed False) (planDependencyPlans plan)
      package <-
        if locationImmutable locations || planOrigin plan /= PlanLocal
          then installStorePackage config root reinstall (locationStoreRoot locations) dependencies plan
          else installLocalPackage config reinstall (locationBuildRoot locations) dependencies plan
      modifyIORef' installed (Map.insert key package)
      pure package

data PackageInputs = PackageInputs
  { inputCabalFile :: !FilePath,
    inputDescription :: !GenericPackageDescription,
    inputSources :: ![HackageCabal.FileInfo],
    inputCCompileInfo :: !HackageCabal.CCompileInfo,
    -- | The configure script of a @build-type: Configure@ package.
    inputConfigureScript :: !(Maybe FilePath),
    -- | The headers the package expects its configure script to write.
    inputAutogenIncludes :: ![FilePath]
  }

-- | Read what the installer needs from a planned package. The @.cabal@ file
-- is the one the plan already parsed, so it is not read again here.
readPackageInputs :: ModuleCompileConfig -> PackagePlan -> IO PackageInputs
readPackageInputs config plan = do
  let root = planSourcePath plan
      cabalFile = planCabalFile plan
      gpd = planDescription plan
  let (targetOs, targetArch) = cabalPlatformForTarget (compileTarget config)
  files <- HackageCabal.collectLibraryFilesFor targetOs targetArch gpd root
  configureScript <- case HackageCabal.packageBuildType gpd of
    HackageCabal.Configure -> do
      let script = root </> "configure"
      exists <- doesFileExist script
      unless exists $
        ioError (userError ("The package has build-type Configure but no configure script: " <> script))
      pure (Just script)
    _ -> pure Nothing
  pure
    PackageInputs
      { inputCabalFile = cabalFile,
        inputDescription = gpd,
        inputSources = files,
        inputCCompileInfo = HackageCabal.collectLibraryCCompileInfoFor targetOs targetArch gpd root,
        inputConfigureScript = configureScript,
        inputAutogenIncludes = HackageCabal.collectLibraryAutogenIncludesFor targetOs targetArch gpd
      }

-- | Install an immutable package into the store, unless the store has it.
installStorePackage :: ModuleCompileConfig -> Bool -> Bool -> FilePath -> [InstalledPackage] -> PackagePlan -> IO InstalledPackage
installStorePackage config named reinstall storeRoot dependencies plan = do
  inputs <- readPackageInputs config plan
  (packageDirectory, unitIdentity) <- storePackageIdentity config dependencies inputs
  forM_ dependencies $ \dependency ->
    unless (installedImmutable dependency) $
      ioError
        ( userError
            ( "The package "
                <> T.unpack unitIdentity
                <> " is installed into the store but depends on the local package "
                <> T.unpack (installedIdentity dependency)
                <> ". Pass --immutable to install both into the store."
            )
        )
  let storePath = storeRoot </> packageDirectory
  exists <- doesDirectoryExist storePath
  if exists && not reinstall
    then do
      package <- loadInstalledPackage Set.empty True storePath
      requireInstalledFlags config named package
      pure package
    else do
      createDirectoryIfMissing True storeRoot
      bracket
        (createTemporaryStoreRoot storeRoot packageDirectory)
        removeTemporaryStoreRoot
        (buildAndPublish inputs packageDirectory unitIdentity storePath exists)
  where
    buildAndPublish inputs packageDirectory unitIdentity storePath exists temporaryRoot = do
      built <- installPackageDirect config packageDirectory unitIdentity True temporaryRoot dependencies (planSourcePath plan) inputs
      when exists (removeDirectoryRecursive storePath)
      publishResult <- try (renameDirectory (installStorePath (installedResult built)) storePath)
      case publishResult of
        Right () -> pure (setInstalledStorePath storePath built)
        Left err -> do
          published <- doesDirectoryExist storePath
          if published
            then loadInstalledPackage Set.empty True storePath
            else throwIO (err :: IOException)

-- | Build a local package in place under the build root.
installLocalPackage :: ModuleCompileConfig -> Bool -> FilePath -> [InstalledPackage] -> PackagePlan -> IO InstalledPackage
installLocalPackage config reinstall buildRoot dependencies plan = do
  let root = planSourcePath plan
  inputs <- readPackageInputs config plan
  let (packageDirectory, unitIdentity) = localPackageIdentity inputs
      buildPath = buildRoot </> packageDirectory
  exists <- doesDirectoryExist buildPath
  when (exists && reinstall) (removeDirectoryRecursive buildPath)
  createDirectoryIfMissing True buildPath
  installPackageDirect config packageDirectory unitIdentity False buildRoot dependencies root inputs

-- | The flags the store entry was built with must cover the flags of this
-- install: the entry is never changed, so a missing output stays missing.
-- The extra outputs matter for the package the user named; a dependency
-- only has to have code when code is wanted.
requireInstalledFlags :: ModuleCompileConfig -> Bool -> InstalledPackage -> IO ()
requireInstalledFlags config named package = do
  let built = packageManifestFlags (installedManifest package)
      missing =
        [ flag
        | named,
          (wanted, flag) <-
            [ (compileKeepCore config, "keep-core"),
              (compileKeepGrin config, "keep-grin"),
              (compileKeepLir config, "keep-lir"),
              (compileKeepNative config, "keep-native")
            ],
          wanted,
          flag `notElem` built
        ]
          <> ["code" | not (compileNoCode config), "no-code" `elem` built]
  unless (null missing) $
    ioError
      ( userError
          ( "The store holds "
              <> T.unpack (installedIdentity package)
              <> " built without "
              <> intercalate ", " (map (("--" <>) . T.unpack) missing)
              <> ". Pass --reinstall to build it again."
          )
      )

installPackageDirect :: ModuleCompileConfig -> FilePath -> Text -> Bool -> FilePath -> [InstalledPackage] -> FilePath -> PackageInputs -> IO InstalledPackage
installPackageDirect config packageDirectory unitIdentity immutable storeRoot dependencies root inputs = do
  let target = compileTarget config
      verbose = compileVerbose config
  verbose ("Read Cabal package: " <> root)
  let gpd = inputDescription inputs
  let packageId = package (packageDescription gpd)
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
  let storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId unitIdentity)
  (configuredFiles, cCompileInfo) <- configurePackage config root storePath packageNameText inputs
  let dependencyVersions =
        dependencyVersionsFromManifests
          [(installedName dependency, installedVersion dependency) | dependency <- dependencies]
  files <- preprocessPackage config dependencyVersions root storePath (inputConfigureScript inputs) cCompileInfo configuredFiles
  compiled <- compileModulesWithDependencies config (capiStubOptions files cCompileInfo) storePath root resolvePackage files dependencies
  let parsed = compiledSources compiled
      allExports = compiledExports compiled
      allTypes = compiledTypes compiled
      allScopeHashes = compiledScopeHashes compiled
      allTypeHashes = compiledTypeHashes compiled
      written = compiledWritten compiled
      reused = compiledReused compiled
  unless (compileNoCode config) $ do
    let archive = storePath </> "lib" </> "lib" <> T.unpack packageNameText <> ".a"
    -- A @--lto@ archive holds the wrapper and C objects only: the Haskell
    -- code of the package reaches the executable as System FC.
    moduleObjects <- moduleObjectPaths (not (compileLto config)) storePath target (map sourceName parsed)
    -- The archive follows its objects and the C sources. Both are known
    -- without reading the objects: a unit that wrote an object says so, and
    -- the C sources are hashed for the archive stamp.
    archiveInputs <- archiveInputsHash config root dependencies inputs
    let stampPath = storePath </> "lib" </> "archive.hash"
    previous <- readStampText stampPath
    archiveExists <- doesFileExist archive
    let current = not (Set.null written) || not archiveExists || previous /= Just archiveInputs
    if current
      then do
        cObjects <- compilePackageCFiles target (compileOptimization config) (compileHeaderDirectory config) verbose root storePath cCompileInfo
        buildLibraryArchive target verbose archive (moduleObjects <> cObjects)
        BS8.writeFile stampPath (BS8.pack archiveInputs)
      else verbose ("Reuse archive: " <> archive)
  let manifest =
        PackageManifest
          { packageManifestName = packageNameText,
            packageManifestVersion = packageVersionText,
            packageManifestIdentity = T.pack packageDirectory,
            packageManifestUnitId = unitIdentity,
            packageManifestDependencies = sortOn id (map installedIdentity dependencies),
            packageManifestModules = sortOn id (HackageCabal.collectLibraryExposedModules gpd),
            packageManifestCompiledModules = sortOn id (map sourceName parsed),
            packageManifestFlags = compileFlagNames config
          }
  writePackageManifest (packageManifestPath storePath) manifest
  let exposedNames = Set.fromList (HackageCabal.collectLibraryExposedModules gpd)
      ownExports =
        Map.filterWithKey
          (\moduleKey _ -> moduleKeyPackage moduleKey == resolvePackage && moduleKeyName moduleKey `Set.member` exposedNames)
          allExports
  pure
    InstalledPackage
      { installedResult = InstallResult storePath (Set.toAscList written) (Set.toAscList reused),
        installedName = packageNameText,
        installedVersion = packageVersionText,
        installedIdentity = T.pack packageDirectory,
        installedImmutable = immutable,
        installedManifest = manifest,
        installedExports = ownExports,
        installedTypes = Map.restrictKeys allTypes exposedNames,
        installedScopeHashes = Map.restrictKeys allScopeHashes exposedNames,
        installedTypeHashes = Map.restrictKeys allTypeHashes exposedNames,
        installedInstanceDigest = compiledInstanceDigest compiled,
        installedInstanceFacts = compiledInstanceFacts compiled,
        installedInstanceProviders = Map.restrictKeys (compiledInstanceProviders compiled) exposedNames
      }

readStampText :: FilePath -> IO (Maybe String)
readStampText path = do
  exists <- doesFileExist path
  if exists then Just . BS8.unpack <$> BS.readFile path else pure Nothing

-- | Whether @--keep-native@ and @--keep-lir@ name the same file for the
-- target of the build. An object backend writes its object itself, so the
-- Lir text is the only source there is to keep beside it.
keepNativeIsKeepLir :: ModuleCompileConfig -> Bool
keepNativeIsKeepLir config = nativeSourceIsLir (compileTarget config)

-- | Whether the build keeps the Lir text of each module.
keepsLirText :: ModuleCompileConfig -> Bool
keepsLirText config = compileKeepLir config || (compileKeepNative config && keepNativeIsKeepLir config)

-- | The names of the flags an installed package records in its manifest.
compileFlagNames :: ModuleCompileConfig -> [Text]
compileFlagNames config =
  [ flag
  | (set, flag) <-
      [ (compileKeepCore config, "keep-core"),
        (compileKeepGrin config, "keep-grin"),
        (compileKeepLir config, "keep-lir"),
        (compileKeepNative config, "keep-native"),
        (compileLint config, "lint"),
        (compileCheckPrimBounds config, "check-prim-bounds"),
        (compileLto config, "lto"),
        (compileNoCode config, "no-code"),
        (compileOptimization config /= defaultOptimizationLevel, optimizationFlagName (compileOptimization config))
      ],
    set
  ]

compileModules :: ModuleCompileConfig -> ModuleCompileRequest -> IO ModuleCompileResult
compileModules config request = do
  compiled <-
    compileModulesWithDependencies
      config
      (compileCapiStubOptions request)
      (compileOutputRoot request)
      (compilePackageRoot request)
      (compilePackage request)
      (compileSourceFiles request)
      (compileDependencies request)
  let names = map sourceName (compiledSources compiled)
  objects <- moduleObjectPaths (not (compileLto config)) (compileOutputRoot request) (compileTarget config) names
  pure ModuleCompileResult {compileObjectPaths = objects, compileModuleNames = names}

-- | The objects of a set of modules: one for each module when the modules
-- have objects, and the capi wrappers of those that declare any.
--
-- The wrappers are found on disk rather than reported by the backend,
-- because a module whose artifacts were reused compiled nothing this time and
-- still has the wrapper object it built before.  A module that no longer
-- declares a capi import has had its wrapper object removed, so what is there
-- is what belongs in the link.
moduleObjectPaths :: Bool -> FilePath -> NativeTarget -> [Text] -> IO [FilePath]
moduleObjectPaths withModuleObjects root target names = do
  capiObjects <- filterM doesFileExist [outputCapiObjectPath (paths name) | name <- names]
  pure (sortOn id ([outputObjectPath (paths name) | withModuleObjects, name <- names] <> capiObjects))
  where
    paths = moduleOutputPaths root target

compileModulesWithDependencies :: ModuleCompileConfig -> CapiStubOptions -> FilePath -> FilePath -> Package -> [HackageCabal.FileInfo] -> [InstalledPackage] -> IO CompiledPackageModules
compileModulesWithDependencies config capiOptions outputRoot packageRoot resolvePackage files dependencies = do
  let verbose = compileVerbose config
  verbose ("Parse " <> show (length files) <> " modules")
  capabilities <- getNumCapabilities
  let versions =
        dependencyVersionsFromManifests
          [(installedName dependency, installedVersion dependency) | dependency <- dependencies]
  (parsed, importTimings) <- loadSourceModules (compileHeaderDirectory config) (max 1 capabilities) packageRoot versions files
  -- The two serial stretches between the task graphs. Neither runs a task,
  -- so both show as idle workers on the timeline, and both build their
  -- result lazily: forcing them here is what puts the time on the line
  -- that names the work rather than on whichever task first asks for it.
  setupStart <- getMonotonicTimeNSec
  loadedDependencies <- evaluate . force =<< loadRequiredDependencies parsed dependencies
  let dependencyExports = Map.unions (map installedExports loadedDependencies)
      dependencyTypes = LazyMap.unions (map installedTypes loadedDependencies)
      dependencyScopeHashes = Map.unions (map installedScopeHashes loadedDependencies)
      dependencyTypeHashes = LazyMap.unions (map installedTypeHashes loadedDependencies)
      dependencyPackages =
        [ (installedName dependency, installedInstanceDigest dependency, Map.keysSet (installedTypeHashes dependency))
        | dependency <- loadedDependencies
        ]
      dependencyInstanceFacts = mergeTcInterfaces (configMergeCheck config) (map installedInstanceFacts loadedDependencies)
      dependencyInstanceProviders = Map.unions (map installedInstanceProviders loadedDependencies)
      primIdentity = packagePrimIdentity resolvePackage dependencyExports
  _ <-
    evaluate
      ( force
          ( dependencyExports,
            dependencyTypes,
            dependencyScopeHashes,
            dependencyTypeHashes,
            dependencyPackages,
            dependencyInstanceFacts,
            dependencyInstanceProviders
          )
      )
  setupEnd <- getMonotonicTimeNSec
  depgraphStart <- getMonotonicTimeNSec
  units <- evaluate (sourceModuleUnits parsed)
  -- The graph this phase builds is which units there are and which units
  -- each waits on. The modules in them are its input, forced when they
  -- were parsed; forcing them here would only move that work out of the
  -- parse tasks that run in parallel.
  _ <- evaluate (force [(sourceUnitId unit, sourceUnitDependencies unit) | unit <- units])
  depgraphEnd <- getMonotonicTimeNSec
  backendPhaseTimings <- newIORef mempty
  let taskContext =
        PackageTaskContext
          { taskModuleCompileConfig = config,
            taskStorePath = outputRoot,
            taskResolvePackage = resolvePackage,
            taskPrimIdentity = primIdentity,
            taskPackageRoot = packageRoot,
            taskDependencyExports = dependencyExports,
            taskDependencyScopeHashes = dependencyScopeHashes,
            taskDependencyTypes = dependencyTypes,
            taskDependencyTypeHashes = dependencyTypeHashes,
            taskDependencyPackages = dependencyPackages,
            taskDependencyInstanceFacts = dependencyInstanceFacts,
            taskDependencyInstanceProviders = dependencyInstanceProviders,
            taskCapiStubOptions = capiOptions,
            taskBackendPhaseTimings = backendPhaseTimings
          }
  verbose ("Compute " <> show (length units) <> " SCC units")
  (runtimes, taskTimings) <- runPackageTasks taskContext (max 1 capabilities) units
  resolveResults <- mapM (atomically . readTMVar . runtimeResolveResult) runtimes
  phaseTimings <- readIORef backendPhaseTimings
  compilePrintTimings
    config
    ( renderTaskTimeline
        (compileUseColor config)
        [ ("Setup", setupEnd - setupStart),
          ("Depgraph", depgraphEnd - depgraphStart)
        ]
        (importTimings <> taskTimings)
        <> renderBackendPhaseTotals phaseTimings
    )
  typeResults <- mapM (atomically . readTMVar . runtimeTypeResult) runtimes
  let parseDiagnostics = concatMap (concatMap sourceModuleParseDiagnostics . sourceUnitSources . runtimeUnit) runtimes
      resolveDiagnostics = concatMap resolveUnitErrors resolveResults
      -- An unlocated diagnostic names the modules of its unit.
      typeDiagnostics =
        concat
          [ [(unitLabel (runtimeUnit runtime), diagnostic) | diagnostic <- typeUnitDiagnostics result, diagSeverity diagnostic == TcError]
          | (runtime, result) <- zip runtimes typeResults
          ]
  frontendFailure <- renderFrontendFailure (excerptSourceLoader (compileHeaderDirectory config) packageRoot versions files) parseDiagnostics resolveDiagnostics typeDiagnostics
  unless (null frontendFailure) (ioError (userError frontendFailure))
  let localExports = Map.unions (map resolveUnitExports resolveResults)
      localScopeHashes = Map.unions (map resolveUnitScopeHashes resolveResults)
      localTypes = Map.unions (map typeUnitTypes typeResults)
      localTypeHashes = Map.unions (map typeUnitHashes typeResults)
      allExports = localExports `Map.union` dependencyExports
      allScopeHashes = localScopeHashes `Map.union` dependencyScopeHashes
      allTypes = localTypes `LazyMap.union` dependencyTypes
      allTypeHashes = localTypeHashes `LazyMap.union` dependencyTypeHashes
      packageInstanceInterface = mergeTcInterfaces (configMergeCheck config) (dependencyInstanceFacts : map typeUnitOwnInstanceInterface typeResults)
      instanceProviders =
        Map.fromList
          [ (sourceName source, interfaceInstanceProviders (typeUnitInstanceInterface result))
          | (runtime, result) <- zip runtimes typeResults,
            source <- sourceUnitSources (runtimeUnit runtime)
          ]
  instanceDigest <- writePackageInstanceArtifact verbose outputRoot instanceProviders packageInstanceInterface
  -- A consumer takes the digests from here rather than encoding the
  -- interfaces again.
  writeStamp
    (packageDigestsPath outputRoot)
    PackageDigests
      { packageDigestsModules =
          Map.fromList
            [ (sourceName source, ModuleDigests scopeDigest typeDigest)
            | source <- parsed,
              Just scopeDigest <- [Map.lookup (sourceName source) localScopeHashes],
              Just typeDigest <- [Map.lookup (sourceName source) localTypeHashes]
            ],
        packageDigestsInstances = instanceDigest
      }
  pure
    CompiledPackageModules
      { compiledSources = parsed,
        compiledExports = allExports,
        compiledTypes = allTypes,
        compiledScopeHashes = allScopeHashes,
        compiledTypeHashes = allTypeHashes,
        compiledInstanceDigest = instanceDigest,
        compiledInstanceFacts = packageInstanceInterface,
        compiledInstanceProviders = instanceProviders,
        compiledWritten = Set.unions (map typeUnitWritten typeResults),
        compiledReused = Set.unions (map typeUnitReused typeResults)
      }

-- | The kind vocabulary of the aihc core libraries, given the identity of
-- the primitive package.
primKinds :: PackageId -> TcKinds
primKinds = mkTcKinds . primTcWiring

packagePrimIdentity :: Package -> ModuleExports -> PackageId
packagePrimIdentity resolvePackage dependencyExports =
  fromMaybe (PackageId "aihc-prim") $
    if packageName resolvePackage == "aihc-prim"
      then Just (packageId resolvePackage)
      else
        listToMaybe
          [ dependencyIdentity
          | ModuleKey (Package dependencyName dependencyIdentity) _ <- Map.keys dependencyExports,
            dependencyName == "aihc-prim"
          ]

-- | The directory name and unit identity of a package in the store.
--
-- The fingerprint is a function of the plan: the package name and version,
-- the compiler, the target, and the identities of the dependencies. It does
-- not read the sources, so a consumer computes it without them. A Hackage
-- release never changes, and a core library is identified by the compiler
-- it ships with.
storePackageIdentity :: ModuleCompileConfig -> [InstalledPackage] -> PackageInputs -> IO (FilePath, Text)
storePackageIdentity config dependencies inputs = do
  let (unitIdentity, packageNameText, packageVersionText) = packageUnitIdentity inputs
      cInputs = inputCCompileInfo inputs
      -- A configure script answers for the sysroot it saw, and its answers
      -- reach the Haskell sources through the CPP pass, so they count even
      -- without code.
      -- So does hsc2hs, which computes its constants with the C compiler of
      -- the target.
      usesPreprocessor = any (isJust . HackageCabal.fileInfoPreprocessor) (inputSources inputs)
      usesSysroot = isJust (inputConfigureScript inputs) || usesPreprocessor || not (compileNoCode config || null (HackageCabal.cCompileSources cInputs))
  cSysrootArguments <-
    if usesSysroot
      then wasmSysrootIncludeArguments (compileTarget config)
      else pure []
  let fingerprint =
        stableHash
          ( map
              TE.encodeUtf8
              ( packageArtifactFormatVersion
                  : T.pack (packageOptionsKey config)
                  : T.pack (show cSysrootArguments)
                  : packageNameText
                  : packageVersionText
                  : sortOn id (map installedIdentity dependencies)
              )
          )
  pure (T.unpack unitIdentity <> "-" <> take 16 fingerprint, unitIdentity)

-- | The directory name and unit identity of a package in a build directory.
localPackageIdentity :: PackageInputs -> (FilePath, Text)
localPackageIdentity inputs =
  let (unitIdentity, _, _) = packageUnitIdentity inputs
   in (T.unpack unitIdentity, unitIdentity)

packageUnitIdentity :: PackageInputs -> (Text, Text, Text)
packageUnitIdentity inputs =
  let packageId = package (packageDescription (inputDescription inputs))
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
   in (packageNameText <> "-" <> packageVersionText, packageNameText, packageVersionText)

-- | What the package archive depends on besides the module objects.
archiveInputsHash :: ModuleCompileConfig -> FilePath -> [InstalledPackage] -> PackageInputs -> IO String
archiveInputsHash config root dependencies inputs = do
  let cInputs = inputCCompileInfo inputs
  sourceHash <- sourceFilesHash root (inputCabalFile inputs : HackageCabal.cCompileSources cInputs)
  cSysrootArguments <-
    if null (HackageCabal.cCompileSources cInputs)
      then pure []
      else wasmSysrootIncludeArguments (compileTarget config)
  -- The C sources include the headers configure wrote.
  configureHash <- maybe (pure "") (configureInputsHash config) (inputConfigureScript inputs)
  pure
    ( stableHash
        ( map
            TE.encodeUtf8
            ( T.pack (backendOptionsKey config)
                : T.pack sourceHash
                : T.pack (show cSysrootArguments)
                : T.pack configureHash
                : sortOn id (map installedIdentity dependencies)
            )
        )
    )

buildEnvironmentIdentity :: NativeTarget -> IO String
buildEnvironmentIdentity target = do
  (compiler, arguments) <- backendCompiler target
  archiver <- backendArchiver target
  compilerHash <- executableIdentity compiler
  archiverHash <- executableIdentity archiver
  let headerHash = compilerHeaderIdentity target
  pure (stableHash (map BS8.pack [compilerBuildIdentity, compilerHash, archiverHash, headerHash, show arguments]))

-- | The part of the configuration that changes what a package is: the
-- compiler, the target, the optimization level, whether the package stops
-- at System FC, and whether its array primitives check their bounds. Flags
-- that add or drop outputs, such as @--keep-core@, or that only check,
-- such as @--lint@, are recorded in the manifest instead.
packageOptionsKey :: ModuleCompileConfig -> String
packageOptionsKey config = stableHash (compilerKeyParts config <> optimizationKeyParts config <> ltoKeyParts config <> checkPrimBoundsKeyParts config)

-- | The compiler and the target.
compilerKeyParts :: ModuleCompileConfig -> [BS8.ByteString]
compilerKeyParts config =
  [ BS8.pack (compileBuildIdentity config),
    TE.encodeUtf8 packageArtifactFormatVersion,
    BS8.pack (show (compileTarget config))
  ]

-- | The key part of a level that is not the default. The default level
-- adds nothing, so the keys of a default build are the keys of a build
-- before the level existed, and the store entries of such a build stay
-- valid.
optimizationKeyParts :: ModuleCompileConfig -> [BS8.ByteString]
optimizationKeyParts config
  | level == defaultOptimizationLevel = []
  | otherwise = [TE.encodeUtf8 (optimizationFlagName level)]
  where
    level = compileOptimization config

-- | The name a level goes by in a manifest flag and in a store key.
optimizationFlagName :: OptimizationLevel -> Text
optimizationFlagName level = "O" <> T.pack (renderOptimizationLevel level)

-- | The key part of a @--lto@ build. A build without the flag adds nothing,
-- so its keys stay the keys of a build before the flag existed.
ltoKeyParts :: ModuleCompileConfig -> [BS8.ByteString]
ltoKeyParts config = ["lto" | compileLto config]

-- | The key part of a @--check-prim-bounds@ build. A build without the
-- flag adds nothing, so its keys stay the keys of a build before the flag
-- existed.
checkPrimBoundsKeyParts :: ModuleCompileConfig -> [BS8.ByteString]
checkPrimBoundsKeyParts config = ["check-prim-bounds" | compileCheckPrimBounds config]

-- | The part of the configuration the type interfaces depend on. The level
-- changes only C and LLVM objects, so a local package that changes its
-- level keeps its interfaces.
frontendOptionsKey :: ModuleCompileConfig -> String
frontendOptionsKey config = stableHash (compilerKeyParts config)

-- | The part of the configuration the backend outputs depend on.
backendOptionsKey :: ModuleCompileConfig -> String
backendOptionsKey config =
  stableHash
    ( [ BS8.pack (compileBuildIdentity config),
        TE.encodeUtf8 packageArtifactFormatVersion,
        BS8.pack (show (compileTarget config, compileKeepCore config, compileKeepGrin config, compileKeepNative config, compileLint config))
      ]
        <> optimizationKeyParts config
        <> ltoKeyParts config
        <> checkPrimBoundsKeyParts config
    )

createTemporaryStoreRoot :: FilePath -> FilePath -> IO FilePath
createTemporaryStoreRoot storeRoot packageDirectory = do
  (path, handle) <- openBinaryTempFile storeRoot (".tmp-" <> packageDirectory <> "-")
  hClose handle
  removeFile path
  createDirectory path
  pure path

removeTemporaryStoreRoot :: FilePath -> IO ()
removeTemporaryStoreRoot path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)

setInstalledStorePath :: FilePath -> InstalledPackage -> InstalledPackage
setInstalledStorePath storePath installed =
  installed
    { installedResult =
        (installedResult installed)
          { installStorePath = storePath
          }
    }

loadRequiredDependencies :: [SourceModule] -> [InstalledPackage] -> IO [InstalledPackage]
loadRequiredDependencies sources = mapM loadDependency
  where
    requirements = requiredDependencyModules sources
    loadDependency dependency = loadInstalledPackage requirements (installedImmutable dependency) (installStorePath (installedResult dependency))

requiredDependencyModules :: [SourceModule] -> Set.Set (Maybe Text, Text)
requiredDependencyModules sources =
  Set.fromList
    ( [ importDecl
      | source <- sources,
        importDecl <- sourceModuleImports source,
        not (localImport importDecl)
      ]
        <> [(Nothing, "Prelude") | any moduleUsesImplicitPrelude sources]
        <> [(Nothing, name) | name <- wiredInterfaceModules]
    )
  where
    localNames = Set.fromList (map sourceName sources)
    localImport (package, name) =
      package == Just "this" || (isNothing package && name `Set.member` localNames)

loadInstalledPackage :: Set.Set (Maybe Text, Text) -> Bool -> FilePath -> IO InstalledPackage
loadInstalledPackage requirements immutable storePath = do
  manifestResult <- readPackageManifest (packageManifestPath storePath)
  manifest <- either (ioError . userError . ("Invalid installed package manifest: " <>)) pure manifestResult
  digests <-
    readStamp (packageDigestsPath storePath)
      >>= maybe (ioError (userError ("The installed package has no digests: " <> storePath))) pure
  let selectedModules = filter (moduleRequired manifest) (packageManifestModules manifest)
  entries <- mapM loadModule selectedModules
  (decodedFacts, instanceProviders) <-
    if null selectedModules
      then pure (emptyTcInterface, Map.empty)
      else loadPackageInstances selectedModules
  -- A written interface holds each of its parts once and names it
  -- everywhere it is used, so the interfaces read above are already
  -- shared within themselves; nothing here has to look for equal parts.
  let instanceFacts' = decodedFacts
      interfaces = [interface | (_, _, interface) <- entries]
      package = Package (packageManifestName manifest) (PackageId (packageManifestUnitId manifest))
      exports = Map.fromList [(ModuleKey package name, scope) | (name, scope, _) <- entries]
      types = LazyMap.fromList (zip [name | (name, _, _) <- entries] interfaces)
      exposed = Map.restrictKeys (packageDigestsModules digests) (Set.fromList (packageManifestModules manifest))
      scopeHashes = Map.map moduleScopeDigest exposed
      typeHashes = Map.map moduleTypeDigest exposed
  pure
    InstalledPackage
      { installedResult = InstallResult storePath [] (packageManifestModules manifest),
        installedName = packageManifestName manifest,
        installedVersion = packageManifestVersion manifest,
        installedIdentity = packageManifestIdentity manifest,
        installedImmutable = immutable,
        installedManifest = manifest,
        installedExports = exports,
        installedTypes = types,
        installedScopeHashes = scopeHashes,
        installedTypeHashes = typeHashes,
        installedInstanceDigest = packageDigestsInstances digests,
        installedInstanceFacts = instanceFacts',
        installedInstanceProviders = instanceProviders
      }
  where
    moduleRequired manifest name =
      any
        (\(packageName', moduleName') -> moduleName' == name && maybe True (== packageManifestName manifest) packageName')
        (Set.toList requirements)

    loadModule name = do
      let root = storePath </> moduleNameDirectory name
          resolvePath = root </> "resolve.cbor"
          typePath = root </> "type.cbor"
      resolveBytes <- BS.readFile resolvePath
      resolveArtifact <- either (ioError . userError . (("Invalid resolve artifact " <> resolvePath <> ": ") <>)) pure (decodeResolveArtifact resolveBytes)
      typeBytes <- BL.readFile typePath
      typeArtifact <- readTypeArtifact typePath typeBytes
      unless (resolveArtifactModuleName resolveArtifact == name) (ioError (userError ("Resolve artifact module name does not match " <> resolvePath)))
      unless (typeArtifactModuleName typeArtifact == name) (ioError (userError ("Type artifact module name does not match " <> typePath)))
      pure (name, resolveArtifactScope resolveArtifact, typeArtifactInterface typeArtifact)

    loadPackageInstances selected = do
      let path = storePath </> "instances.cbor"
      exists <- doesFileExist path
      if not exists
        then pure (emptyTcInterface, Map.empty)
        else do
          bytes <- BL.readFile path
          artifact <- readTypeArtifact path bytes
          unless (typeArtifactModuleName artifact == "$package-instances") (ioError (userError ("Package instance artifact name does not match " <> path)))
          let providers = Map.restrictKeys (Map.map Set.fromList (typeArtifactInstanceProviders artifact)) (Set.fromList selected)
              visibleProviders = Set.unions (Map.elems providers)
          pure (selectInstanceProviders (typeArtifactInterface artifact) visibleProviders, providers)

parseSource :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO SourceModule
parseSource headerDir root versions fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  ParsedInterfaceFile
    { parsedFilePath = path,
      parsedFileModule = modu,
      parsedFileParseDiagnostics = parseDiagnostics,
      parsedFileCppDiagnostics = cppDiagnostics,
      parsedFileExtensions = extensions,
      parsedFileDeps = deps
    } <-
    parseInterfaceBytes headerDir root versions fileInfo bytes
  let (cppWarnings, cppErrors) = partition isCppWarning cppDiagnostics
  mapM_ (hPutStrLn stderr . renderHumanDiagnostic "cpp") cppWarnings
  unless (null cppErrors) $
    ioError (userError ("Preprocess failed:\n" <> concatMap (renderHumanDiagnostic "cpp") cppErrors))
  -- The effective extensions of the module: the cabal default extensions,
  -- the language edition and the module's own pragmas folded into one set.
  -- Name resolution and the type checker take that set as data, so neither
  -- reads the pragmas again.
  let name = fromMaybe "Main" (moduleName modu)
      imports = [(importDeclPackage importDecl, importDeclModule importDecl) | importDecl <- Syntax.moduleImports modu]
  parsed <- newMVar modu
  -- Built here rather than returned as a thunk: the strict fields below
  -- are what the phases after this one read instead of the parse tree,
  -- and they only run when the record is built. Left to 'pure', the
  -- first read of any of them built every module's -- the digest, the
  -- name, the imports -- on the serial stretch before the task graph.
  evaluate
    SourceModule
      { sourceModulePath = path,
        sourceModuleSize = BS.length bytes,
        sourceModuleHash = moduleDepsDigest deps,
        sourceModuleParsed = parsed,
        sourceModuleName = name,
        sourceModuleDirectory = moduleNameDirectory name,
        sourceModuleImports = imports,
        sourceModuleExtensions = extensions,
        sourceModuleParseDiagnostics = parseDiagnostics
      }

isCppWarning :: Value -> Bool
isCppWarning (Object diagnostic) = KeyMap.lookup "severity" diagnostic == Just (String "Warning")
isCppWarning _ = False

loadSourceModules :: FilePath -> Int -> FilePath -> DependencyVersions -> [HackageCabal.FileInfo] -> IO ([SourceModule], [TaskTiming])
loadSourceModules headerDir workers root versions files = do
  results <- mapM (const newEmptyTMVarIO) files
  let tasks = zipWith3 loadTask [0 ..] files results
  timings <- runTaskGraph workers tasks
  sources <- mapM (atomically . readTMVar) results
  pure (sources, timings)
  where
    loadTask order fileInfo result =
      Task
        { taskId = TaskId order,
          taskKind = TaskParse,
          taskOrder = order,
          taskDependencies = Set.empty,
          -- The header fields of the module are strict, so the import
          -- list is known once the source exists.
          taskAction = parseSource headerDir root versions fileInfo >>= atomically . putTMVar result
        }

sourceModuleUnits :: [SourceModule] -> [SourceUnit]
sourceModuleUnits sources = zipWith makeUnit [0 ..] orderedComponents
  where
    node source = (source, sourceName source, moduleDependencies source)
    moduleDependencies source =
      nub (filter (/= sourceName source) wiredTypeModules <> sourceDependencyNames source)
    flatten (AcyclicSCC value) = [value]
    flatten (CyclicSCC values) = values
    components = map (sortOn sourceName . flatten) (stronglyConnComp (map node sources))
    componentNames = Map.fromList [(sourceName source, index) | (index, component) <- zip [0 ..] components, source <- component]
    dependenciesFor component =
      Set.toAscList $
        Set.fromList
          [ dependencyIndex
          | source <- component,
            dependency <- moduleDependencies source,
            Just dependencyIndex <- [Map.lookup dependency componentNames],
            dependencyIndex /= fromMaybe (-1) (Map.lookup (sourceName source) componentNames)
          ]
    componentDependencies = Map.fromList [(index, dependenciesFor component) | (index, component) <- zip [0 ..] components]
    componentLabel component = minimum (map sourceName component)
    orderedIndices = canonicalTopologicalOrder components componentDependencies componentLabel
    orderedComponents = [components !! index | index <- orderedIndices]
    orderedIdByOldIndex = Map.fromList [(oldIndex, UnitId order) | (order, oldIndex) <- zip [0 ..] orderedIndices]
    makeUnit order component =
      let oldIndex =
            fromMaybe (error "missing source component") $
              listToMaybe component >>= (\source -> Map.lookup (sourceName source) componentNames)
       in SourceUnit
            { sourceUnitId = UnitId order,
              sourceUnitOrder = order,
              sourceUnitSources = component,
              sourceUnitDependencies =
                sortOn
                  id
                  [ dependencyId
                  | dependencyIndex <- Map.findWithDefault [] oldIndex componentDependencies,
                    Just dependencyId <- [Map.lookup dependencyIndex orderedIdByOldIndex]
                  ]
            }

canonicalTopologicalOrder :: [[SourceModule]] -> Map.Map Int [Int] -> ([SourceModule] -> Text) -> [Int]
canonicalTopologicalOrder components dependencies label = go Set.empty []
  where
    componentCount = length components
    go complete ordered
      | Set.size complete == componentCount = reverse ordered
      | otherwise =
          case sortOn
            (label . (components !!))
            [ index
            | index <- [0 .. componentCount - 1],
              index `Set.notMember` complete,
              all (`Set.member` complete) (Map.findWithDefault [] index dependencies)
            ] of
            [] -> error "source component graph is cyclic"
            index : _ -> go (Set.insert index complete) (index : ordered)

renderResolveErrors :: DiagnosticSourceMap -> [ResolveError] -> String
renderResolveErrors sourceLines errors =
  "Name resolution failed:\n"
    <> intercalate "\n\n" (map (renderResolveError sourceLines) errors)
    <> "\n"

renderResolveError :: DiagnosticSourceMap -> ResolveError -> String
renderResolveError sourceLines resolveError =
  case resolveError of
    ResolveResolutionError Nothing name namespace message ->
      "error: " <> renderResolveMessage message name namespace
    ResolveResolutionError (Just sourceSpan) name namespace message ->
      renderResolveLocation sourceSpan
        <> ": error: "
        <> renderResolveMessage message name namespace
        <> renderResolveExcerpt sourceLines sourceSpan
    ResolveNotImplemented message -> "error: not implemented: " <> message

renderResolveLocation :: SourceSpan -> String
renderResolveLocation (SourceSpan sourcePath startLine startColumn _ _ _ _) =
  T.unpack sourcePath <> ":" <> show startLine <> ":" <> show startColumn

renderResolveMessage :: String -> Text -> ResolutionNamespace -> String
renderResolveMessage message name namespace
  | message == "unbound" = "unbound " <> renderedNamespace <> " name ‘" <> T.unpack name <> "’"
  | message == "not found" = renderedNamespace <> " ‘" <> T.unpack name <> "’ not found"
  | otherwise = message <> ": " <> renderedNamespace <> " name ‘" <> T.unpack name <> "’"
  where
    renderedNamespace =
      case namespace of
        ResolutionNamespaceTerm -> "term"
        ResolutionNamespaceType -> "type"
        ResolutionNamespaceModule -> "module"

renderResolveExcerpt :: DiagnosticSourceMap -> SourceSpan -> String
renderResolveExcerpt sourceLines sourceSpan =
  case sourceSpan of
    SourceSpan sourcePath startLine startColumn endLine endColumn _ _ ->
      case Map.lookup (T.unpack sourcePath) sourceLines >>= Map.lookup startLine of
        Nothing -> ""
        Just sourceLine ->
          let lineNumber = show startLine
              gutterWidth = length lineNumber
              caretStart = max 0 (startColumn - 1)
              caretWidth
                | startLine == endLine = max 1 (endColumn - startColumn)
                | otherwise = max 1 (T.length sourceLine - caretStart)
           in "\n  "
                <> lineNumber
                <> " | "
                <> T.unpack sourceLine
                <> "\n  "
                <> replicate gutterWidth ' '
                <> " | "
                <> replicate caretStart ' '
                <> replicate caretWidth '^'

-- | The report of a failed frontend. The excerpts load the source files
-- again here: nothing keeps the lines of every module in memory for the
-- rare build that needs a few of them.
renderFrontendFailure :: (FilePath -> IO DiagnosticSourceMap) -> [Value] -> [ResolveError] -> [(Text, TcDiagnostic)] -> IO String
renderFrontendFailure loadSource parseDiagnostics resolveDiagnostics typeDiagnostics = do
  sourceLines <-
    loadExcerptSources
      loadSource
      ( [sourceSpan | ResolveResolutionError (Just sourceSpan) _ _ _ <- resolveDiagnostics]
          <> [sourceSpan | (_, diagnostic) <- typeDiagnostics, Just sourceSpan <- [diagLoc diagnostic]]
      )
  let sections =
        [renderParseDiagnostics parseDiagnostics | not (null parseDiagnostics)]
          <> [renderResolveErrors sourceLines resolveDiagnostics | not (null resolveDiagnostics)]
          <> [renderTypeErrors sourceLines typeDiagnostics | not (null typeDiagnostics)]
  pure $
    case sections of
      [] -> ""
      _ -> intercalate "\n\n" (map dropFinalNewlines sections) <> "\n"
  where
    dropFinalNewlines = reverse . dropWhile (== '\n') . reverse

-- | The lines of the files that some spans point into, by file and line.
loadExcerptSources :: (FilePath -> IO DiagnosticSourceMap) -> [SourceSpan] -> IO DiagnosticSourceMap
loadExcerptSources loadSource spans =
  Map.unionsWith Map.union
    <$> mapM loadSource (nub (map (T.unpack . sourceSpanSourceName) spans))

-- | How the excerpts of a package's diagnostics find their lines. A module
-- of the package is read through the preprocessor again, so an excerpt
-- shows the line as the compiler saw it and maps included files back to
-- their own paths, exactly as the parse did. Any other file (a header a
-- span points into) is read as it is. A file that cannot be read gets no
-- excerpt.
excerptSourceLoader :: FilePath -> FilePath -> DependencyVersions -> [HackageCabal.FileInfo] -> FilePath -> IO DiagnosticSourceMap
excerptSourceLoader headerDir root versions files path =
  case Map.lookup path fileInfos of
    Just fileInfo -> do
      bytes <- BS.readFile path
      parsedFileSourceLines <$> parseInterfaceBytes headerDir root versions fileInfo bytes
    Nothing -> do
      result <- try (BS.readFile path)
      pure $
        case result of
          Left (_ :: IOException) -> Map.empty
          Right bytes -> Map.singleton path (Map.fromList (zip [1 ..] (T.lines (TE.decodeUtf8With lenientDecode bytes))))
  where
    fileInfos = Map.fromList [(HackageCabal.fileInfoPath fileInfo, fileInfo) | fileInfo <- files]

renderParseDiagnostics :: [Value] -> String
renderParseDiagnostics diagnostics =
  "Parse failed:\n" <> intercalate "\n" (map (renderHumanDiagnostic "parse") diagnostics)

renderTypeErrors :: DiagnosticSourceMap -> [(Text, TcDiagnostic)] -> String
renderTypeErrors sourceLines diagnostics =
  "Type check failed:\n"
    <> intercalate "\n\n" (map renderTypeError diagnostics)
    <> "\n"
  where
    renderTypeError (label, diagnostic) =
      case diagLoc diagnostic of
        Nothing -> "<unknown location in " <> T.unpack label <> ">: error: " <> renderTypeErrorKind (diagKind diagnostic)
        Just sourceSpan ->
          renderResolveLocation sourceSpan
            <> ": error: "
            <> renderTypeErrorKind (diagKind diagnostic)
            <> renderResolveExcerpt sourceLines sourceSpan

renderTypeErrorKind :: TcErrorKind -> String
renderTypeErrorKind kind =
  case kind of
    UnificationError left right _ _ ->
      "could not match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    FunDepUnknownTyVar className name ->
      "the functional dependency of class " <> T.unpack className <> " names " <> T.unpack name <> ", which is not a parameter of the class"
    InstanceFunDepCoverage predicate determiners determined ->
      "instance " <> renderPred predicate <> " does not determine " <> unwords (map T.unpack determined) <> " from " <> unwords (map T.unpack determiners)
    InstanceFunDepConflict predicate other determiners determined ->
      "instance " <> renderPred predicate <> " conflicts with instance " <> renderPred other <> " under the functional dependency " <> renderFunDepNames determiners determined
    OtherError message ->
      message

runPackageTasks :: PackageTaskContext -> Int -> [SourceUnit] -> IO ([UnitRuntime], [TaskTiming])
runPackageTasks context workers units = do
  runtimes <-
    forM units $ \unit ->
      UnitRuntime unit <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO
  let runtimeMap = Map.fromList [(sourceUnitId (runtimeUnit runtime), runtime) | runtime <- runtimes]
      -- The task numbering of the package. The parse tasks take the indices
      -- of the modules, which the units partition in order, and the three
      -- phases of a unit take three indices each above them.
      sourceBases = scanl (+) 0 (map (length . sourceUnitSources) units)
      sourceCount = sum (map (length . sourceUnitSources) units)
      tasks = concat (zipWith (unitTasks runtimeMap sourceCount) sourceBases runtimes)
  timings <- runTaskGraph workers tasks
  pure (runtimes, timings)
  where
    unitTasks runtimeMap sourceCount sourceBase runtime =
      zipWith (parseTask runtime) [sourceBase ..] (sourceUnitSources unit)
        <> [ resolveTask runtimeMap sourceCount sourceBase runtime,
             typeTask runtimeMap sourceCount runtime
           ]
        <> [backendTask sourceCount runtime | not (compileNoCode config)]
      where
        unit = runtimeUnit runtime

    parseTask runtime index source =
      Task
        { taskId = TaskId index,
          taskKind = TaskParse,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies = Set.empty,
          taskAction = do
            modu <- readMVar (sourceModuleParsed source)
            evaluate (rnf (modu, sourceModuleParseDiagnostics source))
        }

    resolveTask runtimeMap sourceCount sourceBase runtime =
      Task
        { taskId = resolveTaskId sourceCount (sourceUnitId (runtimeUnit runtime)),
          taskKind = TaskResolve,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( [TaskId index | index <- take (length (sourceUnitSources (runtimeUnit runtime))) [sourceBase ..]]
                  <> map (resolveTaskId sourceCount) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runResolveUnit
              context
              runtimeMap
              runtime
        }

    typeTask runtimeMap sourceCount runtime =
      Task
        { taskId = typeTaskId sourceCount (sourceUnitId (runtimeUnit runtime)),
          taskKind = TaskTypeCheck,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( resolveTaskId sourceCount (sourceUnitId (runtimeUnit runtime))
                  : map (typeTaskId sourceCount) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runTypeUnit
              context
              runtimeMap
              runtime
        }

    backendTask sourceCount runtime =
      Task
        { taskId = backendTaskId sourceCount (sourceUnitId (runtimeUnit runtime)),
          taskKind = TaskBackend,
          taskOrder = negate (sum (map sourceModuleSize (sourceUnitSources (runtimeUnit runtime)))),
          taskDependencies = Set.singleton (typeTaskId sourceCount (sourceUnitId (runtimeUnit runtime))),
          taskAction = runBackendUnit context runtime
        }
    config = taskModuleCompileConfig context

-- | The three task indices a unit owns, above the indices of the modules.
resolveTaskId :: Int -> UnitId -> TaskId
resolveTaskId sourceCount (UnitId order) = TaskId (sourceCount + 3 * order)

typeTaskId :: Int -> UnitId -> TaskId
typeTaskId sourceCount (UnitId order) = TaskId (sourceCount + 3 * order + 1)

backendTaskId :: Int -> UnitId -> TaskId
backendTaskId sourceCount (UnitId order) = TaskId (sourceCount + 3 * order + 2)

unitLabel :: SourceUnit -> Text
unitLabel = T.intercalate "+" . map sourceName . sourceUnitSources

sourceName :: SourceModule -> Text
sourceName = sourceModuleName

-- | Take the parse trees of the modules of a unit, as the later phases take
-- them: each with the extension set that reading its source decided. The
-- resolve task of the unit calls this once.
takePackageModuleUnits :: Package -> [SourceModule] -> IO [ModuleUnit]
takePackageModuleUnits package sources = do
  parsed <- mapM (takeMVar . sourceModuleParsed) sources
  pure (modulesInPackage package (zip parsed (map sourceModuleExtensions sources)))

sourceDependencyNames :: SourceModule -> [Text]
sourceDependencyNames source =
  map snd (sourceModuleImports source)
    <> ["Prelude" | moduleUsesImplicitPrelude source]

moduleUsesImplicitPrelude :: SourceModule -> Bool
moduleUsesImplicitPrelude = elem ImplicitPrelude . sourceModuleExtensions

lookupRuntime :: Map.Map UnitId UnitRuntime -> UnitId -> UnitRuntime
lookupRuntime runtimes identifier =
  fromMaybe (error "missing unit runtime") (Map.lookup identifier runtimes)

readDependencyResults :: (UnitRuntime -> TMVar value) -> Map.Map UnitId UnitRuntime -> [UnitId] -> IO [value]
readDependencyResults select runtimes =
  mapM (atomically . readTMVar . select . lookupRuntime runtimes)

runResolveUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runResolveUnit context runtimes runtime = do
  dependencyResults <- readDependencyResults runtimeResolveResult runtimes (sourceUnitDependencies unit)
  let storePath = taskStorePath context
      resolvePackage = taskResolvePackage context
      root = taskPackageRoot context
      dependencyExports = taskDependencyExports context
      dependencyScopeHashes = taskDependencyScopeHashes context
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
      availableExports = Map.unions (map resolveUnitExports dependencyResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResults) `Map.union` dependencyScopeHashes
      scopeInputs = [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableScopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      inputs = sortOn fst (sourceHashes <> scopeInputs)
      resolvePath source = sourceModuleDirectory source </> "resolve.cbor"
      stampPath = storePath </> unitResolveStampPath unit
      parseSuccess = all (null . sourceModuleParseDiagnostics) sources
      dependenciesSucceeded = all resolveUnitSuccess dependencyResults
  -- This task owns the parse trees from here: they leave with the type
  -- input, and nothing else holds them.
  packageModules <- takePackageModuleUnits resolvePackage sources
  reused <-
    if parseSuccess && dependenciesSucceeded
      then reuseResolveUnit storePath stampPath inputs resolvePackage (map resolvePath sources)
      else pure Nothing
  (result, typeInput) <- case reused of
    Just (unitExports, scopeHashes) -> do
      verbose ("Reuse resolve context: " <> T.unpack (unitLabel unit))
      pure
        ( ResolveUnitResult
            { resolveUnitExports = unitExports,
              resolveUnitScopeHashes = scopeHashes,
              resolveUnitErrors = [],
              resolveUnitSuccess = True
            },
          TypeInputParsed packageModules
        )
    Nothing -> do
      let builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
          resolved = resolveWithDeps builtinScope availableExports packageModules
          errors = resolveErrors resolved
          unitExports = extractInterfaceWithDeps availableExports resolved
          success = parseSuccess && dependenciesSucceeded && null errors
      scopeHashes <-
        if success
          then do
            digests <- forM sources $ \source -> writeArtifact verbose unitExports resolvePackage (storePath </> resolvePath source) source
            files <- stampFiles storePath (map resolvePath sources)
            writeStamp stampPath ResolveStamp {resolveStampInputs = inputs, resolveStampScopes = Map.fromList digests, resolveStampFiles = files}
            pure (Map.fromList digests)
          else pure Map.empty
      pure
        ( ResolveUnitResult
            { resolveUnitExports = unitExports,
              resolveUnitScopeHashes = scopeHashes,
              resolveUnitErrors = errors,
              resolveUnitSuccess = success
            },
          TypeInputResolved resolved
        )
  atomically $ do
    putTMVar (runtimeResolveResult runtime) result
    putTMVar (runtimeTypeInput runtime) typeInput
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

-- | The exports and scope digests of a unit whose resolve artifacts were
-- built from the same inputs, if the artifacts are the ones the stamp
-- recorded.
reuseResolveUnit :: FilePath -> FilePath -> [(Text, Text)] -> Package -> [FilePath] -> IO (Maybe (ModuleExports, Map.Map Text Text))
reuseResolveUnit storePath stampPath inputs resolvePackage artifactPaths = do
  stamp <- readStamp stampPath
  case stamp of
    Just recorded | resolveStampInputs recorded == inputs -> do
      current <- filesMatchStamps storePath (resolveStampFiles recorded)
      if not current
        then pure Nothing
        else do
          decoded <- forM artifactPaths $ \path -> decodeResolveArtifact <$> BS.readFile (storePath </> path)
          case sequence decoded of
            Left _ -> pure Nothing
            Right artifacts ->
              pure
                ( Just
                    ( Map.fromList [(ModuleKey resolvePackage (resolveArtifactModuleName artifact), resolveArtifactScope artifact) | artifact <- artifacts],
                      resolveStampScopes recorded
                    )
                )
    _ -> pure Nothing

runTypeUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runTypeUnit context runtimes runtime = do
  resolvedOutput <- atomically (readTMVar (runtimeResolveResult runtime))
  -- The modules of the unit are this task's to check and then drop.
  typeInput <- atomically (takeTMVar (runtimeTypeInput runtime))
  dependencyResults <- readDependencyResults runtimeTypeResult runtimes (sourceUnitDependencies unit)
  dependencyResolveResults <- readDependencyResults runtimeResolveResult runtimes (sourceUnitDependencies unit)
  let storePath = taskStorePath context
      resolvePackage = taskResolvePackage context
      primIdentity = taskPrimIdentity context
      root = taskPackageRoot context
      dependencyExports = taskDependencyExports context
      dependencyScopeHashes = taskDependencyScopeHashes context
      dependencyTypes = taskDependencyTypes context
      dependencyTypeHashes = taskDependencyTypeHashes context
      dependencyInstanceFacts = taskDependencyInstanceFacts context
      dependencyInstanceProviders = taskDependencyInstanceProviders context
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
      availableTypes = LazyMap.unions (map typeUnitTypes dependencyResults) `LazyMap.union` dependencyTypes
      availableTypeHashes = LazyMap.unions (map typeUnitHashes dependencyResults) `LazyMap.union` dependencyTypeHashes
      availableExports = Map.unions (map resolveUnitExports dependencyResolveResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResolveResults) `Map.union` dependencyScopeHashes
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      scopeInputs =
        [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableScopeHashes]]
      typeInputs =
        [("type:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableTypeHashes]]
      -- The instances a unit sees come from every unit below it and from
      -- the dependency packages that supply an imported module. A facts
      -- digest covers the facts of the units below the one it names.
      factsInputs =
        [ ("facts:" <> unitLabel (runtimeUnit (lookupRuntime runtimes dependency)), typeUnitFactsDigest result)
        | (dependency, result) <- zip (sourceUnitDependencies unit) dependencyResults
        ]
      packageInputs =
        [ ("package:" <> name, digest)
        | (name, digest, modules) <- taskDependencyPackages context,
          any (`Set.member` modules) dependencyNames
        ]
      inputs =
        sortOn fst $
          sourceHashes
            <> scopeInputs
            <> typeInputs
            <> factsInputs
            <> packageInputs
            <> [ ("options:frontend", T.pack (frontendOptionsKey config)),
                 ("options:extensions", T.pack (show (map sourceModuleExtensions sources)))
               ]
      typePath source = sourceModuleDirectory source </> "type.cbor"
      factsPath = unitFactsPath unit
      stampPath = storePath </> unitStampPath unit
      frontendFiles = factsPath : map typePath sources
      externalInstanceProviders =
        Set.unions
          [ Map.findWithDefault Set.empty name dependencyInstanceProviders
          | name <- dependencyNames,
            name `notElem` unitNames
          ]
      externalInstanceInterface = selectInstanceProviders dependencyInstanceFacts externalInstanceProviders
      -- Each dependency carries the instance closure of its own dependencies,
      -- so the closures agree wherever they overlap.
      importedInstanceInterface =
        unionTcInterfaces
          (externalInstanceInterface : map typeUnitInstanceInterface dependencyResults)
      importedTypes =
        mergeTcInterfaces
          (configMergeCheck config)
          ( importedInstanceInterface
              : [ interface
                | name <- dependencyNames,
                  name `notElem` unitNames,
                  Just interface <- [Map.lookup name availableTypes]
                ]
          )
      checkUnit = do
        let resolved =
              case typeInput of
                TypeInputResolved result -> result
                TypeInputParsed packageModules ->
                  let builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
                   in resolveWithDeps builtinScope availableExports packageModules
            checked =
              typecheckModuleSccWithInterface
                (primTcConfig primIdentity)
                importedTypes
                (resolvedModules resolved)
            checkedDiagnostics = concatMap tcModuleDiagnostics (fst checked)
        _ <- evaluate (length checkedDiagnostics)
        pure (checked, checkedDiagnostics)
      dependencySuccess = all typeUnitSuccess dependencyResults
      resolveSuccess = resolveUnitSuccess resolvedOutput
  reused <-
    if resolveSuccess && dependencySuccess
      then reuseTypeUnit config storePath stampPath inputs
      else pure Nothing
  case reused of
    -- A unit only reaches the checker with a resolved tree and with the
    -- checked types of everything it imports. Resolve success already
    -- covers both: it is false for a unit whose own names did not resolve
    -- and for one that imports such a unit. Checking anyway would report
    -- knock-ons of errors the resolver already located, and would trip
    -- internal invariants ("resolver error reached type checker",
    -- "missing checked type constructor") that stay assertions for real
    -- compiler bugs. A unit whose dependency merely failed to type check
    -- is still checked: its types are published either way, and the unit
    -- has its own errors to report in this same run.
    Nothing
      | not resolveSuccess -> do
          verbose ("Skip type check after failed name resolution: " <> T.unpack (unitLabel unit))
          atomically $ do
            putTMVar
              (runtimeTypeResult runtime)
              TypeUnitResult
                { typeUnitTypes = Map.empty,
                  typeUnitHashes = Map.empty,
                  typeUnitOwnInstanceInterface = emptyTcInterface,
                  typeUnitFactsDigest = "",
                  typeUnitInstanceInterface = importedInstanceInterface,
                  typeUnitDiagnostics = [],
                  typeUnitWritten = Set.empty,
                  typeUnitReused = Set.empty,
                  typeUnitPendingStamp = Nothing,
                  typeUnitSuccess = False
                }
            putTMVar (runtimeBackendInput runtime) Nothing
    Just recorded -> do
      artifacts <- mapM (readTypeArtifactFile . (storePath </>) . typePath) sources
      decodedFacts <- typeArtifactInterface <$> readTypeArtifactFile (storePath </> factsPath)
      let ownFacts = decodedFacts
          interfaces = map typeArtifactInterface artifacts
      verbose ("Reuse type and backend artifacts: " <> T.unpack (unitLabel unit))
      atomically $ do
        putTMVar
          (runtimeTypeResult runtime)
          TypeUnitResult
            { typeUnitTypes = Map.fromList (zip unitNames interfaces),
              typeUnitHashes = unitStampTypes recorded,
              typeUnitOwnInstanceInterface = ownFacts,
              typeUnitFactsDigest = unitStampFacts recorded,
              typeUnitInstanceInterface = unionTcInterfaces [importedInstanceInterface, ownFacts],
              typeUnitDiagnostics = [],
              typeUnitWritten = Set.empty,
              typeUnitReused = Set.fromList unitNames,
              typeUnitPendingStamp = Nothing,
              typeUnitSuccess = True
            }
        putTMVar (runtimeBackendInput runtime) Nothing
    Nothing -> do
      ((checkedModules, checkedInterface), diagnostics) <- checkUnit
      let completeInterface = mergeTcInterfaces (configMergeCheck config) [importedTypes, checkedInterface]
          -- What the unit publishes outlives this task, so its equal
          -- parts are made one object each; the checking state is not.
          (ownInstanceInterface, unitTypes) =
            case shareTcInterfaces
              ( addReferencedFacts (typeLiteralKindTyCons (primKinds primIdentity)) (typeLiteralSupportTerms primIdentity) completeInterface (instanceFacts checkedInterface)
                  : map (moduleTypeInterface (primKinds primIdentity) (typeLiteralSupportTerms primIdentity) (resolveUnitExports resolvedOutput) resolvePackage completeInterface) sources
              ) of
              facts : rest -> (facts, rest)
              [] -> error "shareTcInterfaces dropped the unit facts"
          completeInstanceInterface = unionTcInterfaces [importedInstanceInterface, ownInstanceInterface]
          typeSuccess = not (any ((== TcError) . diagSeverity) diagnostics)
          success = resolveSuccess && dependencySuccess && typeSuccess
      (ownTypeHashes, factsDigest) <-
        if success
          then do
            typeHashes <- Map.fromList <$> zipWithM (writeTypeArtifact verbose ((storePath </>) . typePath)) sources unitTypes
            let factsBytes = encodeTypeArtifact (TypeArtifact "$unit" Map.empty ownInstanceInterface)
            createDirectoryIfMissing True (takeDirectory (storePath </> factsPath))
            BL.writeFile (storePath </> factsPath) factsBytes
            -- The facts digest covers the facts digests of the units below
            -- this one, so it changes with any of them.
            pure (typeHashes, T.pack (stableHash [BL.toStrict factsBytes, BS8.pack (show (sortOn fst (factsInputs <> packageInputs)))]))
          else pure (Map.empty, "")
      -- The unit goes all the way to System FC here, so the checked AST
      -- ends with this task: the backend takes the FC and nothing else.
      pendingBackend <-
        if compileNoCode config || not success
          then pure Nothing
          else do
            let desugarConfigs =
                  Map.fromList
                    [ (name, Fc.moduleDesugarConfig (primKinds primIdentity) primIdentity resolvePackage name (resolveUnitExports resolvedOutput))
                    | name <- unitNames
                    ]
            (fcModules, desugarNs) <-
              measureTime
                ( desugarCheckedModules
                    config
                    verbose
                    primIdentity
                    completeInterface
                    (moduleOutputPaths storePath (compileTarget config))
                    desugarConfigs
                    checkedModules
                )
            atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> mempty {backendDesugarNs = desugarNs}, ()))
            capiStubs <- evaluate (force [(name, renderCapiStub name (moduleCapiWrappers name completeInterface)) | name <- unitNames])
            pure (Just (PendingBackend fcModules capiStubs))
      let unitSet = Set.fromList unitNames
          -- A unit with warnings is not stamped, so the next build reports
          -- them again.
          pendingStamp
            | success && null diagnostics =
                Just
                  PendingStamp
                    { pendingStampPath = stampPath,
                      pendingStampInputs = inputs,
                      pendingStampTypes = ownTypeHashes,
                      pendingStampFacts = factsDigest,
                      pendingStampFrontendFiles = frontendFiles
                    }
            | otherwise = Nothing
      -- Force the type result before this type-check task ends.
      typeResult <-
        evaluate
          TypeUnitResult
            { typeUnitTypes = Map.fromList (zip unitNames unitTypes),
              typeUnitHashes = ownTypeHashes,
              typeUnitOwnInstanceInterface = ownInstanceInterface,
              typeUnitFactsDigest = factsDigest,
              typeUnitInstanceInterface = completeInstanceInterface,
              typeUnitDiagnostics = diagnostics,
              typeUnitWritten = unitSet,
              typeUnitReused = Set.empty,
              typeUnitPendingStamp = pendingStamp,
              typeUnitSuccess = success
            }
      when (compileNoCode config) $
        forM_ pendingStamp $
          \pending -> writeUnitStamp storePath pending Nothing
      atomically $ do
        putTMVar (runtimeTypeResult runtime) typeResult
        putTMVar (runtimeBackendInput runtime) pendingBackend
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

-- | The stamp of a unit whose type artifacts, and objects when code is
-- wanted, were built from the same inputs and are still the recorded files.
reuseTypeUnit :: ModuleCompileConfig -> FilePath -> FilePath -> [(Text, Text)] -> IO (Maybe UnitStamp)
reuseTypeUnit config storePath stampPath inputs = do
  stamp <- readStamp stampPath
  case stamp of
    Just recorded | unitStampInputs recorded == inputs -> do
      frontendCurrent <- filesMatchStamps storePath (unitStampFiles recorded)
      backendCurrent <-
        if compileNoCode config
          then pure True
          else case unitStampBackend recorded of
            Just backend
              | backendStampOptions backend == T.pack (backendOptionsKey config) ->
                  -- A capi wrapper is rebuilt when a header it included
                  -- changed, which nothing else in the build would notice.
                  (&&) <$> filesMatchStamps storePath (backendStampFiles backend) <*> filesMatchStamps "" (backendStampHeaders backend)
            _ -> pure False
      pure (if frontendCurrent && backendCurrent then Just recorded else Nothing)
    _ -> pure Nothing

writeUnitStamp :: FilePath -> PendingStamp -> Maybe (Text, [FilePath], [FileStamp]) -> IO ()
writeUnitStamp storePath pending backend = do
  files <- stampFiles storePath (pendingStampFrontendFiles pending)
  backendStamp <- forM backend $ \(options, paths, headers) -> BackendStamp options <$> stampFiles storePath paths <*> pure headers
  writeStamp
    (pendingStampPath pending)
    UnitStamp
      { unitStampInputs = pendingStampInputs pending,
        unitStampTypes = pendingStampTypes pending,
        unitStampFacts = pendingStampFacts pending,
        unitStampFiles = files,
        unitStampBackend = backendStamp
      }

runBackendUnit :: PackageTaskContext -> UnitRuntime -> IO ()
runBackendUnit context runtime = do
  started <- getMonotonicTimeNSec
  result <- atomically (readTMVar (runtimeTypeResult runtime))
  -- The FC of the unit is this task's: once it is compiled it is gone.
  pending <- atomically (takeTMVar (runtimeBackendInput runtime))
  case pending of
    Just backend | typeUnitSuccess result -> do
      let config = taskModuleCompileConfig context
          storePath = taskStorePath context
      (phaseTimings, capiOutputs) <-
        compileUnitFcModules
          config
          (taskCapiStubOptions context)
          (compileVerbose config)
          (moduleOutputPaths storePath (compileTarget config))
          backend
      capiHeaders <- stampFiles "" (sortOn id (nub (concatMap capiStubHeaders capiOutputs)))
      forM_ (typeUnitPendingStamp result) $ \stamp ->
        writeUnitStamp
          storePath
          stamp
          ( Just
              ( T.pack (backendOptionsKey config),
                unitBackendPaths config (runtimeUnit runtime) <> capiStubPaths (compileTarget config) capiOutputs,
                capiHeaders
              )
          )
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended phaseTimings, ()))
    _ -> do
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended mempty, ()))

unitStampDirectory :: FilePath
unitStampDirectory = ".units"

unitStampBase :: SourceUnit -> FilePath
unitStampBase unit = unitStampDirectory </> stableHash [TE.encodeUtf8 (unitLabel unit)]

unitFactsPath :: SourceUnit -> FilePath
unitFactsPath unit = unitStampBase unit <.> "cbor"

unitResolveStampPath :: SourceUnit -> FilePath
unitResolveStampPath unit = unitStampBase unit <.> "resolve.json"

unitStampPath :: SourceUnit -> FilePath
unitStampPath unit = unitStampBase unit <.> "unit.json"

-- | The backend outputs of a unit, relative to the package. A @--lto@ unit
-- writes the System FC of each module and nothing below it.
unitBackendPaths :: ModuleCompileConfig -> SourceUnit -> [FilePath]
unitBackendPaths config unit = concatMap paths (sourceUnitSources unit)
  where
    paths source
      | compileLto config = [outputFcPath (output source)]
      | otherwise =
          [outputObjectPath (output source)]
            <> [outputFcPath (output source) | compileKeepCore config]
            <> concat [[outputGrinPath (output source), outputCpsGrinPath (output source), outputGcGrinPath (output source)] | compileKeepGrin config]
            <> [outputLirPath (output source) | keepsLirText config]
            <> [outputNativePath (output source) | compileKeepNative config, not (keepNativeIsKeepLir config)]
    output source = moduleOutputPaths "" (compileTarget config) (sourceName source)

instanceFacts :: TcInterface -> TcInterface
instanceFacts interface =
  emptyTcInterface
    { tcInterfaceInstanceMap = tcInterfaceInstanceMap interface,
      tcInterfaceDataFamilyInstanceMap = tcInterfaceDataFamilyInstanceMap interface,
      tcInterfaceTypeFamilyInstanceMap = tcInterfaceTypeFamilyInstanceMap interface
    }

interfaceInstanceProviders :: TcInterface -> Set.Set InstanceProvider
interfaceInstanceProviders interface =
  Set.fromList
    ( map (first PackageId . iiDictOrigin) (tcInterfaceInstances interface)
        <> map (tyConOrigin . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstances interface)
        <> map tfiiOrigin (tcInterfaceTypeFamilyInstances interface)
    )
  where
    first transform (left, right) = (transform left, right)
    tyConOrigin tyCon = (tyConPackageId tyCon, tyConModuleName tyCon)

-- | The instance facts of a dependency, which already carries everything
-- its own modules refer to, so it needs no extra roots.
selectInstanceProviders :: TcInterface -> Set.Set InstanceProvider -> TcInterface
selectInstanceProviders complete providers
  | Set.null providers = emptyTcInterface
  | otherwise =
      addReferencedFacts
        []
        []
        complete
        emptyTcInterface
          { tcInterfaceInstanceMap = Map.filter ((`Set.member` providers) . first PackageId . iiDictOrigin) (tcInterfaceInstanceMap complete),
            tcInterfaceDataFamilyInstanceMap = Map.filter ((`Set.member` providers) . tyConOrigin . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstanceMap complete),
            tcInterfaceTypeFamilyInstanceMap = Map.filter ((`Set.member` providers) . tfiiOrigin) (tcInterfaceTypeFamilyInstanceMap complete)
          }
  where
    first transform (left, right) = (transform left, right)
    tyConOrigin tyCon = (tyConPackageId tyCon, tyConModuleName tyCon)

writePackageInstanceArtifact :: (String -> IO ()) -> FilePath -> Map.Map Text (Set.Set InstanceProvider) -> TcInterface -> IO Text
writePackageInstanceArtifact verbose storePath providers interface = do
  let path = storePath </> "instances.cbor"
      bytes = encodeTypeArtifact (TypeArtifact "$package-instances" (Map.map Set.toAscList providers) interface)
  createDirectoryIfMissing True storePath
  BL.writeFile path bytes
  verbose ("Write package instances: " <> path)
  pure (T.pack (stableHash [BL.toStrict bytes]))

wiredTypeModules :: [Text]
wiredTypeModules = ["GHC.CString", "GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String", "GHC.Tuple", "GHC.Types"]

-- | Modules whose names generated code refers to, but whose order the
-- dependency graph must not fix: a derived @Read@ instance calls the reader
-- of the primitive package and a derived @Lift@ the Template Haskell
-- builders, and a module that derives either does not import them. A
-- package that compiles one of these modules itself does so in its own
-- import order.
--
-- The deriving reference table is the list: a reference added there becomes
-- visible without an import, so the two cannot disagree. The identity of
-- the package does not matter here, because only the module names are
-- taken.
wiredDerivingModules :: [Text]
wiredDerivingModules =
  nub (map referenceModule (derivingReferenceList (primDerivingReferences (PackageId "aihc-prim"))))

-- | Every module whose type interface a compilation needs without an
-- import.
wiredInterfaceModules :: [Text]
wiredInterfaceModules = wiredTypeModules <> wiredDerivingModules

builtinFunctionScope :: Package -> ModuleExports -> [ModuleUnit] -> Scope
builtinFunctionScope currentPackage dependencyExports packageModules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    allExports = collectModuleExportsWithDeps dependencyExports packageModules `Map.union` dependencyExports
    lookupBuiltin name = lookupImportedModule currentPackage Nothing name allExports
    builtinFunctionModules = ["GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String"]

measureTime :: IO a -> IO (a, Word64)
measureTime action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  pure (value, end - start)

withOtherTime :: Word64 -> Word64 -> BackendPhaseTimings -> BackendPhaseTimings
withOtherTime started ended timings =
  timings
    { backendOtherNs = extra
    }
  where
    accounted = backendDesugarNs timings + backendGrinNs timings + backendNativeNs timings
    elapsed = ended - started
    extra
      | elapsed > accounted = elapsed - accounted
      | otherwise = 0

renderBackendPhaseTotals :: BackendPhaseTimings -> String
renderBackendPhaseTotals timings =
  unlines
    [ "desugar total: " <> renderDuration (backendDesugarNs timings),
      "grin total: " <> renderDuration (backendGrinNs timings),
      "native total: " <> renderDuration (backendNativeNs timings),
      "other total: " <> renderDuration (backendOtherNs timings)
    ]

-- | Whether the interface merges of a compile verify the sides against
-- each other. The check costs a comparison of every fact two merged
-- interfaces share, so it runs under @--lint@ and nowhere else.
configMergeCheck :: ModuleCompileConfig -> MergeCheck
configMergeCheck config
  | compileLint config = CheckMergedFacts
  | otherwise = TrustMergedFacts

-- | Desugar the checked modules of a unit to System FC, lint it when asked,
-- and write it when a later build or a @--lto@ link reads it. This is the
-- last phase that sees the Haskell AST.
desugarCheckedModules :: ModuleCompileConfig -> (String -> IO ()) -> PackageId -> TcInterface -> (Text -> ModuleOutputPaths) -> Map.Map Text DesugarConfig -> [Module] -> IO [FcModule]
desugarCheckedModules config verbose primIdentity interface outputPaths desugarConfigs checkedModules = do
  let moduleNames = map (fromMaybe "Main" . moduleName) checkedModules
  do
    let kinds = primKinds primIdentity
        -- A module the resolver did not report on keeps every name public.
        desugarConfig name =
          Map.findWithDefault (Fc.allPublicDesugarConfig kinds primIdentity) name desugarConfigs
        -- Each module is desugared against its own bindings; the rest of
        -- the unit reaches it through the interface.
        desugarResults =
          [ Fc.desugarModuleFc (desugarConfig name) (tcModuleBindings (primTcWiring primIdentity) checked) interface checked
          | (name, checked) <- zip moduleNames checkedModules
          ]
        desugarErrors =
          [ T.unpack name <> ": " <> err
          | (name, result) <- zip moduleNames desugarResults,
            err <- dsErrors result
          ]
    unless (all dsSuccess desugarResults) (ioError (userError ("FC generation failed: " <> unlines desugarErrors)))
    -- The FC waits in memory for the backend, so equal names and types
    -- are made one object each before it is kept.
    let fcModules = zipWith FcModule moduleNames (map (Fc.shareProgram . dsProgram) desugarResults)
    fcErrors <-
      fmap concat $
        forM fcModules $ \fcModule -> do
          when lint (verbose ("Lint FC: " <> T.unpack (fcModuleName fcModule)))
          let errors = [(fcModuleName fcModule, err) | err <- Fc.lintProgram (fcProgram fcModule)]
          when lint (void (evaluate (length errors)))
          pure errors
    let fcReport = ["    " <> T.unpack name <> ": " <> show err | (name, err) <- fcErrors]
    when lint $
      unless (null fcErrors) $
        ioError
          ( userError
              ( unlines
                  ( ["FC lint failed:"]
                      <> fcReport
                  )
              )
          )
    -- A @--lto@ build keeps the System FC of every module: it is what the
    -- executable compiles.
    when (keepCore || lto) (mapM_ writeFcModule fcModules)
    -- The FC is forced here so that no thunk into the checked AST leaves
    -- with it.
    evaluate (force fcModules)
  where
    keepCore = compileKeepCore config
    lint = compileLint config
    lto = compileLto config

    writeFcModule fcModule = do
      let name = fcModuleName fcModule
          path = outputFcPath (outputPaths name)
      writeFcFile path (fcProgram fcModule)
      verbose ("Write FC: " <> T.unpack name)

    writeFcFile path program = do
      let rendered = Fc.renderProgram program
          output = if "\n" `T.isSuffixOf` rendered then rendered else rendered <> "\n"
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path output

-- | Compile the System FC of a unit to objects, and its capi wrappers
-- beside them. Only the FC and the rendered wrappers come in: the frontend
-- state of the unit is gone.
compileUnitFcModules :: ModuleCompileConfig -> CapiStubOptions -> (String -> IO ()) -> (Text -> ModuleOutputPaths) -> PendingBackend -> IO (BackendPhaseTimings, [CapiStubOutput])
compileUnitFcModules config capiOptions verbose outputPaths pending = do
  (grinNs, nativeNs) <-
    if lto
      then pure (0, 0)
      else do
        -- Each module is inlined on its own: the program is not known here.
        optimized <- forM (pendingFcModules pending) $ \fcModule -> do
          program <- optimizeFcProgram config verbose Nothing (fcModuleName fcModule) (fcProgram fcModule)
          pure fcModule {fcProgram = program}
        compileFcModules config verbose outputPaths optimized
  -- The wrappers are part of the native phase: they are the last objects the
  -- backend writes for a unit.
  (capiOutputs, capiNs) <- measureTime (concat <$> mapM (uncurry buildCapiStub) (pendingCapiStubs pending))
  pure
    ( BackendPhaseTimings
        { backendDesugarNs = 0,
          backendGrinNs = grinNs,
          backendNativeNs = nativeNs + capiNs,
          backendOtherNs = 0
        },
      capiOutputs
    )
  where
    lto = compileLto config
    target = compileTarget config

    -- The C wrappers of a module's capi imports are compiled beside its
    -- object and archived with it, so a module that declares none must leave
    -- no wrapper object behind for the archive to pick up.
    buildCapiStub name stub = do
      let paths = outputPaths name
      case stub of
        Nothing -> do
          mapM_ removeFileIfPresent [outputCapiSourcePath paths, outputCapiObjectPath paths, outputCapiDependencyPath paths]
          pure []
        Just source -> do
          createDirectoryIfMissing True (takeDirectory (outputCapiSourcePath paths))
          TIO.writeFile (outputCapiSourcePath paths) source
          arguments <- capiStubArguments target (compileOptimization config) capiOptions (compileHeaderDirectory config)
          verbose ("Compile capi wrappers: " <> T.unpack name)
          (compiler, _) <- backendCompiler target
          runTool
            compiler
            ( arguments
                <> ["-MD", "-MF", outputCapiDependencyPath paths]
                <> ["-c", outputCapiSourcePath paths, "-o", outputCapiObjectPath paths]
            )
          recorded <- readFile (outputCapiDependencyPath paths)
          source' <- canonicalizePath (outputCapiSourcePath paths)
          -- The stub itself is a prerequisite of its own object, and it is
          -- already recorded as an output of this unit.
          headers <- filter (/= source') <$> mapM canonicalizePath (parseDependencyFile recorded)
          pure [CapiStubOutput name (sortOn id (nub headers))]

-- | Run the System FC passes of the plan on a program, in order. The
-- roots are the values the program must keep, or 'Nothing' to keep every
-- public value. Each pass is logged under @--verbose@ and the program is
-- linted after each under @--lint@.
--
-- The passes come from the plan of the level; nothing here reads the
-- level. See @docs/optimization.md@.
optimizeFcProgram :: ModuleCompileConfig -> (String -> IO ()) -> Maybe [Fc.Name] -> Text -> Fc.Program -> IO Fc.Program
optimizeFcProgram config verbose roots name = foldM step `flip` compilePasses config
  where
    step program pass = do
      let (program', report) = Fc.runPass roots pass program
      verbose (renderPassReport name report)
      lintOptimized config (T.unpack (Fc.reportPass report)) name program'
      pure program'

-- | One log line for a pass: its name, the program, the sizes before and
-- after, and what else it counted.
renderPassReport :: Text -> Fc.PassReport -> String
renderPassReport name report =
  T.unpack (Fc.reportPass report)
    <> " FC: "
    <> T.unpack name
    <> ", size "
    <> show (Fc.reportBefore report)
    <> " -> "
    <> show (Fc.reportAfter report)
    <> (if T.null (Fc.reportDetail report) then "" else ", " <> T.unpack (Fc.reportDetail report))

lintOptimized :: ModuleCompileConfig -> String -> Text -> Fc.Program -> IO ()
lintOptimized config phase name program =
  when (compileLint config) $ do
    let errors = Fc.lintProgram program
    unless (null errors) (ioError (userError ("FC lint failed after " <> phase <> " " <> T.unpack name <> ":\n" <> unlines (map (("    " <>) . show) errors))))

-- | Lower System FC modules to objects: GRIN, then Lir, then the object of
-- the target. A module with no declarations gets an empty object. Returns
-- the time the GRIN phase and the native phase took.
compileFcModules :: ModuleCompileConfig -> (String -> IO ()) -> (Text -> ModuleOutputPaths) -> [FcModule] -> IO (Word64, Word64)
compileFcModules config verbose outputPaths = foldM compileOne (0, 0)
  where
    keepGrin = compileKeepGrin config
    keepNative = compileKeepNative config
    -- The Lir text is written for @--keep-lir@, and on a target whose
    -- native source is that same text for @--keep-native@ as well.
    keepLir = keepsLirText config
    target = compileTarget config
    compileOne (grinTotal, nativeTotal) fcModule = do
      (grinNs, nativeNs) <-
        if null (Fc.programDecls (fcProgram fcModule))
          then do
            (_, elapsed) <- measureTime (writeEmptyModule fcModule)
            pure (0, elapsed)
          else do
            (gcProgram, grinElapsed) <- measureTime (lowerGrinModule fcModule)
            (_, nativeElapsed) <- measureTime (writeModule (fcModuleName fcModule) gcProgram)
            pure (grinElapsed, nativeElapsed)
      let nextGrin = grinTotal + grinNs
          nextNative = nativeTotal + nativeNs
      nextGrin `seq` nextNative `seq` pure (nextGrin, nextNative)

    writeModule name gcProgram = do
      let paths = outputPaths name
      createDirectoryIfMissing True (takeDirectory (outputObjectPath paths))
      source <- compileGrinTo (compileLint config) (compileCheckPrimBounds config) target (if keepLir then Just (outputLirPath paths) else Nothing) gcProgram (outputObjectPath paths)
      when keepLir (verbose ("Write Lir: " <> T.unpack name))
      mapM_ (TIO.writeFile (outputNativePath paths)) source
      when (isJust source) (verbose ("Write native source: " <> T.unpack name))
      when (isJust source) $ do
        (compiler, arguments) <- backendCompiler target
        let levelArguments = [optimizationArgument (compileOptimization config) | target == Llvm]
        runTool compiler (arguments <> levelArguments <> ["-c", outputNativePath paths, "-o", outputObjectPath paths])
        unless keepNative (removeFile (outputNativePath paths))
      verbose ("Write object: " <> T.unpack name)

    writeEmptyModule fcModule = do
      let name = fcModuleName fcModule
          paths = outputPaths name
      createDirectoryIfMissing True (takeDirectory (outputObjectPath paths))
      BS.writeFile (outputObjectPath paths) ""
      when (compileKeepGrin config) $ do
        writeFile (outputGrinPath paths) ""
        writeFile (outputCpsGrinPath paths) ""
        writeFile (outputGcGrinPath paths) ""
      when keepLir (writeFile (outputLirPath paths) "")
      when keepNative (writeFile (outputNativePath paths) "")
      verbose ("Write empty object: " <> T.unpack name)

    lowerGrinModule fcModule = do
      let name = fcModuleName fcModule
          paths = outputPaths name
      verbose ("Lower GRIN: " <> T.unpack (fcModuleName fcModule))
      plainProgram <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram (fcProgram fcModule))
      when (compileLint config) $ do
        let plainErrors = Grin.lintProgram plainProgram
        unless (null plainErrors) (ioError (userError ("GRIN lint failed in " <> T.unpack (fcModuleName fcModule) <> ": " <> show plainErrors)))
      when keepGrin $ do
        writeGrinFile (outputGrinPath paths) plainProgram
        verbose ("Write GRIN: " <> T.unpack name)
      cpsProgram <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin plainProgram)
      when keepGrin $ do
        writeGrinFile (outputCpsGrinPath paths) (Grin.cpsGrinProgram cpsProgram)
        verbose ("Write CPS-GRIN: " <> T.unpack name)
      let gcProgram = Grin.lowerGc cpsProgram
      when (compileLint config) $ do
        let gcErrors = Grin.lintGcProgram gcProgram
        unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed in " <> T.unpack (fcModuleName fcModule) <> ": " <> show gcErrors)))
      when keepGrin $ do
        writeGrinFile (outputGcGrinPath paths) (Grin.gcGrinProgram gcProgram)
        verbose ("Write GC-GRIN: " <> T.unpack name)
      pure gcProgram

    writeGrinFile path program = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (withFinalNewline (renderString (layoutPretty defaultLayoutOptions (Grin.prettyProgram program))))

moduleOutputPaths :: FilePath -> NativeTarget -> Text -> ModuleOutputPaths
moduleOutputPaths storePath target name =
  ModuleOutputPaths
    { outputFcPath = directory </> "core",
      outputGrinPath = directory </> "grin",
      outputCpsGrinPath = directory </> "cps.grin",
      outputGcGrinPath = directory </> "gc.grin",
      outputLirPath = objectPath <> ".lir",
      outputNativePath = objectPath <> nativeSourceExtension target,
      outputObjectPath = objectPath,
      outputCapiSourcePath = capiPath <> ".c",
      outputCapiObjectPath = capiPath <> ".o",
      outputCapiDependencyPath = capiPath <> ".d"
    }
  where
    directory = storePath </> moduleNameDirectory name
    objectPath = directory </> T.unpack name <> ".o"
    capiPath = directory </> T.unpack name <> ".capi"

withFinalNewline :: String -> String
withFinalNewline rendered
  | "\n" `isSuffixOf` rendered = rendered
  | otherwise = rendered <> "\n"

-- | What a module's capi wrappers were compiled from and what they read.
--
-- The headers come from the dependency file the compile wrote, so a wrapper
-- is rebuilt when a header it included changes, even though nothing else in
-- the compiler ever read that header.
data CapiStubOutput = CapiStubOutput
  { capiStubModule :: !Text,
    capiStubHeaders :: ![FilePath]
  }
  deriving (Eq, Show)

-- | The include directories and options of a package, which its capi wrappers
-- are compiled with just as its own C sources are.
capiStubOptions :: [HackageCabal.FileInfo] -> HackageCabal.CCompileInfo -> CapiStubOptions
capiStubOptions files info =
  CapiStubOptions
    { capiStubIncludeDirs = nub (concatMap HackageCabal.fileInfoIncludeDirs files <> HackageCabal.cCompileIncludeDirs info),
      capiStubCcOptions = HackageCabal.cCompileCcOptions info
    }

-- | What the capi wrappers of a unit add to its recorded backend outputs.
capiStubPaths :: NativeTarget -> [CapiStubOutput] -> [FilePath]
capiStubPaths target outputs =
  [ path
  | output <- outputs,
    let paths = moduleOutputPaths "" target (capiStubModule output),
    path <- [outputCapiSourcePath paths, outputCapiObjectPath paths, outputCapiDependencyPath paths]
  ]

removeFileIfPresent :: FilePath -> IO ()
removeFileIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)

compilePackageCFiles :: NativeTarget -> OptimizationLevel -> FilePath -> (String -> IO ()) -> FilePath -> FilePath -> HackageCabal.CCompileInfo -> IO [FilePath]
compilePackageCFiles target level headerDirectory verbose packageRoot storePath info
  | null (HackageCabal.cCompileSources info) = pure []
  | otherwise = do
      (compiler, targetArguments) <- backendCompiler target
      sysrootIncludes <- wasmSysrootIncludeArguments target
      let includeArguments =
            sysrootIncludes
              <> ["-I" <> directory | directory <- HackageCabal.cCompileIncludeDirs info]
              <> ["-I" <> headerDirectory]
          objectRoot = storePath </> "cbits"
      createDirectoryIfMissing True objectRoot
      forM (HackageCabal.cCompileSources info) $ \source -> do
        exists <- doesFileExist source
        unless exists (ioError (userError ("C source is absent: " <> source)))
        let object = objectRoot </> cObjectFileName (makeRelative packageRoot source)
        verbose ("Compile C source: " <> source)
        runTool
          compiler
          ( targetArguments
              <> handwrittenCArguments level
              <> HackageCabal.cCompileCcOptions info
              <> includeArguments
              <> ["-c", source, "-o", object]
          )
        pure object

-- | Run the configure script of a @build-type: Configure@ package and return
-- the sources and C inputs with its outputs in their include paths.
--
-- Cabal runs the script in the package directory, so the generated headers
-- land beside their templates. Here the source tree is shared by every
-- target -- a Hackage release is unpacked once into the cache -- while the
-- answers configure finds are per target, so the script runs out of tree
-- from a directory under the package's own output path. Autoconf supports
-- this: the outputs of @AC_CONFIG_HEADERS@ and @AC_CONFIG_FILES@ are written
-- relative to the working directory and @srcdir@ is derived from the script
-- path. Every include directory of the package then gets a counterpart under
-- the configure directory that is searched first, which is how the generated
-- headers reach both the CPP pass over the Haskell sources and the C
-- compiles. A @<package>.buildinfo@ the script writes is merged the way
-- Cabal merges it.
--
-- The script sees the C compiler of the target, so its feature tests answer
-- for the target rather than the host.
configurePackage :: ModuleCompileConfig -> FilePath -> FilePath -> Text -> PackageInputs -> IO ([HackageCabal.FileInfo], HackageCabal.CCompileInfo)
configurePackage config root storePath packageName inputs =
  case inputConfigureScript inputs of
    Nothing -> pure (files, cInfo)
    Just script -> do
      let buildDirectory = storePath </> "configure"
          stampPath = buildDirectory </> "configure.hash"
      (executable, arguments, environment) <- configureCommand (compileTarget config) (compileOptimization config) script
      inputsHash <- configureInputsHash config script
      previous <- readStampText stampPath
      if previous == Just inputsHash
        then verbose ("Reuse configure: " <> buildDirectory)
        else do
          exists <- doesDirectoryExist buildDirectory
          when exists (removeDirectoryRecursive buildDirectory)
          createDirectoryIfMissing True buildDirectory
          verbose ("Configure: " <> unwords (executable : arguments))
          runToolIn buildDirectory environment executable arguments
          BS8.writeFile stampPath (BS8.pack inputsHash)
      let packageIncludeDirs = nub (concatMap HackageCabal.fileInfoIncludeDirs files <> HackageCabal.cCompileIncludeDirs cInfo)
          -- An include directory outside the package has no generated
          -- counterpart.
          counterparts =
            [ buildDirectory </> relative
            | directory <- packageIncludeDirs,
              let relative = makeRelative root directory,
              isRelative relative
            ]
      generatedDirs <- filterM doesDirectoryExist counterparts
      hooked <- readHookedBuildInfo buildDirectory packageName
      let (files', cInfo') =
            HackageCabal.applyHookedBuildInfo
              buildDirectory
              hooked
              (map (HackageCabal.prependIncludeDirs generatedDirs) files)
              cInfo {HackageCabal.cCompileIncludeDirs = nub (generatedDirs <> HackageCabal.cCompileIncludeDirs cInfo)}
          searchDirs = nub (concatMap HackageCabal.fileInfoIncludeDirs files' <> HackageCabal.cCompileIncludeDirs cInfo')
      forM_ (inputAutogenIncludes inputs) $ \header -> do
        found <- filterM (\directory -> doesFileExist (directory </> header)) searchDirs
        when (null found) $
          ioError
            ( userError
                ( "The configure script of "
                    <> T.unpack packageName
                    <> " did not write the autogen-includes header "
                    <> header
                    <> " under "
                    <> buildDirectory
                )
            )
      pure (files', cInfo')
  where
    files = inputSources inputs
    cInfo = inputCCompileInfo inputs
    verbose = compileVerbose config

-- | The command that runs a configure script for a target: the shell, since
-- an unpacked release does not keep the executable bit; the script and its
-- arguments; and an environment naming the C compiler of the target.
--
-- @CC@ and @CFLAGS@ are what the C sources of the package are later compiled
-- with, so a feature test and the code that acts on its answer see the same
-- compiler, target and sysroot.
--
-- Autoconf and aihc use the word host for opposite machines. Autoconf's
-- build machine is where the compiler runs, which aihc calls the host; its
-- host machine is where the compiled code runs, which aihc calls the target.
-- So the aihc target is passed as @--host@, and only when it is not the aihc
-- host: that tells the script it cannot run the programs it compiles. When
-- the two coincide nothing is passed, as Cabal passes nothing: the build
-- machine is guessed by the script, and a named host that differs from that
-- guess, even only by a version suffix, counts as cross-compiling too.
configureCommand :: NativeTarget -> OptimizationLevel -> FilePath -> IO (FilePath, [String], [(String, String)])
configureCommand target level script = do
  (compiler, cflagList) <- targetCCompiler target level
  inherited <- getEnvironment
  let cflags = unwords cflagList
      overrides = [("CC", compiler), ("CFLAGS", cflags)]
      environment = overrides <> [entry | entry@(name, _) <- inherited, name `notElem` map fst overrides]
      crossArguments = ["--host=" <> name | Just target /= hostNativeTarget, Just name <- [autoconfHostName target]]
  pure ("sh", script : crossArguments, environment)

-- | The C compiler of a target and the flags handwritten C is compiled
-- with: the target arguments, the level, and the sysroot includes. A tool
-- that compiles C on the package's behalf, such as a configure script or
-- hsc2hs, gets these so that what it learns about the target holds for the
-- code that is later compiled for it.
targetCCompiler :: NativeTarget -> OptimizationLevel -> IO (FilePath, [String])
targetCCompiler target level = do
  (compiler, targetArguments) <- backendCompiler target
  sysrootIncludes <- wasmSysrootIncludeArguments target
  pure (compiler, targetArguments <> handwrittenCArguments level <> sysrootIncludes)

-- | Turn the sources a preprocessor owns into Haskell modules, and return
-- the source list with those files pointing at the generated modules.
--
-- The generated files live under @<storePath>/preprocess@, mirroring the
-- package layout. That directory is per target, as it must be: hsc2hs
-- answers with the sizes and constants of the target, so the same @.hsc@
-- file yields a different module per target. The package's own tree is
-- shared across targets and stays untouched.
--
-- Each output carries a stamp of everything it was made from, and an
-- unchanged stamp skips the tool. The configure hash is part of it because
-- a @.hsc@ file includes the headers configure wrote, and the macro header
-- is part of it because a @.hsc@ file branches on the versions it reports.
--
-- Beside each output goes that file's @cabal_macros.h@: the preprocessor
-- resolves the file's @#if@ lines with a C compiler, which knows nothing of
-- the macros aihc's own CPP pass prepends to a Haskell source. The header is
-- per file because @cpp-options@ and @build-depends@ are per component.
preprocessPackage :: ModuleCompileConfig -> DependencyVersions -> FilePath -> FilePath -> Maybe FilePath -> HackageCabal.CCompileInfo -> [HackageCabal.FileInfo] -> IO [HackageCabal.FileInfo]
preprocessPackage config versions root storePath configureScript cInfo = mapM preprocessFile
  where
    verbose = compileVerbose config

    preprocessFile file =
      case HackageCabal.fileInfoPreprocessor file of
        Nothing -> pure file
        Just preprocessor -> do
          let input = HackageCabal.fileInfoPath file
              stem = storePath </> "preprocess" </> dropExtension (makeRelative root input)
              output = stem <.> "hs"
              macrosPath = stem <.> "macros.h"
              stampPath = output <.> "hash"
              macros = cabalMacrosHeader (HackageCabal.fileInfoCppOptions file) versions (HackageCabal.fileInfoDependencies file)
          (executable, arguments) <- preprocessorCommand config preprocessor cInfo file output macrosPath
          toolIdentity <- preprocessorIdentity executable
          inputBytes <- BS.readFile input
          configureHash <- maybe (pure "") (configureInputsHash config) configureScript
          environmentIdentity <- buildEnvironmentIdentity (compileTarget config)
          let inputsHash =
                stableHash
                  [ TE.encodeUtf8 packageArtifactFormatVersion,
                    inputBytes,
                    TE.encodeUtf8 macros,
                    BS8.pack toolIdentity,
                    BS8.pack (show (executable, arguments)),
                    BS8.pack configureHash,
                    BS8.pack environmentIdentity
                  ]
          previous <- readStampText stampPath
          exists <- doesFileExist output
          if exists && previous == Just inputsHash
            then verbose ("Reuse preprocessed: " <> output)
            else do
              createDirectoryIfMissing True (takeDirectory output)
              TIO.writeFile macrosPath macros
              verbose ("Preprocess: " <> unwords (executable : arguments))
              -- The tool keeps its scratch files next to the output, and
              -- an @#include "..."@ in the source resolves against the
              -- source's own directory through the -I passed above.
              inherited <- getEnvironment
              runToolIn (takeDirectory output) inherited executable arguments
              BS8.writeFile stampPath (BS8.pack inputsHash)
          pure file {HackageCabal.fileInfoPath = output, HackageCabal.fileInfoPreprocessor = Nothing}

-- | The executable and arguments that run a preprocessor over one file,
-- given the path of that file's @cabal_macros.h@.
preprocessorCommand :: ModuleCompileConfig -> Preprocessor -> HackageCabal.CCompileInfo -> HackageCabal.FileInfo -> FilePath -> FilePath -> IO (FilePath, [String])
preprocessorCommand config preprocessor cInfo file output macrosPath = do
  executable <- preprocessorExecutable preprocessor
  arguments <-
    case preprocessor of
      Hsc2hs -> hsc2hsArguments config cInfo file output macrosPath
  pure (executable, arguments)

-- | The arguments Cabal would give hsc2hs, with one difference: aihc always
-- asks for cross-compilation mode. In that mode hsc2hs finds every constant
-- by compiling test programs with the C compiler of the target and never
-- runs one, so the same code path serves the host, a foreign machine and
-- wasm, and the result cannot depend on which of them aihc happens to run
-- on.
--
-- Cross-compilation mode is paired with @--via-asm@, which reads the
-- constants back out of the assembly of a single compilation per file.
-- Without it hsc2hs binary-searches for each constant separately, which
-- costs dozens of C compiler runs per constant: a package the size of
-- @unix@ spends minutes there instead of seconds.
--
-- The C compiler is the target's, with the flags handwritten C is compiled
-- with, plus the package's @cc-options@ and @cpp-options@ and its include
-- directories, which by now include the ones configure wrote. The template
-- hsc2hs wraps the file in includes @HsFFI.h@, so the runtime's include
-- directory is searched too. The @*_HOST_OS@ and @*_HOST_ARCH@ macros are
-- defined the way Cabal defines them, and the @cabal_macros.h@ of the file
-- is force-included the way Cabal includes it, since a @.hsc@ file resolves
-- its own @#if@ lines through the C compiler rather than through aihc's CPP
-- pass: without the header a @MIN_VERSION_*@ guard is not merely wrong but
-- a C error, an undefined function-like macro.
hsc2hsArguments :: ModuleCompileConfig -> HackageCabal.CCompileInfo -> HackageCabal.FileInfo -> FilePath -> FilePath -> IO [String]
hsc2hsArguments config cInfo file output macrosPath = do
  let target = compileTarget config
      input = HackageCabal.fileInfoPath file
  (compiler, cflags) <- targetCCompiler target (compileOptimization config)
  let includeDirs = nub (takeDirectory input : HackageCabal.fileInfoIncludeDirs file <> HackageCabal.cCompileIncludeDirs cInfo <> [compileHeaderDirectory config])
      options = HackageCabal.cCompileCcOptions cInfo <> HackageCabal.fileInfoCppOptions file
  pure
    ( ["--cross-compile", "--via-asm", "--cc=" <> compiler, "--ld=" <> compiler]
        <> map ("--cflag=" <>) (cflags <> options <> hostPlatformMacros target <> ["-include", macrosPath])
        <> map ("-I" <>) includeDirs
        <> ["-o", output, input]
    )

-- | Where a preprocessor's executable is: the environment variable named
-- for it, or else the search path.
preprocessorExecutable :: Preprocessor -> IO FilePath
preprocessorExecutable preprocessor = do
  let name = preprocessorToolName preprocessor
      variable = preprocessorEnvironmentVariable preprocessor
  override <- lookupEnv variable
  case override of
    Just path | not (null path) -> pure path
    _ -> do
      found <- findExecutable name
      case found of
        Just path -> pure path
        Nothing ->
          ioError
            ( userError
                ( "The package has a source that needs "
                    <> name
                    <> ", which is not on the PATH. Install it, or name it with "
                    <> variable
                    <> "."
                )
            )

-- | What identifies a preprocessor for the stamps of its outputs: where it
-- is and what it says its version is.
preprocessorIdentity :: FilePath -> IO String
preprocessorIdentity executable = do
  path <- canonicalizePath executable
  version <- readProcess executable ["--version"] ""
  pure (stableHash [BS8.pack path, BS8.pack version])

-- | The name autoconf gives the machine an aihc target's code runs on, in
-- autoconf's vocabulary the host, for the @--host@ argument of a configure
-- script. The names are the canonical ones config.sub produces, which is not
-- always the Clang triple: Clang says @arm64@ where autoconf says @aarch64@.
autoconfHostName :: NativeTarget -> Maybe String
autoconfHostName target =
  case target of
    AppleArm64 -> Just "aarch64-apple-darwin"
    LinuxAmd64 -> Just "x86_64-unknown-linux-gnu"
    Wasm32Wasip3 -> Just "wasm32-unknown-wasi"
    -- The LLVM target is whatever machine aihc runs on, so it has no name
    -- of its own and is never a cross target.
    Llvm -> Nothing

-- | What the outputs of a configure run depend on: the script, the compiler
-- and arguments it sees, and the target.
configureInputsHash :: ModuleCompileConfig -> FilePath -> IO String
configureInputsHash config script = do
  let target = compileTarget config
  scriptBytes <- BS.readFile script
  environmentIdentity <- buildEnvironmentIdentity target
  (executable, arguments, environment) <- configureCommand target (compileOptimization config) script
  pure
    ( stableHash
        [ TE.encodeUtf8 packageArtifactFormatVersion,
          scriptBytes,
          BS8.pack environmentIdentity,
          BS8.pack (show (executable, arguments, lookup "CC" environment, lookup "CFLAGS" environment))
        ]
    )

-- | The @<package>.buildinfo@ a configure script wrote, if it wrote one.
readHookedBuildInfo :: FilePath -> Text -> IO HookedBuildInfo
readHookedBuildInfo buildDirectory packageName = do
  let path = buildDirectory </> T.unpack packageName <.> "buildinfo"
  exists <- doesFileExist path
  if not exists
    then pure emptyHookedBuildInfo
    else do
      bytes <- BS.readFile path
      case runParseResult (parseHookedBuildInfo bytes) of
        (_, Right value) -> pure value
        (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> path <> ": " <> show errors))

wasmSysrootIncludeArguments :: NativeTarget -> IO [String]
wasmSysrootIncludeArguments target =
  case target of
    Wasm32Wasip3 -> do
      sysroot <- wasmSysroot
      pure ["-isystem" <> wasmSysrootInclude sysroot]
    _ -> pure []

cObjectFileName :: FilePath -> FilePath
cObjectFileName source =
  map replaceSeparator (dropExtension source) <.> "o"
  where
    replaceSeparator character =
      if character == '/' || character == '\\'
        then '_'
        else character

buildLibraryArchive :: NativeTarget -> (String -> IO ()) -> FilePath -> [FilePath] -> IO ()
buildLibraryArchive target verbose archive moduleObjects = do
  createDirectoryIfMissing True (takeDirectory archive)
  archiveExists <- doesFileExist archive
  when archiveExists (removeFile archive)
  archiver <- backendArchiver target
  nonemptyObjects <- filterM (fmap (> 0) . getFileSize) moduleObjects
  -- BSD ar refuses to create an archive with no members, and a package whose
  -- modules are all empty standins (aihc-internal) has none. Every archive
  -- format begins with the same global header, and an archive that stops
  -- there is a valid empty archive for ld64, GNU ld, lld and wasm-ld alike.
  if null nonemptyObjects
    then BS.writeFile archive emptyArchive
    else do
      environment <- getEnvironment
      -- Set archive timestamps only in the child process environment.
      let archiveEnvironment = ("ZERO_AR_DATE", "1") : filter ((/= "ZERO_AR_DATE") . fst) environment
      runToolWithEnvironment (Just archiveEnvironment) archiver (["rcs", archive] <> nonemptyObjects)
  verbose ("Write archive: " <> archive)

-- | The global header every archive format begins with. An archive that
-- stops here holds no member.
emptyArchive :: BS.ByteString
emptyArchive = BS8.pack "!<arch>\n"

-- | Whether the archive holds a member. An archive of the header alone is a
-- valid empty archive for GNU ld, lld and wasm-ld, but the ld64 of the
-- cctools binutils rejects it as a file too small to read, so the link
-- leaves such an archive out rather than passing it to the linker.
archiveHasMembers :: FilePath -> IO Bool
archiveHasMembers archive = do
  size <- getFileSize archive
  pure (size > fromIntegral (BS.length emptyArchive))

runTool :: FilePath -> [String] -> IO ()
runTool = runToolWithEnvironment Nothing

runToolWithEnvironment :: Maybe [(String, String)] -> FilePath -> [String] -> IO ()
runToolWithEnvironment environment = runToolWith (\process -> process {env = environment})

-- | Run a tool from a directory with the given environment.
runToolIn :: FilePath -> [(String, String)] -> FilePath -> [String] -> IO ()
runToolIn directory environment = runToolWith (\process -> process {cwd = Just directory, env = Just environment})

runToolWith :: (CreateProcess -> CreateProcess) -> FilePath -> [String] -> IO ()
runToolWith adjust executable arguments = do
  (status, output, errors) <- readCreateProcessWithExitCode (adjust (proc executable arguments)) ""
  case status of
    ExitSuccess -> pure ()
    ExitFailure code ->
      ioError
        ( userError
            ( executable
                <> " failed with exit code "
                <> show code
                <> ":\n"
                <> if null errors then output else errors
            )
        )

moduleTypeInterface :: TcKinds -> [TcTermKey] -> ModuleExports -> Package -> TcInterface -> SourceModule -> TcInterface
moduleTypeInterface kinds supportTerms exports package interface source =
  addReferencedFacts
    (typeLiteralKindTyCons kinds)
    supportTerms
    interface
    interface
      { tcInterfaceTermMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfaceTermMap interface),
        tcInterfaceTyConMap = Map.filter visibleTyCon (tcInterfaceTyConMap interface),
        tcInterfaceDataTypeMap = Map.filterWithKey (\key _ -> visibleTypeIdentity key) (tcInterfaceDataTypeMap interface),
        tcInterfaceClassMap = Map.filter visibleClass (tcInterfaceClassMap interface),
        tcInterfaceInstanceMap = Map.filter visibleInstance (tcInterfaceInstanceMap interface),
        tcInterfaceDataFamilyInstanceMap = Map.filter visibleDataFamilyInstance (tcInterfaceDataFamilyInstanceMap interface),
        tcInterfaceTypeFamilyInstanceMap = Map.filter visibleTypeFamilyInstance (tcInterfaceTypeFamilyInstanceMap interface),
        tcInterfacePatSynMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfacePatSynMap interface),
        tcInterfaceForeignImportMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfaceForeignImportMap interface)
      }
  where
    name = sourceModuleName source
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
    localTyCon tyCon = tyConPackageId tyCon == packageId package && tyConModuleName tyCon == name
    visibleTerm (TcTermGlobal packageId' moduleName' identifier) =
      visibleTermIdentity (packageId', moduleName', identifier)
        || any (visibleTermIdentity . (packageId',moduleName',)) (patSynHelperBase identifier)
    visibleTerm (TcTermLocal {}) = False
    visibleTermIdentity identity@(_, _, identifier) =
      Map.member identifier (scopeTerms scope) || identity `Set.member` termIdentities || identity == localIdentity identifier
    -- The matcher and the builder of a visible pattern synonym are visible.
    patSynHelperBase identifier = mapMaybe (`T.stripPrefix` identifier) ["$m", "$b"]
    visibleTyCon info =
      let tyCon = tciTyCon info
          identity = (tyConPackageId tyCon, tyConModuleName tyCon, tciName info)
          (namespaceScope, namespaceIdentities) =
            case tyConNamespace tyCon of
              ResolutionNamespaceTerm -> (scopeTerms scope, termIdentities)
              ResolutionNamespaceType -> (scopeTypes scope, typeIdentities)
              ResolutionNamespaceModule -> (Map.empty, Set.empty)
       in Map.member (tciName info) namespaceScope || identity `Set.member` namespaceIdentities || identity == localIdentity (tciName info)
    visibleTypeIdentity (packageId', moduleName', namespace, identifier) =
      let identity = (packageId', moduleName', identifier)
       in namespace == ResolutionNamespaceType
            && (Map.member identifier (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity identifier)
    visibleClass info =
      case ciOrigin info of
        Just (packageIdText, moduleName') ->
          let identity = (PackageId packageIdText, moduleName', ciName info)
           in Map.member (ciName info) (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity (ciName info)
        Nothing -> False
    visibleInstance info = iiDictOrigin info == (packageIdText (packageId package), name)
    visibleDataFamilyInstance = localTyCon . dfiiRepresentationTyCon
    visibleTypeFamilyInstance info = any localTyCon (typeTyCons (tfiiLeft info) <> typeTyCons (tfiiRight info))
    resolvedIdentity resolved = case resolved of
      ResolvedTopLevel packageId' resolvedModule resolvedName -> Just (packageId', resolvedModule, nameText resolvedName)
      _ -> Nothing

-- | The kinds of the type-level literals. A literal names no type
-- constructor of its own, but its kind is one and the desugarer needs that
-- kind's declaration, so every module carries the three.
typeLiteralKindTyCons :: TcKinds -> [TyCon]
typeLiteralKindTyCons kinds =
  [kindsNaturalTyCon kinds, kindsSymbolTyCon kinds, kindsCharTyCon kinds]

-- | The terms that the evidence of a known type-level literal is built
-- from. The desugarer writes a call of this whether or not the module
-- names the module it comes from.
typeLiteralSupportTerms :: PackageId -> [TcTermKey]
typeLiteralSupportTerms prim =
  [TcTermGlobal prim "GHC.Prim.Natural" "naturalFromInteger#"]

-- | Carry into an interface the facts it refers to but does not hold.
--
-- The extra roots are type constructors the module needs that nothing in
-- its own facts names: the kinds of the type-level literals, which a
-- literal refers to without naming.
addReferencedFacts :: [TyCon] -> [TcTermKey] -> TcInterface -> TcInterface -> TcInterface
addReferencedFacts extraRoots extraTerms complete interface =
  interface
    { tcInterfaceTermMap = tcInterfaceTermMap interface <> Map.fromList (callStackSupportTerms <> typeableSupportTerms),
      tcInterfaceTyConMap = tcInterfaceTyConMap interface <> supportTyCons,
      tcInterfaceDataTypeMap = tcInterfaceDataTypeMap interface <> supportDataTypes,
      tcInterfaceClassMap = tcInterfaceClassMap interface <> supportClasses
    }
  where
    availableTyCons = tcInterfaceTyConMap complete
    availableDataTypes = tcInterfaceDataTypeMap complete
    availableClasses = tcInterfaceClassMap complete
    -- A use of a function with a HasCallStack constraint desugars to calls
    -- of the call-stack helpers, even when the module does not import them.
    callStackModules =
      Set.fromList
        [ (tyConPackageId tyCon, tyConModuleName tyCon)
        | tyCon <- Set.toList (Set.unions (map (typeSchemeTyCons . snd) (tcInterfaceTerms interface))),
          tyConName tyCon == "CallStack"
        ]
    callStackSupportTerms =
      [ (key, scheme)
      | (package', moduleName') <- Set.toList callStackModules,
        identifier <- ["pushCallStack", "emptyCallStack"],
        let key = TcTermGlobal package' moduleName' identifier,
        key `Map.notMember` tcInterfaceTermMap interface,
        Just scheme <- [Map.lookup key (tcInterfaceTermMap complete)]
      ]
        <> [ (key, scheme)
           | key <- extraTerms,
             key `Map.notMember` tcInterfaceTermMap interface,
             Just scheme <- [Map.lookup key (tcInterfaceTermMap complete)]
           ]
    callStackSupportTyCons
      | Set.null callStackModules = []
      | otherwise =
          [ tyCon
          | info <- Map.elems availableTyCons,
            let tyCon = tciTyCon info,
            (tyConPackageId tyCon, tyConModuleName tyCon) `Set.member` callStackModules,
            tyConName tyCon `elem` ["SrcLoc", "CallStack"]
          ]
    referenced =
      interfaceTyCons interface
        <> Set.unions (map (typeSchemeTyCons . snd) callStackSupportTerms)
        <> Set.fromList callStackSupportTyCons
        <> Set.fromList extraRoots
    reachable = closeTyCons Set.empty referenced
    -- Typeable evidence for an applied type desugars to a call of the
    -- class's @typeRep@ selector on the evidence of each argument. The
    -- class reaches a module as the superclass of one it names, so the
    -- module may hold the class without ever importing the selector.
    typeableSupportTerms =
      [ (key, scheme)
      | tyCon <- Set.toList reachable,
        tyConName tyCon == "Typeable",
        tyConModuleName tyCon `elem` ["Type.Reflection", "Type.Reflection.Internal"],
        let key = TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon) "typeRep",
        key `Map.notMember` tcInterfaceTermMap interface,
        Just scheme <- [Map.lookup key (tcInterfaceTermMap complete)]
      ]
    reachableKeys = Set.map tyConKey reachable
    supportTyCons = Map.restrictKeys availableTyCons (reachableKeys `Set.difference` Map.keysSet (tcInterfaceTyConMap interface))
    supportDataTypes = Map.restrictKeys availableDataTypes (reachableKeys `Set.difference` Map.keysSet (tcInterfaceDataTypeMap interface))
    supportClasses = Map.restrictKeys availableClasses (reachableKeys `Set.difference` Map.keysSet (tcInterfaceClassMap interface))
    closeTyCons found pending
      | Set.null pending = found
      | otherwise =
          let (tyCon, pending') = Set.deleteFindMin pending
              key = tyConKey tyCon
              dependencies =
                maybe mempty tyConInfoTyCons (Map.lookup key availableTyCons)
                  <> maybe mempty dataTypeInfoTyCons (Map.lookup key availableDataTypes)
                  <> maybe mempty classInfoTyCons (Map.lookup key availableClasses)
              found' = Set.insert tyCon found
           in closeTyCons found' (pending' <> (dependencies `Set.difference` found'))

writeTypeArtifact :: (String -> IO ()) -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO (Text, Text)
writeTypeArtifact verbose artifactPath source interface = do
  let path = artifactPath source
      name = sourceModuleName source
      (artifactBytes, interfaceBytes) = encodeTypeArtifactParts (TypeArtifact name Map.empty interface)
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path artifactBytes
  verbose ("Write type interface: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict interfaceBytes]))

moduleNameDirectory :: Text -> FilePath
moduleNameDirectory = foldl' (</>) "" . map T.unpack . T.splitOn "."

-- | Write the resolve artifact of a module and return the digest of the
-- scope inside it, taken from the bytes as written.
writeArtifact :: (String -> IO ()) -> ModuleExports -> Package -> FilePath -> SourceModule -> IO (Text, Text)
writeArtifact verbose exports package path source = do
  createDirectoryIfMissing True (takeDirectory path)
  let name = sourceModuleName source
      scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
      (artifactBytes, scopeBytes) = encodeResolveArtifactParts (ResolveArtifact name scope)
  BL.writeFile path artifactBytes
  verbose ("Write resolve context: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict scopeBytes]))

-- | Read a type artifact and report its path if the bytes are not valid.
readTypeArtifactFile :: FilePath -> IO TypeArtifact
readTypeArtifactFile path = BL.readFile path >>= readTypeArtifact path

readTypeArtifact :: FilePath -> BL.ByteString -> IO TypeArtifact
readTypeArtifact path bytes =
  either (ioError . userError . (("Invalid type artifact " <> path <> ": ") <>)) pure (decodeTypeArtifact bytes)

stableHash :: [BS.ByteString] -> String
stableHash = hashChunks

packageArtifactFormatVersion :: Text
packageArtifactFormatVersion = "aihc-artifacts-29"
