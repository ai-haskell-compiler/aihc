{-# LANGUAGE PatternSynonyms #-}

module Aihc.Cli.Install
  ( InstallResult (..),
    InstallLocations (..),
    InstalledPackage (..),
    archiveHasMembers,
    CompiledExecutable (..),
    ExecutableComponent (..),
    FcModule (..),
    ModuleCompileConfig (..),
    ModuleOutputPaths (..),
    backendOptionsKey,
    cabalPlatformForTarget,
    compileFcModules,
    optimizeFcProgram,
    moduleOutputPaths,
    packageLinkArguments,
    buildEnvironmentIdentity,
    defaultBuildRoot,
    install,
    installExecutables,
    installWith,
    installTargetRoot,
    newModuleCompileConfig,
    parsePackageTarget,
    planProgressItem,
    planProgressItems,
    planRequestFor,
    runInstall,
    sourceFileModuleName,

    -- * The front end, one phase at a time

    -- The pieces of the pipeline that @aihc-dev frontend@ drives one phase
    -- at a time, over every unit of a package, to time each phase on its
    -- own. An install interleaves them per unit in one task graph.
    InstanceProvider,
    PackageInputs (..),
    SourceModule (..),
    SourceUnit (..),
    UnitId (..),
    addReferencedFacts,
    builtinFunctionScope,
    configMergeCheck,
    configurePackage,
    excerptSourceLoader,
    instanceFacts,
    interfaceInstanceProviders,
    moduleTypeInterface,
    packagePrimIdentity,
    parseSource,
    preprocessPackage,
    primKinds,
    readPackageInputs,
    renderFrontendFailure,
    runConfigureScript,
    selectInstanceProviders,
    sourceDependencyNames,
    sourceModuleUnits,
    takePackageModuleUnits,
    typeLiteralKindTyCons,
    typeLiteralSupportTerms,
    unitLabel,
    wiredInterfaceModules,
  )
where

import Aihc.Cabal (HookedBuildInfo (..), parseHookedBuildInfo, parseValue)
import Aihc.Cabal qualified as Cabal
import Aihc.Capi (moduleCapiWrappers, parseDependencyFile, renderCapiStub)
import Aihc.Cli.ArtifactCache (compilerBuildIdentity, executableIdentity, hashChunks, sourceFilesHash)
import Aihc.Cli.Backend (compileGrinTo, compileLirObject, lirModuleDefinesCode, nativeSourceExtension, nativeSourceIsLir)
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
import Aihc.Cli.Hackage (defaultHackageSource)
import Aihc.Cli.InterfaceTyCons (classInfoTyCons, dataTypeInfoTyCons, interfaceNonTermRootTyCons, interfaceTermTyCons, tyConInfoTyCons, typeSchemeTyCons, typeTyCons)
import Aihc.Cli.ModuleProvider
  ( InstanceProvider,
    ModuleProvider,
    PackageLocator,
    PackageSource (..),
    ResolvedModuleFacts (..),
    TypedModuleFacts (..),
    moduleNameDirectory,
    newModuleProvider,
    providerInstanceFacts,
    providerPackagesOf,
    providerResolved,
    providerTyped,
  )
import Aihc.Cli.OptimizationPlan (OptimizationPlan (..), optimizationPlan)
import Aihc.Cli.Options (InstallOptions (..), PlanOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.Progress (ProgressEvent (..), ProgressItem (..), ProgressReporter (..), progressTaskObserver, quietProgress, withProgress)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifactParts)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Cli.TaskGraph
  ( Task (..),
    TaskGraph,
    TaskId (..),
    TaskKind (..),
    addTasks,
    allocateTaskIds,
    renderDuration,
    renderTaskTimeline,
    runTaskGraphWith,
  )
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeArtifactParts)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..))
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (cabalMacrosHeader)
import Aihc.Hackage.Package (Arch, OS, mkPackageName, packageNameOf, parsePackageIdentifier, parseVersionString, showVersion, unFlagAssignment, unFlagName, unPackageName)
import Aihc.Hackage.Package qualified as HackagePackage
import Aihc.Hackage.Preprocessor (Preprocessor (..), preprocessorEnvironmentVariable, preprocessorToolName)
import Aihc.Hackage.Source (HackageSource)
import Aihc.Lir.Resolve qualified as Lir
import Aihc.Native (NativeTarget (..), OptimizationLevel, WasmSysroot (..), backendArchiver, backendCompiler, cxxStandardLibraryArguments, defaultOptimizationLevel, handwrittenCArguments, handwrittenCOverrideArguments, hostNativeTarget, llvmLtoArguments, nativeTargetHasFrameworks, nativeTargetStoreDirectory, optimizationArgument, renderOptimizationLevel, wasmSysroot)
import Aihc.PackagePlan
  ( DependencyVersions,
    LockMode (..),
    PackagePlan (..),
    PlanOrigin (..),
    PlanRequest (..),
    PlanRoot (..),
    PlannedPackages (..),
    dependencyVersionsFromManifests,
    parseConstraint,
    parseSourcePackageDescriptionAt,
    planBuildContext,
    planPackages,
  )
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap, renderHumanDiagnostic)
import Aihc.PackagePlan.Lock (lockFileName)
import Aihc.PackagePlan.Source (ParsedInterfaceFile (..), moduleDepsDigest, parseInterfaceBytes)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    Module,
    SourceSpan,
    moduleName,
    sourceSpanSourceName,
    pattern SourceSpan,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Prim.Wiring (primDerivingReferences, primTcConfig, primTcWiring)
import Aihc.Resolve
  ( Builtins,
    Entity (..),
    GlobalName (..),
    ModuleExports,
    ModuleKey (..),
    ModuleUnit,
    Package (..),
    PackageId (..),
    ResolutionNamespace (..),
    ResolveError (..),
    ResolveFailure (..),
    ResolvedUnit (..),
    builtins,
    collectModuleExportsWithDeps,
    exportedTerms,
    exportedTypes,
    lookupModuleExport,
    moduleExportKeys,
    moduleExportsFromList,
    modulesInPackage,
    resolveUnit,
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
    tcInterfaceTypeFamilyInstances,
    tcModuleBindings,
    tcModuleDiagnostics,
    tyConKey,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Share (shareTcInterface)
import Aihc.Tc.Types (TyCon, kindsCharTyCon, kindsNaturalTyCon, kindsSymbolTyCon, tyConModuleName, tyConName, tyConNamespace, tyConPackageId)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (TMVar, TVar, atomically, modifyTVar', newEmptyTMVarIO, newTVarIO, putTMVar, readTMVar, readTVar, takeTMVar, tryReadTMVar, tryTakeTMVar, writeTVar)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (IOException, SomeException, bracket_, evaluate, finally, throwIO, try)
import Control.Monad (filterM, foldM, forM, forM_, unless, void, when, zipWithM)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromRight)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate, isSuffixOf, nub, partition, sortOn)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import GHC.IO.Handle.Lock qualified as HandleLock
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getFileSize, listDirectory, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, isRelative, makeRelative, splitDirectories, takeDirectory, takeFileName, (<.>), (</>))
import System.IO (IOMode (ReadWriteMode), hClose, hPutStrLn, openBinaryTempFile, stderr, stdout, withFile)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcess, readCreateProcessWithExitCode)

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
    installedManifest :: !PackageManifest
  }
  deriving (Generic)

instance NFData InstalledPackage

-- | A fact of each module of several packages, by module name and then by
-- package. Two packages can each hold a module of one name, as @filepath@
-- and @os-string@ both hold @System.OsString.Internal.Types@, so a map by
-- module name alone would keep the fact of one package only.
type ByModule value = Map.Map Text (Map.Map Package value)

byModuleFromList :: [(ModuleKey, value)] -> ByModule value
byModuleFromList entries =
  Map.fromListWith
    Map.union
    [(name, Map.singleton package value) | (ModuleKey package name, value) <- entries]

-- | The facts of the modules of one package.
ownModules :: Package -> Map.Map Text value -> ByModule value
ownModules package = Map.map (Map.singleton package)

-- | Left-biased, as 'Map.union' is: where two maps hold a fact for the same
-- module of the same package, the left one wins.
byModuleUnions :: [ByModule value] -> ByModule value
byModuleUnions = Map.unionsWith Map.union

-- | The fact of the module of that name in each package that holds one.
byModuleLookupName :: Text -> ByModule value -> [(Package, value)]
byModuleLookupName name = maybe [] Map.toList . Map.lookup name

-- | The stamp inputs of a unit for the digests of the modules it imports
-- from outside itself, one for each package that holds such a module.
dependencyInputs :: Text -> [Text] -> [Text] -> ByModule Text -> [(Text, Text)]
dependencyInputs label unitNames dependencyNames digests =
  [ (label <> packageIdText (packageId package) <> ":" <> name, digest)
  | name <- dependencyNames,
    name `notElem` unitNames,
    (package, digest) <- byModuleLookupName name digests
  ]

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
  = TypeInputResolved ResolvedUnit
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
    resolveUnitScopeHashes :: !(ByModule Text),
    resolveUnitErrors :: ![ResolveError],
    resolveUnitSuccess :: !Bool
  }

data TypeUnitResult = TypeUnitResult
  { typeUnitTypes :: !(ByModule TcInterface),
    typeUnitHashes :: !(ByModule Text),
    typeUnitOwnInstanceInterface :: !TcInterface,
    -- | The digest of the instance facts of the unit. The facts artifact
    -- carries the facts digests of the dependencies, so this digest changes
    -- when an instance anywhere below the unit changes.
    typeUnitFactsDigest :: !Text,
    typeUnitInstanceInterface :: !TcInterface,
    -- | The modules whose instances the unit sees, as a consumer in the
    -- same graph takes them.
    typeUnitInstanceProviders :: !(Set.Set InstanceProvider),
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
    runtimeResolveTask :: !TaskId,
    runtimeTypeTask :: !TaskId,
    runtimeBackendTask :: !TaskId,
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
    -- | Run the heap points-to analysis of GRIN and its rewrites on each
    -- program that the backend lowers. Only a whole-program plan sets it.
    compileGrinPointsTo :: !Bool,
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
    compileUseColor :: !Bool,
    -- | Where the progress of the build goes.
    compileProgress :: !ProgressReporter
  }

-- | An executable that the install graph compiles beside the packages of
-- the plan. Its modules are a package of their own, and its units start
-- when the units they import are ready.
data ExecutableComponent = ExecutableComponent
  { -- | The package of the modules. The entry archive of the target refers
    -- to the entry of the package whose identity is @exe@.
    componentPackage :: !Package,
    componentSourceRoot :: !FilePath,
    -- | Where the artifacts of the modules and the C objects are written.
    componentOutputRoot :: !FilePath,
    componentDependencies :: ![PackagePlan],
    -- | What the progress names the executable.
    componentItem :: !ProgressItem,
    -- | The source files and the C inputs of the executable, given every
    -- package below it as it builds.
    componentInputs :: [InstalledPackage] -> IO ([HackageCabal.FileInfo], HackageCabal.CCompileInfo)
  }

-- | An executable after the install graph: its objects, and the packages
-- it links, as they are published.
data CompiledExecutable = CompiledExecutable
  { compiledModuleNames :: ![Text],
    -- | The objects of the modules, and of their capi wrappers. A @--lto@
    -- build has wrapper objects only.
    compiledModuleObjects :: ![FilePath],
    -- | The objects of the C sources of the executable itself.
    compiledCObjects :: ![FilePath],
    compiledCCompileInfo :: !HackageCabal.CCompileInfo,
    -- | Every package below the executable, each once.
    compiledPackages :: ![InstalledPackage]
  }

data CompiledPackageModules = CompiledPackageModules
  { compiledSources :: ![SourceModule],
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
    -- | The modules of the dependency packages. A unit reads the ones it
    -- imports from here.
    taskModuleProvider :: !ModuleProvider,
    taskCapiStubOptions :: !CapiStubOptions,
    taskBackendPhaseTimings :: !(IORef BackendPhaseTimings)
  }

-- | Install a package with the progress on stderr, and name the store
-- entry on stdout.
runInstall :: InstallOptions -> IO ()
runInstall options = do
  result <- withProgress stderr (`installWith` options)
  putStrLn ("store: " <> installStorePath result)

-- | Install a package without progress. The verbose and timing messages go
-- to stdout. A library caller, such as a test, uses this entry point.
install :: InstallOptions -> IO InstallResult
install = installWith (quietProgress stdout)

-- | Install a package and report the progress, the verbose messages, and
-- the timing messages to the reporter. A test gives a reporter that writes
-- to a file and reads the file. The test must not redirect the process
-- stdout instead: the test runner writes its progress to stdout from other
-- threads, and a redirect would capture that progress.
installWith :: ProgressReporter -> InstallOptions -> IO InstallResult
installWith reporter options = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot options)
  let target = installTarget options
      targetDirectory = nativeTargetStoreDirectory target
      useColor = progressColor reporter
      report = progressReport reporter
  let verbose message = when (installVerbose options) (report (ProgressLog message))
      printTimings message = when (installPrintTimings options) (report (ProgressLog message))
  hackageSource <- defaultHackageSource
  (root, origin, lockDirectory) <- installTargetRoot (installPackageTarget options)
  request <- planRequestFor hackageSource (installPlanOptions options) (cabalPlatformForTarget target) (maybe [] pure (installWorkspace options)) lockDirectory verbose
  planned <- planPackages request {requestRoots = [root]}
  plan <- case plannedRoots planned of
    [rootPlan] -> pure rootPlan
    _ -> ioError (userError "The plan has no root")
  report (ProgressPlan (planProgressItems [plan]))
  buildRoot <- maybe (pure (defaultBuildRoot (planSourcePath plan))) pure (installBuildRoot options)
  levelConfig <- newModuleCompileConfig target (storeRoot </> targetDirectory) (installLto options) (installOptimization options)
  let config =
        levelConfig
          { compileKeepCore = installKeepCore options,
            compileKeepGrin = installKeepGrin options,
            compileKeepNative = installKeepNative options,
            compileLint = installLint options,
            compileCheckPrimBounds = installCheckPrimBounds options,
            compileNoCode = installNoCode options,
            compileVerbose = verbose,
            compilePrintTimings = printTimings,
            compileUseColor = useColor,
            compileProgress = reporter
          }
      locations =
        InstallLocations
          { locationStoreRoot = storeRoot </> targetDirectory,
            locationBuildRoot = buildRoot </> targetDirectory,
            locationImmutable = installImmutable options,
            locationReinstall = installReinstall options
          }
  -- The plan finds the package being installed by its name, which marks
  -- it local. What the user asked for decides instead: a directory is local,
  -- a Hackage release is not.
  installedResult <$> installPackagePlan config locations plan {planOrigin = origin}

-- | The compile config of a command at the given level, with no output
-- kept, no lint, and no messages. Each command then sets what its own
-- options ask for. The headers of the target go under the store and not
-- under a build directory, because an immutable install writes no build
-- directory at all.
newModuleCompileConfig :: NativeTarget -> FilePath -> Bool -> OptimizationLevel -> IO ModuleCompileConfig
newModuleCompileConfig target storeTargetRoot lto level = do
  buildIdentity <- buildEnvironmentIdentity target
  headerDirectory <- ensureCompilerHeaders target storeTargetRoot
  let plan = optimizationPlan lto level
  pure
    ModuleCompileConfig
      { compileBuildIdentity = buildIdentity,
        compileKeepCore = False,
        compileKeepGrin = False,
        compileKeepLir = False,
        compileKeepNative = False,
        compileLint = False,
        compileCheckPrimBounds = False,
        compileLto = planWholeProgram plan,
        compilePasses = planPasses plan,
        compileGrinPointsTo = planGrinPointsTo plan,
        compileNoCode = False,
        compileOptimization = level,
        compileTarget = target,
        compileHeaderDirectory = headerDirectory,
        compileVerbose = const (pure ()),
        compilePrintTimings = const (pure ()),
        compileUseColor = False,
        compileProgress = quietProgress stdout
      }

-- | The config of a package the user did not name. The flags that keep the
-- output of a phase name the packages the user named alone, so a
-- dependency in the store is never rejected for lacking those outputs.
dependencyCompileConfig :: ModuleCompileConfig -> ModuleCompileConfig
dependencyCompileConfig config =
  config
    { compileKeepCore = False,
      compileKeepGrin = False,
      compileKeepLir = False,
      compileKeepNative = False
    }

-- | Where a local package builds unless @--build-root@ says otherwise.
defaultBuildRoot :: FilePath -> FilePath
defaultBuildRoot root = root </> ".aihc-target"

-- | What the progress names a planned package: its name and version.
planProgressItem :: PackagePlan -> ProgressItem
planProgressItem plan =
  ItemPackage (T.pack (unPackageName (planName plan) <> "-" <> showVersion (Cabal.packageVersion (planDescription plan))))

-- | The packages of the plans in the order they build: the dependencies
-- of a package before it, and each package once.
planProgressItems :: [PackagePlan] -> [ProgressItem]
planProgressItems = reverse . foldl' visit []
  where
    visit seen plan =
      let below = foldl' visit seen (planDependencyPlans plan)
          item = planProgressItem plan
       in if item `elem` below then below else item : below

-- | Turn the install argument into a plan root, say where it came from,
-- and where its lock file lives.
--
-- An existing directory is used as-is, and its lock lives beside its cabal
-- file. Anything else is parsed as a Hackage package name with an optional
-- version (@NAME@ or @NAME-VERSION@). Hackage targets do not use a lock file.
-- Without a version, the solver selects one.
installTargetRoot :: String -> IO (PlanRoot, PlanOrigin, Maybe FilePath)
installTargetRoot target = do
  isDirectory <- doesDirectoryExist target
  if isDirectory
    then do
      (cabalFile, _) <- parseSourcePackageDescriptionAt target
      pure (RootLocal target, PlanLocal, Just (takeDirectory cabalFile))
    else case parsePackageTarget target of
      Nothing ->
        ioError
          ( userError
              (target <> " is not an existing directory nor a Hackage package name (NAME[-VERSION])")
          )
      Just (name, requestedVersion) -> do
        version <- forM requestedVersion $ \text ->
          maybe (ioError (userError ("Invalid version " <> text))) pure (parseVersionString text)
        pure (RootHackage name version, PlanHackage, Nothing)

-- | Split a Hackage target into its package name and optional version.
parsePackageTarget :: String -> Maybe (String, Maybe String)
parsePackageTarget target = do
  (name, version) <- parsePackageIdentifier target
  pure (unPackageName name, showVersion <$> version)

-- | The plan request the command-line plan options describe, without its
-- roots and goals. A local target uses @aihc.lock@ in the given directory.
planRequestFor :: Maybe HackageSource -> PlanOptions -> (OS, Arch) -> [FilePath] -> Maybe FilePath -> (String -> IO ()) -> IO PlanRequest
planRequestFor hackage options platform workspaces lockDirectory verbose = do
  constraints <- forM (planConstraints options) $ \text ->
    either (ioError . userError) pure (parseConstraint text)
  lockMode <-
    case (planLocked options, planUpdate options, planUpdatePackages options) of
      (True, False, []) -> pure LockLocked
      (False, True, []) -> pure LockUpdateAll
      (False, False, []) -> pure LockNormal
      (False, False, names) -> pure (LockUpdate (map mkPackageName names))
      _ -> ioError (userError "--locked, --update, and --update-package exclude one another")
  pure
    PlanRequest
      { requestRoots = [],
        requestGoals = [],
        requestExecutables = Nothing,
        requestCheckBuildTools = True,
        requestWorkspaces = workspaces,
        requestHackage = hackage,
        requestPlatform = platform,
        requestConstraints = concat constraints,
        requestLockFile = (</> lockFileName) <$> lockDirectory,
        requestLockMode = lockMode,
        requestVerbose = verbose
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

-- | What every package of one install shares.
data InstallShared = InstallShared
  { sharedConfig :: !ModuleCompileConfig,
    sharedLocations :: !InstallLocations,
    -- | The temporary store roots of the packages that build. A package is
    -- published from its root when the graph ends, and what is left after
    -- that is removed.
    sharedTemporaryRoots :: !(IORef (Set.Set FilePath)),
    sharedBackendPhaseTimings :: !(IORef BackendPhaseTimings)
  }

-- | One package of a plan, as the graph installs it. The package has four
-- tasks of its own. Configure reads the package and runs its configure
-- script. It waits for no other task, because the script sees only the C
-- compiler. Prepare preprocesses the package, or takes it from the store.
-- Partition cuts its modules into units and adds their tasks. Finish
-- collects the results and archives it. A dependent waits on prepare,
-- partition, and finish, and its units wait on the units they import.
data PackageSlot = PackageSlot
  { slotPlan :: !PackagePlan,
    slotRoot :: !Bool,
    slotDependencies :: ![PackageSlot],
    -- | The dependencies and every package below them, each once. A unit
    -- reads the instance facts of any of them, through the locator.
    slotClosure :: ![PackageSlot],
    -- | The place of the package in the plan, after its dependencies.
    slotOrder :: !Int,
    slotConfigureTask :: !TaskId,
    slotPrepareTask :: !TaskId,
    slotPartitionTask :: !TaskId,
    slotFinishTask :: !TaskId,
    -- | The inputs of the package and the directory its configure script
    -- wrote, once its configure task ran.
    slotConfigured :: !(TMVar (PackageInputs, Maybe FilePath)),
    slotPrepared :: !(TMVar PreparedPackage),
    -- | The unit of each compiled module, once the package is partitioned.
    -- Empty for a package the store already holds.
    slotUnits :: !(TMVar (Map.Map Text UnitRuntime)),
    -- | The package once its finish task ran, in the directory it built in.
    slotBuilt :: !(TMVar InstalledPackage),
    -- | The package once it is published.
    slotInstalled :: !(TMVar InstalledPackage),
    -- | The packages that still read the unit results: the package itself
    -- until it finishes, and each package above it until that finishes.
    slotReaders :: !(TVar Int)
  }

-- | A package after its prepare task.
data PreparedPackage = PreparedPackage
  { -- | The package as its dependents see it while it builds, in the
    -- directory it builds in. A store package moves when it is published.
    preparedPackage :: !InstalledPackage,
    preparedExposedModules :: ![Text],
    -- | Where the facts of the modules of this package and of every
    -- package below it come from.
    preparedLocator :: !PackageLocator,
    -- | What the package builds from, or nothing when the store holds it.
    preparedBuild :: !(Maybe PackageBuild)
  }

data PackageBuild = PackageBuild
  { buildSourceRoot :: !FilePath,
    -- | Where the artifacts are written: the build directory of a local
    -- package, or a temporary root beside the store entry.
    buildPath :: !FilePath,
    -- | The store entry the package is published to.
    buildPublishPath :: !FilePath,
    -- | Whether the store entry exists and is replaced on publish.
    buildReplaces :: !Bool,
    buildPackageDirectory :: !FilePath,
    buildUnitIdentity :: !Text,
    buildImmutable :: !Bool,
    buildInputs :: !PackageInputs,
    buildFiles :: ![HackageCabal.FileInfo],
    buildCCompileInfo :: !HackageCabal.CCompileInfo,
    buildHeaderHash :: !String,
    -- | The dependencies as they are while they build.
    buildDependencies :: ![InstalledPackage]
  }

installPackagePlan :: ModuleCompileConfig -> InstallLocations -> PackagePlan -> IO InstalledPackage
installPackagePlan config locations plan = do
  (roots, _) <- installGraph config locations [plan] []
  case roots of
    [slot] -> atomically (readTMVar (slotInstalled slot))
    _ -> ioError (userError "The plan has no root")

-- | Install the packages below the executables and compile the modules and
-- C sources of each executable, all in one task graph. The user named the
-- executables, so they keep the outputs the config asks for; the packages
-- below are dependencies.
installExecutables :: ModuleCompileConfig -> InstallLocations -> [ExecutableComponent] -> IO [CompiledExecutable]
installExecutables config locations components = do
  (_, executables) <- installGraph config locations [] components
  forM executables $ \slot -> do
    compiled <- atomically (readTMVar (executableSlotCompiled slot))
    packages <- mapM (atomically . readTMVar . slotInstalled) (executableSlotClosure slot)
    pure compiled {compiledPackages = packages}

-- | Install the closure of the plans and of the executables in one task
-- graph. A unit waits on the units it imports and on nothing else of the
-- packages below. The result is the slots of the roots and of the
-- executables.
--
-- A store package is published when the graph ends, not when its own
-- tasks end: a dependent reads its headers from the directory it builds in
-- while the graph runs. When a package fails, the packages that finished
-- are published all the same.
installGraph :: ModuleCompileConfig -> InstallLocations -> [PackagePlan] -> [ExecutableComponent] -> IO ([PackageSlot], [ExecutableSlot])
installGraph config locations plans components = do
  capabilities <- getNumCapabilities
  slotsRef <- newIORef Map.empty
  rootsRef <- newIORef []
  executablesRef <- newIORef []
  temporaryRoots <- newIORef Set.empty
  phaseTimings <- newIORef mempty
  let shared =
        InstallShared
          { sharedConfig = config,
            sharedLocations = locations,
            sharedTemporaryRoots = temporaryRoots,
            sharedBackendPhaseTimings = phaseTimings
          }
      removeTemporaryRoots = readIORef temporaryRoots >>= mapM_ removeTemporaryStoreRoot . Set.toList
      publishFinished = readIORef slotsRef >>= mapM_ (publishSlot shared) . sortOn slotOrder . Map.elems
      seed graph = do
        mapM (planSlot shared graph slotsRef True) plans >>= writeIORef rootsRef
        -- The executables come after every package, so each executable
        -- has an order of its own.
        dependencies <- mapM (mapM (planSlot shared graph slotsRef False) . componentDependencies) components
        packageCount <- Map.size <$> readIORef slotsRef
        zipWithM (executableSlot shared graph) [packageCount ..] (zip components dependencies) >>= writeIORef executablesRef
  outcome <- try (runTaskGraphWith (progressTaskObserver (compileProgress config)) (max 1 capabilities) seed)
  timings <- case outcome of
    Right timings -> do
      publishFinished `finally` removeTemporaryRoots
      pure timings
    Left failure -> do
      -- The failure is the one to report, whatever the publish does.
      _ <- try (publishFinished `finally` removeTemporaryRoots) :: IO (Either SomeException ())
      throwIO (failure :: SomeException)
  totals <- readIORef phaseTimings
  compilePrintTimings config (renderTaskTimeline (compileUseColor config) [] timings <> renderBackendPhaseTotals totals)
  roots <- readIORef rootsRef
  executables <- readIORef executablesRef
  pure (roots, executables)

-- | The config a package of the graph compiles with.
slotConfig :: InstallShared -> PackageSlot -> ModuleCompileConfig
slotConfig shared slot
  | slotRoot slot = sharedConfig shared
  | otherwise = dependencyCompileConfig (sharedConfig shared)

-- | The slot of a package, made after the slots of its dependencies, with
-- its prepare task in the graph.
planSlot :: InstallShared -> TaskGraph -> IORef (Map.Map FilePath PackageSlot) -> Bool -> PackagePlan -> IO PackageSlot
planSlot shared graph slotsRef root plan = do
  key <- canonicalizePath (planSourcePath plan)
  known <- Map.lookup key <$> readIORef slotsRef
  case known of
    Just slot -> pure slot
    Nothing -> do
      dependencies <- mapM (planSlot shared graph slotsRef False) (planDependencyPlans plan)
      order <- Map.size <$> readIORef slotsRef
      base <- allocateTaskIds graph 4
      let closure = slotsClosure dependencies
      slot <-
        PackageSlot plan root dependencies closure order (TaskId base) (TaskId (base + 1)) (TaskId (base + 2)) (TaskId (base + 3))
          <$> newEmptyTMVarIO
          <*> newEmptyTMVarIO
          <*> newEmptyTMVarIO
          <*> newEmptyTMVarIO
          <*> newEmptyTMVarIO
          <*> newTVarIO 1
      forM_ closure $ \below -> atomically (modifyTVar' (slotReaders below) (+ 1))
      modifyIORef' slotsRef (Map.insert key slot)
      addTasks
        graph
        [ Task
            { taskId = slotConfigureTask slot,
              taskKind = TaskPackage,
              taskOrder = order,
              taskDependencies = Set.empty,
              taskAction = configureSlot shared slot
            },
          Task
            { taskId = slotPrepareTask slot,
              taskKind = TaskPackage,
              taskOrder = order,
              taskDependencies = Set.fromList (slotConfigureTask slot : map slotPrepareTask dependencies),
              taskAction = preparePackage shared graph slot
            }
        ]
      pure slot

-- | Read a package and run its configure script.
--
-- The answers of the script are kept in a directory of their own, named
-- after what they depend on, and not in the directory of the package. The
-- directory of a store package has a name that depends on the
-- dependencies, and the script does not need them. Thus every script of
-- the plan can start at the start of the install, in parallel with the
-- other scripts and with the compilation of the dependencies. A package
-- that the store holds finds the answers of its earlier install there.
configureSlot :: InstallShared -> PackageSlot -> IO ()
configureSlot shared slot = do
  let config = slotConfig shared slot
      locations = sharedLocations shared
      plan = slotPlan slot
      root
        | storeBound locations plan = locationStoreRoot locations
        | otherwise = locationBuildRoot locations
  inputs <- readPackageInputs config plan
  configured <- runConfigureScript config (root </> ".configure") inputs
  atomically (putTMVar (slotConfigured slot) (inputs, configured))

-- | Whether a package of the plan goes into the store, or builds in place
-- under the build root.
storeBound :: InstallLocations -> PackagePlan -> Bool
storeBound locations plan = locationImmutable locations || planOrigin plan /= PlanLocal

-- | The packages below the slots and the slots themselves, each once, in
-- the order of the plan.
slotsClosure :: [PackageSlot] -> [PackageSlot]
slotsClosure slots = Map.elems (Map.fromList [(slotOrder below, below) | slot <- slots, below <- slot : slotClosure slot])

-- | An executable of the graph. Like a package it has a prepare task, a
-- partition task, and a finish task, but nothing depends on it.
data ExecutableSlot = ExecutableSlot
  { executableSlotComponent :: !ExecutableComponent,
    -- | Every package below the executable, each once.
    executableSlotClosure :: ![PackageSlot],
    executableSlotOrder :: !Int,
    executableSlotPrepareTask :: !TaskId,
    executableSlotPartitionTask :: !TaskId,
    executableSlotFinishTask :: !TaskId,
    -- | The executable once its finish task ran. The packages are added
    -- after the graph, when they are published.
    executableSlotCompiled :: !(TMVar CompiledExecutable)
  }

-- | The slot of an executable, made after the slots of its dependencies,
-- with its prepare task in the graph. The executable reads the unit results
-- of every package below it until it finishes.
executableSlot :: InstallShared -> TaskGraph -> Int -> (ExecutableComponent, [PackageSlot]) -> IO ExecutableSlot
executableSlot shared graph order (component, dependencies) = do
  base <- allocateTaskIds graph 3
  compiled <- newEmptyTMVarIO
  let closure = slotsClosure dependencies
      slot = ExecutableSlot component closure order (TaskId base) (TaskId (base + 1)) (TaskId (base + 2)) compiled
  forM_ closure $ \below -> atomically (modifyTVar' (slotReaders below) (+ 1))
  addTasks
    graph
    [ Task
        { taskId = executableSlotPrepareTask slot,
          taskKind = TaskPackage,
          taskOrder = order,
          taskDependencies = Set.fromList (map slotPrepareTask closure),
          taskAction = prepareExecutable shared graph slot
        }
    ]
  pure slot

-- | Find the sources of an executable and add its parse tasks and its
-- partition task. The modules of the executable see every package below
-- it, as the packages of an install see their dependencies.
prepareExecutable :: InstallShared -> TaskGraph -> ExecutableSlot -> IO ()
prepareExecutable shared graph slot = do
  let config = sharedConfig shared
      component = executableSlotComponent slot
      closure = executableSlotClosure slot
      package = componentPackage component
      outputRoot = componentOutputRoot component
  dependencies <- mapM (fmap preparedPackage . atomically . readTMVar . slotPrepared) closure
  (ownFiles, ownCInfo) <- componentInputs component dependencies
  headerDirs <- dependencyIncludeDirs dependencies
  let files = map (appendIncludeDirs headerDirs) ownFiles
      cCompileInfo = ownCInfo {HackageCabal.cCompileIncludeDirs = nub (HackageCabal.cCompileIncludeDirs ownCInfo <> headerDirs)}
      finished compiledModules = do
        let names = map sourceName (compiledSources compiledModules)
        moduleObjects <- moduleObjectPaths (not (compileLto config)) outputRoot (compileTarget config) names
        cObjects <- compilePackageCFiles (compileTarget config) (compileOptimization config) (compileLto config) (compileHeaderDirectory config) (compileVerbose config) (componentSourceRoot component) outputRoot cCompileInfo
        atomically $
          putTMVar
            (executableSlotCompiled slot)
            CompiledExecutable
              { compiledModuleNames = names,
                compiledModuleObjects = moduleObjects,
                compiledCObjects = cObjects,
                compiledCCompileInfo = cCompileInfo,
                compiledPackages = []
              }
        releaseReaders closure
  addModuleBuild
    graph
    (sharedBackendPhaseTimings shared)
    ModuleBuild
      { moduleBuildConfig = config,
        moduleBuildOutputRoot = outputRoot,
        moduleBuildPackageRoot = componentSourceRoot component,
        moduleBuildPackage = package,
        moduleBuildItem = componentItem component,
        moduleBuildFiles = files,
        moduleBuildVersions = dependencyVersionsFromManifests [(installedName dependency, installedVersion dependency) | dependency <- dependencies],
        moduleBuildCapiOptions = capiStubOptions files cCompileInfo,
        moduleBuildPrimIdentity = dependencyPrimIdentity package dependencies,
        moduleBuildOrder = executableSlotOrder slot,
        moduleBuildPartitionTask = executableSlotPartitionTask slot,
        moduleBuildFinishTask = executableSlotFinishTask slot,
        moduleBuildPartitionAfter = map slotPartitionTask closure,
        moduleBuildFinishAfter = map slotFinishTask closure,
        moduleBuildDependencies = mapM slotDependency closure,
        moduleBuildPartitioned = const (pure ()),
        moduleBuildFinished = finished
      }

-- | Read, configure, and preprocess a package, or take it from the store.
-- A package that builds gets its parse tasks and its partition task here;
-- one the store holds gets a partition task and a finish task that do
-- nothing, so that its dependents wait on the same tasks either way.
preparePackage :: InstallShared -> TaskGraph -> PackageSlot -> IO ()
preparePackage shared graph slot = do
  prepared <- mapM (atomically . readTMVar . slotPrepared) (slotDependencies slot)
  let config = slotConfig shared slot
      locations = sharedLocations shared
      plan = slotPlan slot
      dependencies = map preparedPackage prepared
      -- Only the package the user named is reinstalled.
      reinstall = slotRoot slot && locationReinstall locations
      order = slotOrder slot
      item = planProgressItem plan
      report = progressReport (compileProgress config)
  report (ProgressPrepare item)
  (inputs, configured) <- atomically (readTMVar (slotConfigured slot))
  (installed, build) <-
    if storeBound locations plan
      then prepareStorePackage shared config (slotRoot slot) reinstall dependencies plan inputs configured
      else prepareLocalPackage config reinstall (locationBuildRoot locations) dependencies plan inputs configured
  let identity = PackageId (packageManifestUnitId (installedManifest installed))
      package = Package (installedName installed) identity
      source = case build of
        Nothing -> StorePackage (installStorePath (installedResult installed))
        Just _ -> graphPackageSource package (slotUnits slot)
  atomically $
    putTMVar
      (slotPrepared slot)
      PreparedPackage
        { preparedPackage = installed,
          preparedExposedModules = packageManifestModules (installedManifest installed),
          preparedLocator = Map.insert identity source (Map.unions (map preparedLocator prepared)),
          preparedBuild = build
        }
  case build of
    Nothing -> do
      report (ProgressStore item)
      atomically $ do
        putTMVar (slotUnits slot) Map.empty
        putTMVar (slotBuilt slot) installed
      addTasks
        graph
        [ Task
            { taskId = slotPartitionTask slot,
              taskKind = TaskPackage,
              taskOrder = order,
              taskDependencies = Set.fromList (map slotPartitionTask (slotDependencies slot)),
              taskAction = pure ()
            },
          Task
            { taskId = slotFinishTask slot,
              taskKind = TaskPackage,
              taskOrder = order,
              -- A package the store holds can have a dependency that
              -- builds, whose finish task is not in the graph yet.
              taskDependencies = Set.singleton (slotPartitionTask slot),
              taskAction = releaseReaders (slot : slotClosure slot)
            }
        ]
    Just packageBuild -> addModuleBuild graph (sharedBackendPhaseTimings shared) (packageModuleBuild shared slot installed packageBuild)

-- | The modules of a package of the plan, as the graph compiles them.
packageModuleBuild :: InstallShared -> PackageSlot -> InstalledPackage -> PackageBuild -> ModuleBuild
packageModuleBuild shared slot installed build =
  ModuleBuild
    { moduleBuildConfig = config,
      moduleBuildOutputRoot = buildPath build,
      moduleBuildPackageRoot = buildSourceRoot build,
      moduleBuildPackage = package,
      moduleBuildItem = item,
      moduleBuildFiles = buildFiles build,
      moduleBuildVersions = dependencyVersionsFromManifests [(installedName dependency, installedVersion dependency) | dependency <- dependencies],
      moduleBuildCapiOptions = capiStubOptions (buildFiles build) (buildCCompileInfo build),
      moduleBuildPrimIdentity = dependencyPrimIdentity package dependencies,
      moduleBuildOrder = slotOrder slot,
      moduleBuildPartitionTask = slotPartitionTask slot,
      moduleBuildFinishTask = slotFinishTask slot,
      moduleBuildPartitionAfter = map slotPartitionTask (slotDependencies slot),
      moduleBuildFinishAfter = map slotFinishTask (slotDependencies slot),
      moduleBuildDependencies = mapM slotDependency (slotDependencies slot),
      moduleBuildPartitioned = atomically . putTMVar (slotUnits slot),
      moduleBuildFinished = \compiled -> do
        built <- finishPackageBuild config build compiled
        progressReport (compileProgress config) (ProgressDone item)
        atomically (putTMVar (slotBuilt slot) built)
        releaseReaders (slot : slotClosure slot)
    }
  where
    config = slotConfig shared slot
    item = planProgressItem (slotPlan slot)
    package = Package (installedName installed) (PackageId (buildUnitIdentity build))
    dependencies = buildDependencies build

-- | A package of the graph as the partition task of a module build above
-- it sees it, once the package is partitioned.
slotDependency :: PackageSlot -> IO PreparedDependency
slotDependency slot = do
  prepared <- atomically (readTMVar (slotPrepared slot))
  units <- atomically (readTMVar (slotUnits slot))
  let installed = preparedPackage prepared
      identity = PackageId (packageManifestUnitId (installedManifest installed))
  pure
    PreparedDependency
      { dependencyPackage = Package (installedName installed) identity,
        dependencyExposed = Set.fromList (preparedExposedModules prepared),
        dependencySource = fromMaybe (StorePackage (installStorePath (installedResult installed))) (Map.lookup identity (preparedLocator prepared)),
        dependencyUnits = units,
        dependencyLocator = preparedLocator prepared
      }

-- | Release the unit results that a package or an executable read, when
-- it finished: a package no reader waits on drops them, so that the
-- interfaces of a package die once the last reader above it is done. A
-- package reads its own results and those of every package below it.
releaseReaders :: [PackageSlot] -> IO ()
releaseReaders = mapM_ releaseReader
  where
    releaseReader reader = do
      remaining <- atomically $ do
        count <- readTVar (slotReaders reader)
        writeTVar (slotReaders reader) (count - 1)
        pure (count - 1)
      when (remaining == 0) $ do
        units <- atomically (readTMVar (slotUnits reader))
        forM_ (Map.elems units) $ \runtime ->
          atomically $ do
            void (tryTakeTMVar (runtimeResolveResult runtime))
            void (tryTakeTMVar (runtimeTypeResult runtime))

-- | Publish a package that finished: a store package moves from its
-- temporary root to its store entry. A package that did not finish is
-- left as it is, and its temporary root goes with the others.
publishSlot :: InstallShared -> PackageSlot -> IO ()
publishSlot shared slot = do
  prepared <- atomically (tryReadTMVar (slotPrepared slot))
  built <- atomically (tryReadTMVar (slotBuilt slot))
  case (prepared >>= preparedBuild, built) of
    (Just build, Just package)
      | buildImmutable build -> publishStorePackage shared build package >>= atomically . putTMVar (slotInstalled slot)
    (_, Just package) -> atomically (putTMVar (slotInstalled slot) package)
    _ -> pure ()

-- | The facts of the modules of a package that compiles in the graph,
-- from the results of its units.
graphPackageSource :: Package -> TMVar (Map.Map Text UnitRuntime) -> PackageSource
graphPackageSource package unitsVar =
  GraphPackage
    { graphResolved = \name -> do
        runtime <- unitOf name
        result <- unitResult name (runtimeResolveResult runtime)
        scope <-
          maybe
            (ioError (userError ("The unit of the module has no exports for it: " <> T.unpack name)))
            pure
            (lookupModuleExport (ModuleKey package name) (resolveUnitExports result))
        pure
          ResolvedModuleFacts
            { resolvedModuleScope = scope,
              resolvedModuleScopeDigest = fromMaybe "" (lookup package (byModuleLookupName name (resolveUnitScopeHashes result))),
              resolvedModuleSuccess = resolveUnitSuccess result
            },
      graphTyped = \name -> do
        runtime <- unitOf name
        result <- unitResult name (runtimeTypeResult runtime)
        pure
          TypedModuleFacts
            { typedModuleInterface = fromMaybe emptyTcInterface (lookup package (byModuleLookupName name (typeUnitTypes result))),
              typedModuleTypeDigest = fromMaybe "" (lookup package (byModuleLookupName name (typeUnitHashes result))),
              typedModuleFactsDigest = typeUnitFactsDigest result,
              typedModuleInstanceProviders = typeUnitInstanceProviders result,
              typedModuleSuccess = typeUnitSuccess result
            },
      graphOwnFacts = \name -> do
        runtime <- unitOf name
        typeUnitOwnInstanceInterface <$> unitResult name (runtimeTypeResult runtime)
    }
  where
    -- A result the unit has: a task waits on the task that writes it, so
    -- an absent result is one that was released, which is a defect.
    unitResult name var =
      atomically (tryReadTMVar var)
        >>= maybe (ioError (userError ("The results of the unit of the module were released: " <> T.unpack name))) pure
    unitOf name = do
      units <- atomically (readTMVar unitsVar)
      maybe
        (ioError (userError ("The package " <> T.unpack (packageName package) <> " has no compiled module " <> T.unpack name)))
        pure
        (Map.lookup name units)

data PackageInputs = PackageInputs
  { inputCabalFile :: !FilePath,
    inputDescription :: !Cabal.Package,
    -- | The platform and the cabal flags the plan decided, which close
    -- the conditions of the cabal file.
    inputContext :: !HackageCabal.BuildContext,
    -- | The cabal file revision of a Hackage release.
    inputRevision :: !(Maybe Int),
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
  let context = planBuildContext (cabalPlatformForTarget (compileTarget config)) plan
  files <- HackageCabal.collectLibraryFilesIn context gpd root
  configureScript <- case HackageCabal.packageBuildType gpd of
    HackageCabal.Configure -> do
      let script = root </> "configure"
      exists <- doesFileExist script
      unless exists $
        ioError (userError ("The package has build-type Configure but no configure script: " <> script))
      pure (Just script)
    _ -> pure Nothing
  cCompileInfo <- either (ioError . userError . ((cabalFile <> ": ") <>)) pure (HackageCabal.collectLibraryCCompileInfoIn context gpd root)
  pure
    PackageInputs
      { inputCabalFile = cabalFile,
        inputDescription = gpd,
        inputContext = context,
        inputRevision = planRevision plan,
        inputSources = files,
        inputCCompileInfo = cCompileInfo,
        inputConfigureScript = configureScript,
        inputAutogenIncludes = HackageCabal.collectLibraryAutogenIncludesIn context gpd
      }

-- | Prepare an immutable package for the store, unless the store has it.
prepareStorePackage :: InstallShared -> ModuleCompileConfig -> Bool -> Bool -> [InstalledPackage] -> PackagePlan -> PackageInputs -> Maybe FilePath -> IO (InstalledPackage, Maybe PackageBuild)
prepareStorePackage shared config named reinstall dependencies plan inputs configured = do
  let storeRoot = locationStoreRoot (sharedLocations shared)
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
      package <- loadInstalledPackage True storePath
      requireInstalledFlags config named package
      pure (package, Nothing)
    else do
      createDirectoryIfMissing True storeRoot
      temporaryRoot <- createTemporaryStoreRoot storeRoot packageDirectory
      atomicModifyIORef' (sharedTemporaryRoots shared) (\roots -> (Set.insert temporaryRoot roots, ()))
      preparePackageBuild config packageDirectory unitIdentity True temporaryRoot storePath exists dependencies plan inputs configured

-- | Move a built package from its temporary root to its store entry.
publishStorePackage :: InstallShared -> PackageBuild -> InstalledPackage -> IO InstalledPackage
publishStorePackage shared build built = do
  let storePath = buildPublishPath build
      temporaryRoot = buildPath build
  when (buildReplaces build) (removeDirectoryRecursive storePath)
  publishResult <- try (renameDirectory temporaryRoot storePath)
  package <- case publishResult of
    Right () -> pure (setInstalledStorePath storePath built)
    Left err -> do
      published <- doesDirectoryExist storePath
      if published
        then loadInstalledPackage True storePath
        else throwIO (err :: IOException)
  removeTemporaryStoreRoot temporaryRoot
  atomicModifyIORef' (sharedTemporaryRoots shared) (\roots -> (Set.delete temporaryRoot roots, ()))
  pure package

-- | Prepare a local package to build in place under the build root.
prepareLocalPackage :: ModuleCompileConfig -> Bool -> FilePath -> [InstalledPackage] -> PackagePlan -> PackageInputs -> Maybe FilePath -> IO (InstalledPackage, Maybe PackageBuild)
prepareLocalPackage config reinstall buildRoot dependencies plan inputs configured = do
  let (packageDirectory, unitIdentity) = localPackageIdentity inputs
      buildPath = buildRoot </> packageDirectory
  exists <- doesDirectoryExist buildPath
  when (exists && reinstall) (removeDirectoryRecursive buildPath)
  createDirectoryIfMissing True buildPath
  preparePackageBuild config packageDirectory unitIdentity False buildPath buildPath False dependencies plan inputs configured

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

-- | Apply the answers of the configure script, preprocess a package, and
-- record what its finish needs. The package is returned as its dependents
-- see it while it builds.
preparePackageBuild :: ModuleCompileConfig -> FilePath -> Text -> Bool -> FilePath -> FilePath -> Bool -> [InstalledPackage] -> PackagePlan -> PackageInputs -> Maybe FilePath -> IO (InstalledPackage, Maybe PackageBuild)
preparePackageBuild config packageDirectory unitIdentity immutable buildPath publishPath replaces dependencies plan inputs configured = do
  let root = planSourcePath plan
      verbose = compileVerbose config
  verbose ("Read Cabal package: " <> root)
  let gpd = inputDescription inputs
      packageNameText = HackagePackage.packageNameText (packageNameOf gpd)
  (configuredFiles, configuredCInfo) <- configurePackage root packageNameText inputs configured
  headerDirs <- dependencyIncludeDirs dependencies
  headerHash <- includeDirectoriesHash headerDirs
  let dependencyVersions =
        dependencyVersionsFromManifests
          [(installedName dependency, installedVersion dependency) | dependency <- dependencies]
      cCompileInfo = configuredCInfo {HackageCabal.cCompileIncludeDirs = nub (HackageCabal.cCompileIncludeDirs configuredCInfo <> headerDirs)}
      sourceFiles = map (appendIncludeDirs headerDirs) configuredFiles
  installPackageHeaders root buildPath configuredCInfo
  files <- preprocessPackage config dependencyVersions root buildPath (inputConfigureScript inputs) headerHash cCompileInfo sourceFiles
  let build =
        PackageBuild
          { buildSourceRoot = root,
            buildPath = buildPath,
            buildPublishPath = publishPath,
            buildReplaces = replaces,
            buildPackageDirectory = packageDirectory,
            buildUnitIdentity = unitIdentity,
            buildImmutable = immutable,
            buildInputs = inputs,
            buildFiles = files,
            buildCCompileInfo = cCompileInfo,
            buildHeaderHash = headerHash,
            buildDependencies = dependencies
          }
      -- The compiled modules are known once the modules are parsed; the
      -- manifest written at the finish names them.
      manifest = packageBuildManifest config build []
  pure (installedPackageOf build (InstallResult buildPath [] []) manifest, Just build)

installedPackageOf :: PackageBuild -> InstallResult -> PackageManifest -> InstalledPackage
installedPackageOf build result manifest =
  InstalledPackage
    { installedResult = result,
      installedName = packageManifestName manifest,
      installedVersion = packageManifestVersion manifest,
      installedIdentity = T.pack (buildPackageDirectory build),
      installedImmutable = buildImmutable build,
      installedManifest = manifest
    }

packageBuildManifest :: ModuleCompileConfig -> PackageBuild -> [Text] -> PackageManifest
packageBuildManifest config build compiledModules =
  PackageManifest
    { packageManifestName = HackagePackage.packageNameText (packageNameOf gpd),
      packageManifestVersion = T.pack (showVersion (Cabal.packageVersion gpd)),
      packageManifestIdentity = T.pack (buildPackageDirectory build),
      packageManifestUnitId = buildUnitIdentity build,
      packageManifestDependencies = sortOn id (map installedIdentity (buildDependencies build)),
      packageManifestModules = sortOn id (HackageCabal.collectLibraryExposedModulesIn (inputContext inputs) gpd),
      packageManifestCompiledModules = sortOn id compiledModules,
      packageManifestFlags = compileFlagNames config,
      packageManifestCabalFlags = Map.fromList [(T.pack (unFlagName flag), value) | (flag, value) <- unFlagAssignment (HackageCabal.contextFlags (inputContext inputs))],
      packageManifestCxxStdLib = not (null (HackageCabal.cCompileCxxSources (buildCCompileInfo build))),
      packageManifestLinkArguments = map T.pack (packageLinkArguments (compileTarget config) (buildCCompileInfo build))
    }
  where
    inputs = buildInputs build
    gpd = inputDescription inputs

-- | The arguments that link the system libraries a package names in its
-- Cabal file, for the linker of the target.
packageLinkArguments :: NativeTarget -> HackageCabal.CCompileInfo -> [String]
packageLinkArguments target = HackageCabal.cCompileLinkArguments (nativeTargetHasFrameworks target)

-- | Archive the compiled modules of a package and write its manifest.
finishPackageBuild :: ModuleCompileConfig -> PackageBuild -> CompiledPackageModules -> IO InstalledPackage
finishPackageBuild config build compiled = do
  let target = compileTarget config
      verbose = compileVerbose config
      inputs = buildInputs build
      root = buildSourceRoot build
      storePath = buildPath build
      dependencies = buildDependencies build
      cCompileInfo = buildCCompileInfo build
      packageNameText = HackagePackage.packageNameText (packageNameOf (inputDescription inputs))
      parsed = compiledSources compiled
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
    archiveInputs <- archiveInputsHash config root dependencies inputs (buildHeaderHash build)
    let stampPath = storePath </> "lib" </> "archive.hash"
    previous <- readStampText stampPath
    archiveExists <- doesFileExist archive
    let current = not (Set.null written) || not archiveExists || previous /= Just archiveInputs
    if current
      then do
        cObjects <- compilePackageCFiles target (compileOptimization config) (compileLto config) (compileHeaderDirectory config) verbose root storePath cCompileInfo
        buildLibraryArchive target verbose archive (moduleObjects <> cObjects)
        BS8.writeFile stampPath (BS8.pack archiveInputs)
      else verbose ("Reuse archive: " <> archive)
  let manifest = packageBuildManifest config build (map sourceName parsed)
  writePackageManifest (packageManifestPath storePath) manifest
  pure (installedPackageOf build (InstallResult storePath (Set.toAscList written) (Set.toAscList reused)) manifest)

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

-- | The module name that a source file declares. The file goes through the
-- same preprocessing and parse as the modules of a package.
sourceFileModuleName :: ModuleCompileConfig -> FilePath -> [InstalledPackage] -> HackageCabal.FileInfo -> IO Text
sourceFileModuleName config packageRoot dependencies file = do
  headerDirs <- dependencyIncludeDirs dependencies
  let versions =
        dependencyVersionsFromManifests
          [(installedName dependency, installedVersion dependency) | dependency <- dependencies]
  sourceName <$> parseSource (compileHeaderDirectory config) packageRoot versions (appendIncludeDirs headerDirs file)

-- | The objects of a set of modules: one for each module when the modules
-- have objects, and the capi wrappers of those that declare any.
--
-- The wrappers are found on disk rather than reported by the backend,
-- because a module whose artifacts were reused compiled nothing this time and
-- still has the wrapper object it built before.  A module that no longer
-- declares a capi import has had its wrapper object removed, so what is there
-- is what belongs in the link.
--
-- A module with no declarations has an empty object file. The linker
-- refuses an empty file, so these objects are not in the list.
moduleObjectPaths :: Bool -> FilePath -> NativeTarget -> [Text] -> IO [FilePath]
moduleObjectPaths withModuleObjects root target names = do
  capiObjects <- filterM doesFileExist [outputCapiObjectPath (paths name) | name <- names]
  moduleObjects <- filterM (fmap (> 0) . getFileSize) [outputObjectPath (paths name) | withModuleObjects, name <- names]
  pure (sortOn id (moduleObjects <> capiObjects))
  where
    paths = moduleOutputPaths root target

-- | The modules of one package, compiled in a graph: parse tasks at once,
-- the unit tasks once the partition task has cut the modules into units,
-- and a finish task that collects the results.
data ModuleBuild = ModuleBuild
  { moduleBuildConfig :: !ModuleCompileConfig,
    moduleBuildOutputRoot :: !FilePath,
    moduleBuildPackageRoot :: !FilePath,
    moduleBuildPackage :: !Package,
    -- | What the progress names the package.
    moduleBuildItem :: !ProgressItem,
    moduleBuildFiles :: ![HackageCabal.FileInfo],
    moduleBuildVersions :: !DependencyVersions,
    moduleBuildCapiOptions :: !CapiStubOptions,
    moduleBuildPrimIdentity :: !PackageId,
    -- | The place of the package among the packages of the graph, which
    -- orders its tasks against theirs.
    moduleBuildOrder :: !Int,
    moduleBuildPartitionTask :: !TaskId,
    moduleBuildFinishTask :: !TaskId,
    -- | The tasks the partition waits on besides the parse tasks: the
    -- partitions of the dependencies, whose units it links to.
    moduleBuildPartitionAfter :: ![TaskId],
    moduleBuildFinishAfter :: ![TaskId],
    -- | The dependencies, read by the partition task.
    moduleBuildDependencies :: !(IO [PreparedDependency]),
    -- | Takes the unit of each compiled module, before the partition task ends.
    moduleBuildPartitioned :: !(Map.Map Text UnitRuntime -> IO ()),
    moduleBuildFinished :: !(CompiledPackageModules -> IO ())
  }

-- | A dependency of a module build, as its partition task sees it.
data PreparedDependency = PreparedDependency
  { dependencyPackage :: !Package,
    dependencyExposed :: !(Set.Set Text),
    dependencySource :: !PackageSource,
    -- | The unit of each module, for a dependency that compiles in the
    -- same graph; empty for one the store holds.
    dependencyUnits :: !(Map.Map Text UnitRuntime),
    dependencyLocator :: !PackageLocator
  }

-- | Add the parse tasks and the partition task of a module build.
addModuleBuild :: TaskGraph -> IORef BackendPhaseTimings -> ModuleBuild -> IO ()
addModuleBuild graph phaseTimings build = do
  let config = moduleBuildConfig build
      files = moduleBuildFiles build
      order = moduleBuildOrder build
  compileVerbose config ("Parse " <> show (length files) <> " modules")
  progressReport (compileProgress config) (ProgressBuild (moduleBuildItem build) (length files))
  sourceSlots <- mapM (const newEmptyTMVarIO) files
  parseBase <- allocateTaskIds graph (length files)
  let parseTasks =
        [ Task
            { taskId = TaskId (parseBase + index),
              taskKind = TaskParse,
              taskOrder = order,
              taskDependencies = Set.empty,
              taskAction = do
                source <- parseSource (compileHeaderDirectory config) (moduleBuildPackageRoot build) (moduleBuildVersions build) file
                -- The header fields of the module are strict, so the import
                -- list is known once the source exists. The tree is forced
                -- here, in the parse task, and not by the first task that
                -- reads it.
                modu <- readMVar (sourceModuleParsed source)
                evaluate (rnf (modu, sourceModuleParseDiagnostics source))
                atomically (putTMVar slot source)
            }
        | (index, file, slot) <- zip3 [0 ..] files sourceSlots
        ]
      partitionTask =
        Task
          { taskId = moduleBuildPartitionTask build,
            taskKind = TaskPackage,
            taskOrder = order,
            taskDependencies = Set.fromList (map taskId parseTasks <> moduleBuildPartitionAfter build),
            taskAction = partitionModules graph phaseTimings build sourceSlots
          }
  addTasks graph (parseTasks <> [partitionTask])

-- | Cut the parsed modules into units and add the tasks of each unit and
-- the finish task. A unit waits on the units of its own package it
-- imports, and on the units of the dependencies that hold the modules it
-- imports.
partitionModules :: TaskGraph -> IORef BackendPhaseTimings -> ModuleBuild -> [TMVar SourceModule] -> IO ()
partitionModules graph phaseTimings build sourceSlots = do
  let config = moduleBuildConfig build
      resolvePackage = moduleBuildPackage build
      order = moduleBuildOrder build
      noCode = compileNoCode config
  parsed <- mapM (atomically . readTMVar) sourceSlots
  dependencies <- moduleBuildDependencies build
  units <- evaluate (sourceModuleUnits parsed)
  -- The graph this task builds is which units there are and which units
  -- each waits on. The modules in them are its input, forced when they
  -- were parsed; forcing them here would only move that work out of the
  -- parse tasks that run in parallel.
  _ <- evaluate (force [(sourceUnitId unit, sourceUnitDependencies unit) | unit <- units])
  compileVerbose config ("Compute " <> show (length units) <> " SCC units")
  provider <-
    newModuleProvider
      (Map.unions (map dependencyLocator dependencies))
      [(dependencyPackage dependency, Set.toAscList (dependencyExposed dependency), dependencySource dependency) | dependency <- dependencies]
  unitBase <- allocateTaskIds graph (3 * length units)
  runtimes <-
    forM (zip [0 ..] units) $ \(index, unit) ->
      UnitRuntime unit (TaskId (unitBase + 3 * index)) (TaskId (unitBase + 3 * index + 1)) (TaskId (unitBase + 3 * index + 2))
        <$> newEmptyTMVarIO
        <*> newEmptyTMVarIO
        <*> newEmptyTMVarIO
        <*> newEmptyTMVarIO
  let runtimeMap = Map.fromList [(sourceUnitId (runtimeUnit runtime), runtime) | runtime <- runtimes]
      context =
        PackageTaskContext
          { taskModuleCompileConfig = config,
            taskStorePath = moduleBuildOutputRoot build,
            taskResolvePackage = resolvePackage,
            taskPrimIdentity = moduleBuildPrimIdentity build,
            taskPackageRoot = moduleBuildPackageRoot build,
            taskModuleProvider = provider,
            taskCapiStubOptions = moduleBuildCapiOptions build,
            taskBackendPhaseTimings = phaseTimings
          }
      localRuntimes unit = map (lookupRuntime runtimeMap) (sourceUnitDependencies unit)
      -- The units of the dependencies that hold the modules the unit
      -- imports, as the provider finds them.
      importedRuntimes unit =
        [ runtime
        | name <- unitExternalNames unit,
          dependency <- dependencies,
          name `Set.member` dependencyExposed dependency,
          Just runtime <- [Map.lookup name (dependencyUnits dependency)]
        ]
      unitExternalNames unit =
        let sources = sourceUnitSources unit
            names = map sourceName sources
         in [name | name <- nub (concatMap sourceDependencyNames sources <> wiredInterfaceModules), name `notElem` names]
      -- The type-check task of a unit says that the package compiles, and
      -- the last task of a unit reports its modules as compiled.
      reportCompiling = progressReport (compileProgress config) (ProgressCompile (moduleBuildItem build))
      reportCompiled unit = progressReport (compileProgress config) (ProgressModules (moduleBuildItem build) (length (sourceUnitSources unit)))
      unitTasks runtime =
        let unit = runtimeUnit runtime
            unitOrder = order * 1000000 + sourceUnitOrder unit
            below = localRuntimes unit <> importedRuntimes unit
         in [ Task
                { taskId = runtimeResolveTask runtime,
                  taskKind = TaskResolve,
                  taskOrder = unitOrder,
                  taskDependencies = Set.fromList (map runtimeResolveTask below),
                  taskAction = runResolveUnit context runtimeMap runtime
                },
              Task
                { taskId = runtimeTypeTask runtime,
                  taskKind = TaskTypeCheck,
                  taskOrder = unitOrder,
                  taskDependencies = Set.fromList (runtimeResolveTask runtime : map runtimeTypeTask below),
                  taskAction = reportCompiling >> runTypeUnit context runtimeMap runtime >> when noCode (reportCompiled unit)
                }
            ]
              <> [ Task
                     { taskId = runtimeBackendTask runtime,
                       taskKind = TaskBackend,
                       taskOrder = negate (sum (map sourceModuleSize (sourceUnitSources unit))),
                       taskDependencies = Set.singleton (runtimeTypeTask runtime),
                       taskAction = runBackendUnit context runtime >> reportCompiled unit
                     }
                 | not noCode
                 ]
      finishTask =
        Task
          { taskId = moduleBuildFinishTask build,
            taskKind = TaskPackage,
            taskOrder = order,
            taskDependencies =
              Set.fromList
                ( concat [runtimeTypeTask runtime : [runtimeBackendTask runtime | not noCode] | runtime <- runtimes]
                    <> moduleBuildFinishAfter build
                ),
            taskAction = finishModules build runtimes parsed >>= moduleBuildFinished build
          }
  addTasks graph (concatMap unitTasks runtimes <> [finishTask])
  moduleBuildPartitioned build (Map.fromList [(sourceName source, runtime) | runtime <- runtimes, source <- sourceUnitSources (runtimeUnit runtime)])

-- | Collect the results of the units of a module build: report the
-- diagnostics, and write the digests a consumer reads.
finishModules :: ModuleBuild -> [UnitRuntime] -> [SourceModule] -> IO CompiledPackageModules
finishModules build runtimes parsed = do
  let config = moduleBuildConfig build
      outputRoot = moduleBuildOutputRoot build
      packageRoot = moduleBuildPackageRoot build
      resolvePackage = moduleBuildPackage build
  resolveResults <- mapM (atomically . readTMVar . runtimeResolveResult) runtimes
  typeResults <- mapM (atomically . readTMVar . runtimeTypeResult) runtimes
  let parseDiagnostics = concatMap (concatMap sourceModuleParseDiagnostics . sourceUnitSources . runtimeUnit) runtimes
      resolveDiagnostics = concatMap resolveUnitErrors resolveResults
      -- An unlocated diagnostic names the modules of its unit.
      typeDiagnostics =
        concat
          [ [(unitLabel (runtimeUnit runtime), diagnostic) | diagnostic <- typeUnitDiagnostics result, diagSeverity diagnostic == TcError]
          | (runtime, result) <- zip runtimes typeResults
          ]
  frontendFailure <- renderFrontendFailure (excerptSourceLoader (compileHeaderDirectory config) packageRoot (moduleBuildVersions build) (moduleBuildFiles build)) parseDiagnostics resolveDiagnostics typeDiagnostics
  unless (null frontendFailure) (ioError (userError frontendFailure))
  let localScopeHashes = byModuleUnions (map resolveUnitScopeHashes resolveResults)
      localTypeHashes = byModuleUnions (map typeUnitHashes typeResults)
      localFactsDigests =
        byModuleUnions
          [ ownModules resolvePackage (Map.fromList [(sourceName source, typeUnitFactsDigest result) | source <- sourceUnitSources (runtimeUnit runtime)])
          | (runtime, result) <- zip runtimes typeResults
          ]
      factsArtifacts =
        Map.fromList
          [ (sourceName source, unitFactsPath (runtimeUnit runtime))
          | runtime <- runtimes,
            source <- sourceUnitSources (runtimeUnit runtime)
          ]
  -- A consumer takes the digests from here rather than encoding the
  -- interfaces again, and finds the facts artifact of each module here.
  writeStamp
    (packageDigestsPath outputRoot)
    PackageDigests
      { packageDigestsModules =
          Map.fromList
            [ (sourceName source, ModuleDigests scopeDigest typeDigest factsDigest factsArtifact)
            | source <- parsed,
              Just scopeDigest <- [lookup resolvePackage (byModuleLookupName (sourceName source) localScopeHashes)],
              Just typeDigest <- [lookup resolvePackage (byModuleLookupName (sourceName source) localTypeHashes)],
              Just factsDigest <- [lookup resolvePackage (byModuleLookupName (sourceName source) localFactsDigests)],
              Just factsArtifact <- [Map.lookup (sourceName source) factsArtifacts]
            ]
      }
  pure
    CompiledPackageModules
      { compiledSources = parsed,
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
          | ModuleKey (Package dependencyName dependencyIdentity) _ <- moduleExportKeys dependencyExports,
            dependencyName == "aihc-prim"
          ]

-- | The identity of the primitive package among the dependencies of a
-- package, as 'packagePrimIdentity' finds it among the exports.
dependencyPrimIdentity :: Package -> [InstalledPackage] -> PackageId
dependencyPrimIdentity resolvePackage dependencies =
  fromMaybe (PackageId "aihc-prim") $
    if packageName resolvePackage == "aihc-prim"
      then Just (packageId resolvePackage)
      else
        listToMaybe
          [ PackageId (packageManifestUnitId (installedManifest dependency))
          | dependency <- dependencies,
            installedName dependency == "aihc-prim"
          ]

-- | The directory name and unit identity of a package in the store.
--
-- The fingerprint is a function of the plan: the package name and version,
-- the cabal flags and revision the plan decided, the compiler, the target,
-- and the identities of the dependencies. It does not read the sources, so
-- a consumer computes it without them. A Hackage release never changes,
-- and a core library is identified by the compiler it ships with.
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
      usesSysroot = isJust (inputConfigureScript inputs) || usesPreprocessor || not (compileNoCode config || (null (HackageCabal.cCompileSources cInputs) && null (HackageCabal.cCompileCxxSources cInputs)))
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
                  : packageFlagsKey inputs
                  : sortOn id (map installedIdentity dependencies)
              )
          )
  pure (T.unpack unitIdentity <> "-" <> take 16 fingerprint, unitIdentity)

-- | The cabal flags the plan decided and the revision it read, so that a
-- package built with a flag on and with it off are two store entries.
packageFlagsKey :: PackageInputs -> Text
packageFlagsKey inputs =
  T.pack
    ( show
        ( sortOn fst [(unFlagName flag, value) | (flag, value) <- unFlagAssignment (HackageCabal.contextFlags (inputContext inputs))],
          inputRevision inputs
        )
    )

-- | The directory name and unit identity of a package in a build directory.
localPackageIdentity :: PackageInputs -> (FilePath, Text)
localPackageIdentity inputs =
  let (unitIdentity, _, _) = packageUnitIdentity inputs
   in (T.unpack unitIdentity, unitIdentity)

packageUnitIdentity :: PackageInputs -> (Text, Text, Text)
packageUnitIdentity inputs =
  let packageNameText = HackagePackage.packageNameText (packageNameOf (inputDescription inputs))
      packageVersionText = T.pack (showVersion (Cabal.packageVersion (inputDescription inputs)))
   in (packageNameText <> "-" <> packageVersionText, packageNameText, packageVersionText)

-- | What the package archive depends on besides the module objects.
archiveInputsHash :: ModuleCompileConfig -> FilePath -> [InstalledPackage] -> PackageInputs -> String -> IO String
archiveInputsHash config root dependencies inputs headerHash = do
  let cInputs = inputCCompileInfo inputs
  lirSources <- lirSourceFiles (HackageCabal.cCompileLirSources cInputs)
  sourceHash <- sourceFilesHash root (inputCabalFile inputs : HackageCabal.cCompileSources cInputs <> HackageCabal.cCompileCxxSources cInputs <> lirSources)
  cSysrootArguments <-
    if null (HackageCabal.cCompileSources cInputs) && null (HackageCabal.cCompileCxxSources cInputs)
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
                : T.pack headerHash
                : sortOn id (map installedIdentity dependencies)
            )
        )
    )

-- | Every file the Lir sources of a package depend on: the sources the
-- Cabal file names and the files their includes reach. A unit that is only
-- included is named by no field, so nothing else would fingerprint it, and
-- an edit to it would leave a stale store entry behind.
--
-- A source that does not parse is left to the compile step, which reports
-- it properly. Hashing the file itself is right in the meantime: it is what
-- the expansion would have read first.
lirSourceFiles :: [FilePath] -> IO [FilePath]
lirSourceFiles sources = concat <$> mapM expand sources
  where
    expand source = do
      exists <- doesFileExist source
      if not exists
        then pure [source]
        else do
          result <- Lir.loadModuleWithIncludes source
          pure (source : either (const []) snd result)

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

-- | An installed package, from its manifest. The modules it holds are
-- read on request through a 'ModuleProvider'.
loadInstalledPackage :: Bool -> FilePath -> IO InstalledPackage
loadInstalledPackage immutable storePath = do
  manifestResult <- readPackageManifest (packageManifestPath storePath)
  manifest <- either (ioError . userError . ("Invalid installed package manifest: " <>)) pure manifestResult
  pure
    InstalledPackage
      { installedResult = InstallResult storePath [] (packageManifestModules manifest),
        installedName = packageManifestName manifest,
        installedVersion = packageManifestVersion manifest,
        installedIdentity = packageManifestIdentity manifest,
        installedImmutable = immutable,
        installedManifest = manifest
      }

-- | The keys of the modules a unit imports from the dependency packages:
-- every package that exposes a module of an imported name, since the
-- resolver sees them all and decides between them.
externalModuleKeys :: ModuleProvider -> [Text] -> [Text] -> [ModuleKey]
externalModuleKeys provider unitNames dependencyNames =
  [ ModuleKey package name
  | name <- dependencyNames,
    name `notElem` unitNames,
    package <- providerPackagesOf provider name
  ]

-- | The resolve facts of the modules a unit imports from the dependency
-- packages, read now if no unit read them before.
externalResolvedFacts :: ModuleProvider -> [Text] -> [Text] -> IO [(ModuleKey, ResolvedModuleFacts)]
externalResolvedFacts provider unitNames dependencyNames =
  forM (externalModuleKeys provider unitNames dependencyNames) $ \key ->
    (,) key <$> providerResolved provider key

-- | The type facts of the modules a unit imports from the dependency
-- packages.
externalTypedFacts :: ModuleProvider -> [Text] -> [Text] -> IO [(ModuleKey, TypedModuleFacts)]
externalTypedFacts provider unitNames dependencyNames =
  forM (externalModuleKeys provider unitNames dependencyNames) $ \key ->
    (,) key <$> providerTyped provider key

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
    ResolveError Nothing name namespace message ->
      "error: " <> renderResolveMessage message name namespace
    ResolveError (Just sourceSpan) name namespace message ->
      renderResolveLocation sourceSpan
        <> ": error: "
        <> renderResolveMessage message name namespace
        <> renderResolveExcerpt sourceLines sourceSpan

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
      ( [sourceSpan | ResolveError (Just sourceSpan) _ _ _ <- resolveDiagnostics]
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
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
  externalResolved <- externalResolvedFacts (taskModuleProvider context) unitNames dependencyNames
  let dependencyExports = moduleExportsFromList [(key, resolvedModuleScope facts) | (key, facts) <- externalResolved]
      dependencyScopeHashes = byModuleFromList [(key, resolvedModuleScopeDigest facts) | (key, facts) <- externalResolved]
      availableExports = mconcat (map resolveUnitExports dependencyResults) <> dependencyExports
      availableScopeHashes = byModuleUnions (map resolveUnitScopeHashes dependencyResults ++ [dependencyScopeHashes])
      scopeInputs = dependencyInputs "scope:" unitNames dependencyNames availableScopeHashes
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      inputs = sortOn fst (sourceHashes <> scopeInputs)
      resolvePath source = sourceModuleDirectory source </> "resolve.cbor"
      stampPath = storePath </> unitResolveStampPath unit
      parseSuccess = all (null . sourceModuleParseDiagnostics) sources
      dependenciesSucceeded = all resolveUnitSuccess dependencyResults && all (resolvedModuleSuccess . snd) externalResolved
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
              resolveUnitScopeHashes = ownModules resolvePackage scopeHashes,
              resolveUnitErrors = [],
              resolveUnitSuccess = True
            },
          TypeInputParsed packageModules
        )
    Nothing -> do
      let unitExports = collectModuleExportsWithDeps availableExports packageModules
          visibleExports = unitExports <> availableExports
          builtinScope = builtinFunctionScope resolvePackage visibleExports
          (resolved, errors) =
            case resolveUnit builtinScope visibleExports packageModules of
              Right resolvedUnit' -> (resolvedUnit', [])
              Left failure -> (ResolvedUnit (failureModules failure), failureErrors failure)
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
              resolveUnitScopeHashes = ownModules resolvePackage scopeHashes,
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
                    ( moduleExportsFromList [(ModuleKey resolvePackage (resolveArtifactModuleName artifact), resolveArtifactExports artifact) | artifact <- artifacts],
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
      provider = taskModuleProvider context
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
  externalResolved <- externalResolvedFacts provider unitNames dependencyNames
  externalTyped <- externalTypedFacts provider unitNames dependencyNames
  -- The instances a unit sees from the dependency packages come from the
  -- providers of the modules it imports, which can be in packages below
  -- the dependencies.
  externalInstanceInterface <- providerInstanceFacts provider (Set.unions [typedModuleInstanceProviders facts | (_, facts) <- externalTyped])
  let dependencyExports = moduleExportsFromList [(key, resolvedModuleScope facts) | (key, facts) <- externalResolved]
      dependencyScopeHashes = byModuleFromList [(key, resolvedModuleScopeDigest facts) | (key, facts) <- externalResolved]
      dependencyTypes = byModuleFromList [(key, typedModuleInterface facts) | (key, facts) <- externalTyped]
      dependencyTypeHashes = byModuleFromList [(key, typedModuleTypeDigest facts) | (key, facts) <- externalTyped]
      dependencyFactsDigests = byModuleFromList [(key, typedModuleFactsDigest facts) | (key, facts) <- externalTyped]
      availableTypes = byModuleUnions (map typeUnitTypes dependencyResults ++ [dependencyTypes])
      availableTypeHashes = byModuleUnions (map typeUnitHashes dependencyResults ++ [dependencyTypeHashes])
      availableExports = mconcat (map resolveUnitExports dependencyResolveResults) <> dependencyExports
      availableScopeHashes = byModuleUnions (map resolveUnitScopeHashes dependencyResolveResults ++ [dependencyScopeHashes])
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      scopeInputs = dependencyInputs "scope:" unitNames dependencyNames availableScopeHashes
      typeInputs = dependencyInputs "type:" unitNames dependencyNames availableTypeHashes
      -- The instances a unit sees come from every unit below it and from
      -- the dependency packages that supply an imported module. A facts
      -- digest covers the facts of the units below the one it names.
      factsInputs =
        [ ("facts:" <> unitLabel (runtimeUnit (lookupRuntime runtimes dependency)), typeUnitFactsDigest result)
        | (dependency, result) <- zip (sourceUnitDependencies unit) dependencyResults
        ]
      -- The facts of a dependency package reach the unit through the units
      -- that hold the modules it imports, so their digests are its inputs.
      packageFactsInputs = dependencyInputs "facts:" unitNames dependencyNames dependencyFactsDigests
      inputs =
        sortOn fst $
          sourceHashes
            <> scopeInputs
            <> typeInputs
            <> factsInputs
            <> packageFactsInputs
            <> [ ("options:frontend", T.pack (frontendOptionsKey config)),
                 ("options:extensions", T.pack (show (map sourceModuleExtensions sources)))
               ]
      typePath source = sourceModuleDirectory source </> "type.cbor"
      factsPath = unitFactsPath unit
      stampPath = storePath </> unitStampPath unit
      frontendFiles = factsPath : map typePath sources
      -- Each dependency carries the instance closure of its own dependencies,
      -- so the closures agree wherever they overlap.
      importedInstanceInterface =
        mergeTcInterfaces
          TrustMergedFacts
          (externalInstanceInterface : map typeUnitInstanceInterface dependencyResults)
      -- Every package that holds an imported module contributes its
      -- interface: the keys of the facts carry the package, so the
      -- interfaces of two packages with a module of one name do not
      -- collide.
      importedTypes =
        mergeTcInterfaces
          (configMergeCheck config)
          ( importedInstanceInterface
              : [ interface
                | name <- dependencyNames,
                  name `notElem` unitNames,
                  (_, interface) <- byModuleLookupName name availableTypes
                ]
          )
      checkUnit = do
        let resolved =
              case typeInput of
                TypeInputResolved result -> result
                TypeInputParsed packageModules ->
                  let visibleExports = collectModuleExportsWithDeps availableExports packageModules <> availableExports
                   in -- The unit resolved when its artifacts were written, and the
                      -- same inputs resolve the same way.
                      fromRight (ResolvedUnit []) (resolveUnit (builtinFunctionScope resolvePackage visibleExports) visibleExports packageModules)
            checked =
              typecheckModuleSccWithInterface
                (primTcConfig primIdentity)
                importedTypes
                (resolvedModules resolved)
            checkedDiagnostics = concatMap tcModuleDiagnostics (fst checked)
        _ <- evaluate (length checkedDiagnostics)
        pure (checked, checkedDiagnostics)
      dependencySuccess = all typeUnitSuccess dependencyResults && all (typedModuleSuccess . snd) externalTyped
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
                  typeUnitInstanceProviders = Set.empty,
                  typeUnitDiagnostics = [],
                  typeUnitWritten = Set.empty,
                  typeUnitReused = Set.empty,
                  typeUnitPendingStamp = Nothing,
                  typeUnitSuccess = False
                }
            putTMVar (runtimeBackendInput runtime) Nothing
    Just recorded -> do
      artifacts <- mapM (readTypeArtifactFile . (storePath </>) . typePath) sources
      factsArtifact <- readTypeArtifactFile (storePath </> factsPath)
      let ownFacts = typeArtifactInterface factsArtifact
          providers = Set.fromList (concat (Map.elems (typeArtifactInstanceProviders factsArtifact)))
          interfaces = map typeArtifactInterface artifacts
      verbose ("Reuse type and backend artifacts: " <> T.unpack (unitLabel unit))
      atomically $ do
        putTMVar
          (runtimeTypeResult runtime)
          TypeUnitResult
            { typeUnitTypes = ownModules resolvePackage (Map.fromList (zip unitNames interfaces)),
              typeUnitHashes = ownModules resolvePackage (unitStampTypes recorded),
              typeUnitOwnInstanceInterface = ownFacts,
              typeUnitFactsDigest = unitStampFacts recorded,
              typeUnitInstanceInterface = mergeTcInterfaces TrustMergedFacts [importedInstanceInterface, ownFacts],
              typeUnitInstanceProviders = providers,
              typeUnitDiagnostics = [],
              typeUnitWritten = Set.empty,
              typeUnitReused = Set.fromList unitNames,
              typeUnitPendingStamp = Nothing,
              typeUnitSuccess = True
            }
        putTMVar (runtimeBackendInput runtime) Nothing
    Nothing -> do
      ((checkedModules, newInterface), diagnostics) <- checkUnit
      let checkedInterface = shareTcInterface newInterface
          completeInterface = mergeTcInterfaces (configMergeCheck config) [importedTypes, checkedInterface]
          ownInstanceInterface = addReferencedFacts (typeLiteralKindTyCons (primKinds primIdentity)) (typeLiteralSupportTerms primIdentity) completeInterface (instanceFacts checkedInterface)
          unitTypes = map (moduleTypeInterface (primKinds primIdentity) (typeLiteralSupportTerms primIdentity) (resolveUnitExports resolvedOutput) resolvePackage completeInterface) sources
          completeInstanceInterface = mergeTcInterfaces TrustMergedFacts [importedInstanceInterface, ownInstanceInterface]
          instanceProviders = interfaceInstanceProviders completeInstanceInterface
          typeSuccess = not (any ((== TcError) . diagSeverity) diagnostics)
          success = resolveSuccess && dependencySuccess && typeSuccess
      (ownTypeHashes, factsDigest) <-
        if success
          then do
            typeHashes <- Map.fromList <$> zipWithM (writeTypeArtifact verbose ((storePath </>) . typePath)) sources unitTypes
            -- The artifact names the providers of each module of the unit,
            -- so a consumer finds the facts the module sees without the
            -- closure in memory.
            let factsBytes = encodeTypeArtifact (TypeArtifact "$unit" (Map.fromList [(name, Set.toAscList instanceProviders) | name <- unitNames]) ownInstanceInterface)
            createDirectoryIfMissing True (takeDirectory (storePath </> factsPath))
            BL.writeFile (storePath </> factsPath) factsBytes
            -- The facts digest covers the facts digests of the units below
            -- this one, so it changes with any of them.
            pure (typeHashes, T.pack (stableHash [BL.toStrict factsBytes, BS8.pack (show (sortOn fst (factsInputs <> packageFactsInputs)))]))
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
            { typeUnitTypes = ownModules resolvePackage (Map.fromList (zip unitNames unitTypes)),
              typeUnitHashes = ownModules resolvePackage ownTypeHashes,
              typeUnitOwnInstanceInterface = ownInstanceInterface,
              typeUnitFactsDigest = factsDigest,
              typeUnitInstanceInterface = completeInstanceInterface,
              typeUnitInstanceProviders = instanceProviders,
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
wiredInterfaceModules = wiredTypeModules <> ["GHC.IsList"] <> wiredDerivingModules

-- | The scope of the functions that desugaring reaches without an import.
-- The argument is everything the unit can see, as 'resolveUnit' takes it.
builtinFunctionScope :: Package -> ModuleExports -> Builtins
builtinFunctionScope currentPackage visibleExports =
  builtins currentPackage visibleExports builtinFunctionModules
  where
    builtinFunctionModules = ["GHC.IsList", "GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String", "GHC.Types"]

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
      Fc.writeProgramFile path (fcProgram fcModule)
      verbose ("Write FC: " <> T.unpack name)

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
          arguments <- capiStubArguments target (compileOptimization config) lto capiOptions (compileHeaderDirectory config)
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

-- | Run the heap points-to analysis on a GRIN program, apply the rewrites
-- that its result permits, and simplify the program again. The analysis
-- and its rewrites are logged under @--verbose@.
optimizeGrinPointsTo :: (String -> IO ()) -> Text -> Grin.GrinProgram -> IO Grin.GrinProgram
optimizeGrinPointsTo verbose name program = do
  (analysis, analysisNs) <- measureTime (evaluate (Grin.analyzePointsTo program) >>= traverse evaluate)
  case analysis of
    Nothing -> do
      verbose ("points-to GRIN: " <> T.unpack name <> ", skipped: the program holds a form that the analysis does not model")
      pure program
    Just result -> do
      ((optimized, rewrites), rewriteNs) <- measureTime $ do
        let (rewritten, counts) = Grin.rewriteWithPointsTo result program
        finished <- either (ioError . userError . ("GRIN points-to rewrite failed: " <>)) pure (Grin.finishGrinProgram rewritten)
        (,) finished <$> evaluate counts
      verbose (renderPointsToReport name (Grin.pointsToStats result) rewrites analysisNs rewriteNs)
      pure optimized

-- | One log line for the points-to analysis: how long the analysis and the
-- rewrites took, how much work the solver did, and the number of rewrites
-- of each kind.
renderPointsToReport :: Text -> Grin.PointsToStats -> Grin.PointsToRewrites -> Word64 -> Word64 -> String
renderPointsToReport name stats rewrites analysisNs rewriteNs =
  "points-to GRIN: "
    <> T.unpack name
    <> ", analysis "
    <> renderDuration analysisNs
    <> " ("
    <> show (Grin.statsIterations stats)
    <> " iterations, "
    <> show (Grin.statsVariables stats)
    <> " variables, "
    <> show (Grin.statsSetNodes stats)
    <> " set nodes, "
    <> show (Grin.statsLocations stats)
    <> " locations, "
    <> show (Grin.statsSharedLocations stats)
    <> " shared, "
    <> show (Grin.statsSingleEntryThunks stats)
    <> " single-entry thunks, "
    <> show (Grin.statsWidenedNodes stats)
    <> " widened), rewrites "
    <> renderDuration rewriteNs
    <> " ("
    <> show (Grin.rewritesDeadAlternatives rewrites)
    <> " dead alternatives, "
    <> show (Grin.rewritesEvaluatedEvals rewrites)
    <> " evals of values, "
    <> show (Grin.rewritesDirectCalls rewrites)
    <> " direct calls, "
    <> show (Grin.rewritesSingleEntryEvals rewrites)
    <> " single-entry evals)"

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
        -- A @--lto@ build of the LLVM target compiles the program to
        -- bitcode, and the link optimizes it with the runtime; see
        -- 'llvmLtoArguments'.
        let levelArguments = [optimizationArgument (compileOptimization config) | target == Llvm] <> llvmLtoArguments target (compileLto config)
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
      loweredProgram <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram (fcProgram fcModule))
      plainProgram <-
        if compileGrinPointsTo config
          then optimizeGrinPointsTo verbose name loweredProgram
          else pure loweredProgram
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

-- | Public headers stay inside the package when its temporary directory moves.
packageHeaderDirectory :: FilePath -> FilePath
packageHeaderDirectory root = root </> "include"

-- | Search package headers before dependency headers.
appendIncludeDirs :: [FilePath] -> HackageCabal.FileInfo -> HackageCabal.FileInfo
appendIncludeDirs directories file =
  file {HackageCabal.fileInfoIncludeDirs = nub (HackageCabal.fileInfoIncludeDirs file <> directories)}

-- | Use only headers that each dependency installed for this target.
dependencyIncludeDirs :: [InstalledPackage] -> IO [FilePath]
dependencyIncludeDirs dependencies =
  filterM doesDirectoryExist (map (packageHeaderDirectory . installStorePath . installedResult) dependencies)

-- | Header changes in local dependencies invalidate C and hsc2hs outputs.
includeDirectoriesHash :: [FilePath] -> IO String
includeDirectoriesHash directories = do
  files <- concat <$> mapM directoryFiles directories
  sourceFilesHash "" files
  where
    directoryFiles directory = do
      names <- listDirectory directory
      concat
        <$> forM
          names
          ( \name -> do
              let path = directory </> name
              isDirectory <- doesDirectoryExist path
              if isDirectory then directoryFiles path else pure [path]
          )

-- | Copy declared public headers. Generated include directories take precedence.
installPackageHeaders :: FilePath -> FilePath -> HackageCabal.CCompileInfo -> IO ()
installPackageHeaders root storePath info = do
  headers <- forM (HackageCabal.cCompileInstallIncludes info) $ \header -> do
    unless (isRelative header && ".." `notElem` splitDirectories header) $
      ioError (userError ("Install header path is invalid: " <> header))
    candidates <- filterM doesFileExist [directory </> header | directory <- HackageCabal.cCompileIncludeDirs info <> [root]]
    case candidates of
      [] -> ioError (userError ("Install header is absent: " <> header))
      source : _ -> (header,) <$> BS.readFile source
  let output = packageHeaderDirectory storePath
  exists <- doesDirectoryExist output
  when exists (removeDirectoryRecursive output)
  forM_ headers $ \(header, bytes) -> do
    let path = output </> header
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path bytes

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

-- | Compile the @c-sources@, the @cxx-sources@, and the Lir units of a
-- package into its @cbits@ directory. A link takes every object there as it
-- is, so the units of the runtime reach a program whether or not a symbol
-- of theirs is referenced before them.
--
-- A C++ source goes through the same driver as C++, with the @cxx-options@
-- of the package in place of its @cc-options@. The objects need the C++
-- standard library, which the link adds for a package whose manifest says
-- it has C++ sources; a target without that library refuses the package
-- here rather than at the link of every program that depends on it.
--
-- A @--lto@ build of the LLVM target compiles every object here to bitcode,
-- so that the link optimizes the runtime and the C of the packages with the
-- program; see 'llvmLtoArguments'.
compilePackageCFiles :: NativeTarget -> OptimizationLevel -> Bool -> FilePath -> (String -> IO ()) -> FilePath -> FilePath -> HackageCabal.CCompileInfo -> IO [FilePath]
compilePackageCFiles target level lto headerDirectory verbose packageRoot storePath info
  | null (HackageCabal.cCompileSources info) && null (HackageCabal.cCompileCxxSources info) && null (HackageCabal.cCompileLirSources info) = pure []
  | otherwise = do
      (compiler, targetArguments) <- backendCompiler target
      sysrootIncludes <- wasmSysrootIncludeArguments target
      unless (null (HackageCabal.cCompileCxxSources info)) $
        either (ioError . userError) (const (pure ())) (cxxStandardLibraryArguments target)
      let includeArguments =
            sysrootIncludes
              <> ["-I" <> directory | directory <- HackageCabal.cCompileIncludeDirs info]
              <> ["-I" <> headerDirectory]
          ltoArguments = llvmLtoArguments target lto
          objectRoot = storePath </> "cbits"
      createDirectoryIfMissing True objectRoot
      cObjects <- forM (HackageCabal.cCompileSources info) $ \source -> do
        exists <- doesFileExist source
        unless exists (ioError (userError ("C source is absent: " <> source)))
        let object = objectRoot </> cObjectFileName (makeRelative packageRoot source)
        verbose ("Compile C source: " <> source)
        runTool
          compiler
          ( targetArguments
              <> handwrittenCArguments level
              <> ltoArguments
              <> HackageCabal.cCompileCcOptions info
              <> handwrittenCOverrideArguments level
              <> includeArguments
              <> ["-c", source, "-o", object]
          )
        pure object
      cxxObjects <- forM (HackageCabal.cCompileCxxSources info) $ \source -> do
        exists <- doesFileExist source
        unless exists (ioError (userError ("C++ source is absent: " <> source)))
        let object = objectRoot </> cObjectFileName (makeRelative packageRoot source)
        verbose ("Compile C++ source: " <> source)
        runTool
          compiler
          ( targetArguments
              <> handwrittenCArguments level
              <> ltoArguments
              <> HackageCabal.cCompileCxxOptions info
              <> handwrittenCOverrideArguments level
              <> includeArguments
              <> ["-x", "c++", "-c", source, "-o", object]
          )
        pure object
      lirObjects <- forM (HackageCabal.cCompileLirSources info) $ \source -> do
        exists <- doesFileExist source
        unless exists (ioError (userError ("Lir source is absent: " <> source)))
        lirModule <- either (ioError . userError . Lir.renderLoadError) pure =<< Lir.loadModule source
        -- A unit of constants alone is there to be included by the others
        -- and has no object.
        if lirModuleDefinesCode lirModule
          then do
            let object = objectRoot </> cObjectFileName (makeRelative packageRoot source)
            verbose ("Compile Lir source: " <> source)
            compileLirObject lto target (dropExtension (takeFileName object)) lirModule objectRoot object
            pure (Just object)
          else pure Nothing
      pure (cObjects <> cxxObjects <> catMaybes lirObjects)

-- | Run the configure script of a @build-type: Configure@ package, and
-- return the directory that holds its outputs. A package without a script
-- has no such directory.
--
-- Cabal runs the script in the package directory, so the generated headers
-- land beside their templates. Here the source tree is shared by every
-- target -- a Hackage release is unpacked once into the cache -- while the
-- answers configure finds are per target, so the script runs out of tree.
-- Autoconf supports this: the outputs of @AC_CONFIG_HEADERS@ and
-- @AC_CONFIG_FILES@ are written relative to the working directory and
-- @srcdir@ is derived from the script path.
--
-- The directory is under the cache root, and its name is the package and
-- the hash of what the outputs depend on. An earlier run with the same
-- hash wrote a stamp, and then the script does not run again. A lock file
-- beside the directory stops two installs that share the cache root from
-- running the same script in the same directory.
--
-- The script sees the C compiler of the target, so its feature tests answer
-- for the target rather than the host.
runConfigureScript :: ModuleCompileConfig -> FilePath -> PackageInputs -> IO (Maybe FilePath)
runConfigureScript config cacheRoot inputs =
  forM (inputConfigureScript inputs) $ \script -> do
    inputsHash <- configureInputsHash config script
    let (package, _, _) = packageUnitIdentity inputs
        directory = cacheRoot </> (T.unpack package <> "-" <> take 16 inputsHash)
        stampPath = directory </> "configure.hash"
    createDirectoryIfMissing True cacheRoot
    withFileLock (directory <.> "lock") $ do
      previous <- readStampText stampPath
      if previous == Just inputsHash
        then verbose ("Reuse configure: " <> directory)
        else do
          (executable, arguments, environment) <- configureCommand (compileTarget config) (compileOptimization config) script
          exists <- doesDirectoryExist directory
          when exists (removeDirectoryRecursive directory)
          createDirectoryIfMissing True directory
          verbose ("Configure: " <> unwords (executable : arguments))
          runToolIn directory environment executable arguments
          BS8.writeFile stampPath (BS8.pack inputsHash)
    pure directory
  where
    verbose = compileVerbose config

-- | Run an action while this process holds the lock file. Another process
-- that asks for the same lock waits until the action ends.
withFileLock :: FilePath -> IO a -> IO a
withFileLock path action =
  withFile path ReadWriteMode $ \handle ->
    bracket_ (HandleLock.hLock handle HandleLock.ExclusiveLock) (HandleLock.hUnlock handle) action

-- | Return the sources and C inputs of a package with the outputs of its
-- configure script in their include paths.
--
-- Every include directory of the package gets a counterpart under the
-- configure directory that is searched first, which is how the generated
-- headers reach both the CPP pass over the Haskell sources and the C
-- compiles. A @<package>.buildinfo@ the script writes is merged the way
-- Cabal merges it.
configurePackage :: FilePath -> Text -> PackageInputs -> Maybe FilePath -> IO ([HackageCabal.FileInfo], HackageCabal.CCompileInfo)
configurePackage root packageName inputs configured =
  case configured of
    Nothing -> pure (files, cInfo)
    Just buildDirectory -> do
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
      (files', cInfo') <-
        either (ioError . userError) pure $
          HackageCabal.applyHookedBuildInfo
            (Cabal.cabalVersion (inputDescription inputs))
            buildDirectory
            hooked
            (map (HackageCabal.prependIncludeDirs generatedDirs) files)
            cInfo {HackageCabal.cCompileIncludeDirs = nub (generatedDirs <> HackageCabal.cCompileIncludeDirs cInfo)}
      let searchDirs = nub (concatMap HackageCabal.fileInfoIncludeDirs files' <> HackageCabal.cCompileIncludeDirs cInfo')
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
--
-- The files are independent. Thus this function preprocesses them in
-- parallel, with a maximum of one tool for each capability. Each tool runs
-- the C compiler, and a package such as @unix@ has dozens of @.hsc@ files.
preprocessPackage :: ModuleCompileConfig -> DependencyVersions -> FilePath -> FilePath -> Maybe FilePath -> String -> HackageCabal.CCompileInfo -> [HackageCabal.FileInfo] -> IO [HackageCabal.FileInfo]
preprocessPackage config versions root storePath configureScript headerHash cInfo files = do
  capabilities <- getNumCapabilities
  limit <- newQSem (max 1 capabilities)
  mapConcurrently (bracket_ (waitQSem limit) (signalQSem limit) . preprocessFile) files
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
                    BS8.pack headerHash,
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
              environment <- preprocessorEnvironment
              runToolIn (takeDirectory output) environment executable arguments
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

-- | The arguments Cabal would give hsc2hs.
--
-- When the code of the target runs on the host, hsc2hs runs in its native
-- mode, as Cabal runs it: it compiles one program for the file, runs that
-- program, and the program writes the module. This is fast, and the
-- program sees every @#include@ of the file before any condition, as GHC
-- sees them. For example, the export list of a @unix@ module tests
-- @B7200@ before the module includes @termios.h@.
--
-- For any other target, hsc2hs runs in cross-compilation mode. In that
-- mode hsc2hs finds every constant by compiling test programs with the C
-- compiler of the target and never runs one. It compiles one test program
-- for each condition and for each constant, and it tests a condition with
-- only the text above it. Thus a file the size of
-- @System.Posix.Terminal.Common@ costs about 275 compiler runs.
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
    ( [flag | not (targetRunsOnHost target), flag <- ["--cross-compile", "--via-asm"]]
        <> ["--cc=" <> compiler, "--ld=" <> compiler]
        <> map ("--cflag=" <>) (cflags <> options <> hostPlatformMacros target <> ["-include", macrosPath])
        <> map ("-I" <>) includeDirs
        <> ["-o", output, input]
    )

-- | Whether the code of a target runs on the machine that aihc runs on. The
-- LLVM target is always that machine.
targetRunsOnHost :: NativeTarget -> Bool
targetRunsOnHost target = target == Llvm || Just target == hostNativeTarget

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
  environment <- preprocessorEnvironment
  version <- readCreateProcess (proc executable ["--version"]) {env = Just environment} ""
  pure (stableHash [BS8.pack path, BS8.pack version])

-- | The environment a preprocessor runs in: that of aihc, without the RTS
-- options meant for aihc itself. hsc2hs is a GHC program too, and one that
-- is not built with @-threaded@, so a @GHCRTS=-N@ that aihc is given would
-- stop it at start-up before it reads its arguments.
preprocessorEnvironment :: IO [(String, String)]
preprocessorEnvironment = filter ((/= "GHCRTS") . fst) <$> getEnvironment

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
    then pure (HookedBuildInfo Nothing Map.empty)
    else do
      bytes <- BS.readFile path
      case parseValue (parseHookedBuildInfo bytes) of
        Right value -> pure value
        Left errors -> ioError (userError ("Failed to parse " <> path <> ": " <> show errors))

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
buildLibraryArchive target verbose archive objects = do
  createDirectoryIfMissing True (takeDirectory archive)
  archiveExists <- doesFileExist archive
  when archiveExists (removeFile archive)
  archiver <- backendArchiver target
  -- BSD ar refuses to create an archive with no members, and a package whose
  -- modules are all empty standins (aihc-internal) has none: 'moduleObjectPaths'
  -- leaves out their empty objects. Every archive format begins with the
  -- same global header, and an archive that stops there is a valid empty
  -- archive for GNU ld, lld and wasm-ld. 'archiveHasMembers' keeps it away
  -- from ld64.
  if null objects
    then BS.writeFile archive emptyArchive
    else do
      environment <- getEnvironment
      -- Set archive timestamps only in the child process environment.
      let archiveEnvironment = ("ZERO_AR_DATE", "1") : filter ((/= "ZERO_AR_DATE") . fst) environment
      runToolWithEnvironment (Just archiveEnvironment) archiver (["rcs", archive] <> objects)
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

-- Applied to the unit's interface and no more, this gives a function the
-- unit's modules share, so 'addReferencedFacts' prepares its tables once.
moduleTypeInterface :: TcKinds -> [Entity] -> ModuleExports -> Package -> TcInterface -> SourceModule -> TcInterface
moduleTypeInterface kinds supportTerms exports package interface = go
  where
    addReference = addReferencedFacts (typeLiteralKindTyCons kinds) supportTerms interface
    go source =
      addReference
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
        scope = fromMaybe (error "missing resolve scope") (lookupModuleExport (ModuleKey package name) exports)
        scopeTerms = exportedTerms scope
        scopeTypes = exportedTypes scope
        termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems scopeTerms))
        typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems scopeTypes))
        localIdentity identifier = (packageId package, name, identifier)
        localTyCon tyCon = tyConPackageId tyCon == packageId package && tyConModuleName tyCon == name
        visibleTerm key = case key of
          EntityGlobal (GlobalName identifier packageId' moduleName' _) ->
            visibleTermIdentity (packageId', moduleName', identifier)
              || any (visibleTermIdentity . (packageId',moduleName',)) (patSynHelperBase identifier)
          EntityLocal {} -> False
          EntitySyntax -> False
        visibleTermIdentity identity@(_, _, identifier) =
          Map.member identifier scopeTerms
            || identity `Set.member` termIdentities
            || identity `Set.member` methodIdentities
            || identity == localIdentity identifier
        -- The methods of a visible class are visible. An instance defines
        -- them, and a derived instance does so when the scope has the class
        -- but not its methods, as after an export of @Show@ without @(..)@.
        methodIdentities =
          Set.fromList
            [ (PackageId packageIdText', moduleName', method)
            | info <- Map.elems (tcInterfaceClassMap interface),
              visibleClass info,
              Just (packageIdText', moduleName') <- [ciOrigin info],
              (method, _) <- ciMethods info
            ]
        -- The matcher and the builder of a visible pattern synonym are visible.
        patSynHelperBase identifier = mapMaybe (`T.stripPrefix` identifier) ["$m", "$b"]
        visibleTyCon info =
          let tyCon = tciTyCon info
              identity = (tyConPackageId tyCon, tyConModuleName tyCon, tciName info)
              (namespaceScope, namespaceIdentities) =
                case tyConNamespace tyCon of
                  ResolutionNamespaceTerm -> (scopeTerms, termIdentities)
                  ResolutionNamespaceType -> (scopeTypes, typeIdentities)
                  ResolutionNamespaceModule -> (Map.empty, Set.empty)
           in Map.member (tciName info) namespaceScope || identity `Set.member` namespaceIdentities || identity == localIdentity (tciName info)
        visibleTypeIdentity (GlobalName identifier packageId' moduleName' namespace) =
          let identity = (packageId', moduleName', identifier)
           in namespace == ResolutionNamespaceType
                && (Map.member identifier scopeTypes || identity `Set.member` typeIdentities || identity == localIdentity identifier)
        visibleClass info =
          case ciOrigin info of
            Just (packageIdText, moduleName') ->
              let identity = (PackageId packageIdText, moduleName', ciName info)
               in Map.member (ciName info) scopeTypes || identity `Set.member` typeIdentities || identity == localIdentity (ciName info)
            Nothing -> False
        visibleInstance info = iiDictOrigin info == (packageIdText (packageId package), name)
        visibleDataFamilyInstance = localTyCon . dfiiRepresentationTyCon
        visibleTypeFamilyInstance info = any localTyCon (typeTyCons (tfiiLeft info) <> typeTyCons (tfiiRight info))
        resolvedIdentity resolved = case resolved of
          EntityGlobal global -> Just (globalNamePackage global, globalNameModule global, globalNameText global)
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
typeLiteralSupportTerms :: PackageId -> [Entity]
typeLiteralSupportTerms prim =
  [GlobalTerm prim "GHC.Prim.Natural" "naturalFromInteger#"]

-- | Carry into an interface the facts it refers to but does not hold.
-- The selected interface must contain only facts from the complete interface.
--
-- The extra roots are type constructors the module needs that nothing in
-- its own facts names: the kinds of the type-level literals, which a
-- literal refers to without naming.
-- Applying this to the complete interface and no more gives a function the
-- modules of a unit share: they all close over the same facts, and the
-- dependencies of each fact are then found once rather than once per module.
addReferencedFacts :: [TyCon] -> [Entity] -> TcInterface -> TcInterface -> TcInterface
addReferencedFacts extraRoots extraTerms complete = go
  where
    availableTyCons = tcInterfaceTyConMap complete
    availableDataTypes = tcInterfaceDataTypeMap complete
    availableClasses = tcInterfaceClassMap complete
    -- The type constructors that each fact of the complete interface refers
    -- to. The values are thunks, so a fact no module reaches costs its key
    -- alone, and one that many modules reach is walked once for all of them.
    tyConDependencies :: LazyMap.Map GlobalName [TyCon]
    tyConDependencies =
      LazyMap.fromSet
        ( \key ->
            Set.toList
              ( maybe mempty tyConInfoTyCons (Map.lookup key availableTyCons)
                  <> maybe mempty dataTypeInfoTyCons (Map.lookup key availableDataTypes)
                  <> maybe mempty classInfoTyCons (Map.lookup key availableClasses)
              )
        )
        (Map.keysSet availableTyCons <> Map.keysSet availableDataTypes <> Map.keysSet availableClasses)
    closeTyCons found [] = found
    closeTyCons found (tyCon : pending)
      | tyCon `Set.member` found = closeTyCons found pending
      | otherwise =
          let dependencies = LazyMap.findWithDefault [] (tyConKey tyCon) tyConDependencies
           in closeTyCons (Set.insert tyCon found) (dependencies <> pending)
    go interface =
      interface
        { tcInterfaceTermMap = tcInterfaceTermMap interface <> Map.fromList (callStackSupportTerms <> typeableSupportTerms),
          tcInterfaceTyConMap = Map.restrictKeys availableTyCons reachableKeys,
          tcInterfaceDataTypeMap = Map.restrictKeys availableDataTypes reachableKeys,
          tcInterfaceClassMap = Map.restrictKeys availableClasses reachableKeys
        }
      where
        termTyCons = interfaceTermTyCons interface
        -- A use of a function with a HasCallStack constraint desugars to
        -- calls of the call-stack helpers, even when the module does not
        -- import them.
        callStackModules =
          Set.fromList
            [ (tyConPackageId tyCon, tyConModuleName tyCon)
            | tyCon <- Set.toList termTyCons,
              tyConName tyCon == "CallStack"
            ]
        callStackSupportTerms =
          [ (key, scheme)
          | (package', moduleName') <- Set.toList callStackModules,
            identifier <- ["pushCallStack", "emptyCallStack"],
            let key = GlobalTerm package' moduleName' identifier,
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
          termTyCons
            <> interfaceNonTermRootTyCons interface
            <> Set.unions (map (typeSchemeTyCons . snd) callStackSupportTerms)
            <> Set.fromList callStackSupportTyCons
            <> Set.fromList extraRoots
        reachable = closeTyCons Set.empty (Set.toList referenced)
        -- Typeable evidence for an applied type desugars to a call of the
        -- class's @typeRep@ selector on the evidence of each argument. The
        -- class reaches a module as the superclass of one it names, so the
        -- module may hold the class without ever importing the selector.
        typeableSupportTerms =
          [ (key, scheme)
          | tyCon <- Set.toList reachable,
            tyConName tyCon == "Typeable",
            tyConModuleName tyCon `elem` ["Type.Reflection", "Type.Reflection.Internal"],
            let key = GlobalTerm (tyConPackageId tyCon) (tyConModuleName tyCon) "typeRep",
            key `Map.notMember` tcInterfaceTermMap interface,
            Just scheme <- [Map.lookup key (tcInterfaceTermMap complete)]
          ]
        reachableKeys = Set.map tyConKey reachable

writeTypeArtifact :: (String -> IO ()) -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO (Text, Text)
writeTypeArtifact verbose artifactPath source interface = do
  let path = artifactPath source
      name = sourceModuleName source
      (artifactBytes, interfaceBytes) = encodeTypeArtifactParts (TypeArtifact name Map.empty interface)
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path artifactBytes
  verbose ("Write type interface: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict interfaceBytes]))

-- | Write the resolve artifact of a module and return the digest of the
-- scope inside it, taken from the bytes as written.
writeArtifact :: (String -> IO ()) -> ModuleExports -> Package -> FilePath -> SourceModule -> IO (Text, Text)
writeArtifact verbose exports package path source = do
  createDirectoryIfMissing True (takeDirectory path)
  let name = sourceModuleName source
      scope = fromMaybe (error "missing resolve scope") (lookupModuleExport (ModuleKey package name) exports)
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
packageArtifactFormatVersion = "aihc-artifacts-44"
