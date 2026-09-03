module Aihc.Cli.Install
  ( InstallResult (..),
    ModuleCompileConfig (..),
    ModuleCompileRequest (..),
    ModuleCompileResult (..),
    compileModules,
    install,
    parsePackageTarget,
    runInstall,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.PackagePlan
  ( DependencyResolver (..),
    PackagePlan (..),
    ParsedInterfaceFile (ParsedInterfaceFile),
    buildPackagePlanWithResolver,
    localDependencyResolverWithFallback,
    packageSpecFromSource,
    parseInterfaceFile,
    renderHumanDiagnostic,
  )
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifact, encodeResolveScope)
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
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeArtifactParts, encodeTypeInterface)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..))
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Aihc.Llvm qualified as Llvm
import Aihc.Native (NativeTarget (..), backendArchiver, backendCompiler, nativeTargetStoreDirectory)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    Module,
    Name (..),
    SourceSpan (..),
    moduleName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
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
    DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    Pred (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcInterface (..),
    TcSeverity (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    dataTypeKey,
    mergeTcInterfaces,
    patSynKey,
    renderPred,
    renderTcType,
    tcConfig,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Env (PatSynInfo (..), TypeSynonymInfo (..))
import Aihc.Tc.Types (tvKind, tyConModuleName, tyConName, tyConNamespace, tyConPackageId)
import Aihc.Wasm qualified as Wasm
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.DeepSeq (rnf)
import Control.Exception (IOException, bracket, evaluate, throwIO, try)
import Control.Monad (filterM, forM, unless, when, zipWithM)
import Data.Aeson (Value)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf, nub, sortOn)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Version (nullVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showHex)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getFileSize, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import System.IO (hClose, hIsTerminalDevice, openBinaryTempFile, stdout)
import System.Process (readProcessWithExitCode)

data InstallResult = InstallResult
  { installStorePath :: !FilePath,
    installWrittenModules :: ![Text],
    installReusedModules :: ![Text]
  }
  deriving (Eq, Show)

data SourceModule = SourceModule
  { sourceModulePath :: !FilePath,
    sourceModuleSize :: !Int,
    sourceModuleHash :: !Text,
    sourceModuleAst :: Module,
    sourceModuleExtensions :: ![Extension],
    sourceModuleSourceLines :: !(Map.Map FilePath (Map.Map Int Text)),
    sourceModuleParseDiagnostics :: [Value]
  }

data InstalledPackage = InstalledPackage
  { installedResult :: !InstallResult,
    installedExports :: !ModuleExports,
    installedTypes :: !(Map.Map Text TcInterface),
    installedScopeHashes :: !(Map.Map Text Text),
    installedTypeHashes :: !(Map.Map Text Text),
    installedInstanceFacts :: !TcInterface,
    installedInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider))
  }

type InstanceProvider = (PackageId, Text)

data ModuleOutputPaths = ModuleOutputPaths
  { outputFcPath :: !FilePath,
    outputGrinPath :: !FilePath,
    outputCpsGrinPath :: !FilePath,
    outputGcGrinPath :: !FilePath,
    outputNativePath :: !FilePath,
    outputObjectPath :: !FilePath
  }

data FcModule = FcModule
  { fcModuleName :: !Text,
    fcProgram :: !Fc.Program
  }

data GrinModule = GrinModule
  { grinModuleName :: !Text,
    plainGrinProgram :: !Grin.GrinProgram,
    cpsGrinProgram :: !Grin.CpsGrinProgram,
    gcGrinProgram :: !Grin.GcGrinProgram
  }

data NativeModule = NativeModule
  { nativeModuleName :: !Text,
    nativeSource :: !(Maybe Text),
    nativeObject :: !(Maybe BL.ByteString)
  }

data PendingCompile = PendingCompile
  { pendingWriteFc :: !Bool,
    pendingModules :: ![Module]
  }

newtype UnitId = UnitId Int
  deriving (Eq, Ord, Show)

data SourceUnit = SourceUnit
  { sourceUnitId :: !UnitId,
    sourceUnitOrder :: !Int,
    sourceUnitSources :: ![SourceModule],
    sourceUnitDependencies :: ![UnitId]
  }

data ResolveUnitResult = ResolveUnitResult
  { resolveUnitExports :: !ModuleExports,
    resolveUnitScopeHashes :: !(Map.Map Text Text),
    resolveUnitResolved :: !(Maybe ResolveResult),
    resolveUnitErrors :: ![ResolveError],
    resolveUnitSuccess :: !Bool
  }

data TypeUnitResult = TypeUnitResult
  { typeUnitTypes :: !(Map.Map Text TcInterface),
    typeUnitHashes :: !(Map.Map Text Text),
    typeUnitOwnInstanceInterface :: !TcInterface,
    typeUnitInstanceInterface :: !TcInterface,
    typeUnitDiagnostics :: ![TcDiagnostic],
    typeUnitWritten :: !(Set.Set Text),
    typeUnitReused :: !(Set.Set Text),
    typeUnitPendingCompile :: !(Maybe PendingCompile),
    typeUnitDesugarInterface :: !TcInterface,
    typeUnitSuccess :: !Bool
  }

data UnitRuntime = UnitRuntime
  { runtimeUnit :: !SourceUnit,
    runtimeResolveResult :: !(TMVar ResolveUnitResult),
    runtimeTypeResult :: !(TMVar TypeUnitResult)
  }

data ModuleCompileConfig = ModuleCompileConfig
  { compileKeepGrin :: !Bool,
    compileKeepNative :: !Bool,
    compileLint :: !Bool,
    compileNoCode :: !Bool,
    compileTarget :: !NativeTarget,
    compileVerbose :: String -> IO (),
    compilePrintTimings :: String -> IO (),
    compileUseColor :: !Bool
  }

data ModuleCompileRequest = ModuleCompileRequest
  { compileOutputRoot :: !FilePath,
    compilePackageRoot :: !FilePath,
    compilePackage :: !Package,
    compileSourceFiles :: ![HackageCabal.FileInfo],
    compileDependencyRoots :: ![FilePath]
  }

newtype ModuleCompileResult = ModuleCompileResult
  { compileObjectPaths :: [FilePath]
  }
  deriving (Eq, Show)

data CompiledPackageModules = CompiledPackageModules
  { compiledSources :: ![SourceModule],
    compiledExports :: !ModuleExports,
    compiledTypes :: !(Map.Map Text TcInterface),
    compiledScopeHashes :: !(Map.Map Text Text),
    compiledTypeHashes :: !(Map.Map Text Text),
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
    taskDependencyInstanceFacts :: !TcInterface,
    taskDependencyInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider)),
    taskBackendPhaseTimings :: !(IORef BackendPhaseTimings)
  }

runInstall :: InstallOptions -> IO ()
runInstall options = do
  result <- install options
  putStrLn ("store: " <> installStorePath result)

install :: InstallOptions -> IO InstallResult
install options = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot options)
  useColor <- hIsTerminalDevice stdout
  let target = installTarget options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
  let verbose message = when (installVerbose options) (putStrLn message)
      printTimings message = when (installPrintTimings options) (putStrLn message)
  root <- resolveInstallTarget (installPackageTarget options)
  let fallbackResolver = networkDependencyResolver
      resolver = localDependencyResolverWithFallback fallbackResolver root
      config =
        ModuleCompileConfig
          { compileKeepGrin = installKeepGrin options,
            compileKeepNative = installKeepNative options,
            compileLint = installLint options,
            compileNoCode = installNoCode options,
            compileTarget = target,
            compileVerbose = verbose,
            compilePrintTimings = printTimings,
            compileUseColor = useColor
          }
  spec <- packageSpecFromSource root
  plan <- buildPackagePlanWithResolver resolver spec
  installedResult <$> installPackagePlan config (installReinstall options) targetStoreRoot plan

-- | Turn the install argument into a local package directory.
--
-- An existing directory is used as-is. Anything else is parsed as a Hackage
-- package name with an optional version (@NAME@ or @NAME-VERSION@) and
-- downloaded from Hackage; without a version the preferred version is used.
resolveInstallTarget :: String -> IO FilePath
resolveInstallTarget target = do
  isDirectory <- doesDirectoryExist target
  if isDirectory
    then pure target
    else case parsePackageTarget target of
      Nothing ->
        ioError
          ( userError
              (target <> " is not an existing directory nor a Hackage package name (NAME[-VERSION])")
          )
      Just (name, requestedVersion) -> do
        version <- maybe (resolvePreferredVersion name) pure requestedVersion
        HackageDownload.downloadPackageWithOptions
          HackageDownload.defaultDownloadOptions
          PackageSpec {pkgName = name, pkgVersion = version}

-- | Split a Hackage target into its package name and optional version.
parsePackageTarget :: String -> Maybe (String, Maybe String)
parsePackageTarget target = do
  packageId <- simpleParsec target :: Maybe CabalPackage.PackageIdentifier
  let version = CabalPackage.pkgVersion packageId
  pure
    ( CabalPackage.unPackageName (CabalPackage.pkgName packageId),
      if version == nullVersion then Nothing else Just (prettyShow version)
    )

resolvePreferredVersion :: String -> IO String
resolvePreferredVersion name = do
  result <- getLatestVersion Nothing name
  either (ioError . userError) pure result

networkDependencyResolver :: DependencyResolver
networkDependencyResolver =
  DependencyResolver
    { resolverResolveVersion = resolvePreferredVersion,
      resolverSourcePath = HackageDownload.downloadPackageWithOptions HackageDownload.defaultDownloadOptions
    }

installPackagePlan :: ModuleCompileConfig -> Bool -> FilePath -> PackagePlan -> IO InstalledPackage
installPackagePlan config reinstall storeRoot plan = do
  dependencies <- mapM (installPackagePlan config False storeRoot) (planDependencyPlans plan)
  installPackage config reinstall storeRoot dependencies (planSourcePath plan)

installPackage :: ModuleCompileConfig -> Bool -> FilePath -> [InstalledPackage] -> FilePath -> IO InstalledPackage
installPackage config reinstall storeRoot dependencies root = do
  packageDirectory <- packageStoreDirectory dependencies root
  let storePath = storeRoot </> packageDirectory
  exists <- doesDirectoryExist storePath
  if exists && not reinstall
    then loadInstalledPackage Set.empty storePath
    else do
      createDirectoryIfMissing True storeRoot
      bracket
        (createTemporaryStoreRoot storeRoot packageDirectory)
        removeTemporaryStoreRoot
        (buildAndPublish storePath)
  where
    buildAndPublish storePath temporaryRoot = do
      built <- installPackageDirect config temporaryRoot dependencies root
      exists <- doesDirectoryExist storePath
      when (exists && reinstall) (removeDirectoryRecursive storePath)
      publishResult <- try (renameDirectory (installStorePath (installedResult built)) storePath)
      case publishResult of
        Right () -> pure (setInstalledStorePath storePath built)
        Left err -> do
          published <- doesDirectoryExist storePath
          if published
            then loadInstalledPackage Set.empty storePath
            else throwIO (err :: IOException)

installPackageDirect :: ModuleCompileConfig -> FilePath -> [InstalledPackage] -> FilePath -> IO InstalledPackage
installPackageDirect config storeRoot dependencies root = do
  let target = compileTarget config
      verbose = compileVerbose config
  verbose ("Read Cabal package: " <> root)
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <- case cabalFiles of
    [] -> ioError (userError ("No .cabal file found under " <> root))
    files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <- case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right value) -> pure value
    (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errors))
  files <- HackageCabal.collectLibraryFiles gpd root
  let packageId = package (packageDescription gpd)
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
  let dependencyIdentities = sortOn id (map (T.pack . takeFileName . installStorePath . installedResult) dependencies)
      packageHash = stableHash (map TE.encodeUtf8 (packageArtifactFormatVersion : dependencyIdentities))
      packageDirectory = T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash
      storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId (T.pack packageDirectory))
  compiled <- compileModulesWithDependencies config storePath root resolvePackage files dependencies
  let parsed = compiledSources compiled
      allExports = compiledExports compiled
      allTypes = compiledTypes compiled
      allScopeHashes = compiledScopeHashes compiled
      allTypeHashes = compiledTypeHashes compiled
      written = compiledWritten compiled
      reused = compiledReused compiled
  unless (compileNoCode config) $ do
    let archive = storePath </> "lib" </> "lib" <> T.unpack packageNameText <> ".a"
        moduleObjects =
          sortOn
            id
            [ outputObjectPath (moduleOutputPaths storePath target (sourceName source))
            | source <- parsed
            ]
    buildLibraryArchive target verbose archive moduleObjects
  writePackageManifest
    (packageManifestPath storePath)
    PackageManifest
      { packageManifestName = packageNameText,
        packageManifestVersion = packageVersionText,
        packageManifestIdentity = T.pack packageDirectory,
        packageManifestDependencies =
          sortOn
            id
            [ T.pack (takeFileName (installStorePath (installedResult dependency)))
            | dependency <- dependencies
            ],
        packageManifestModules = sortOn id (HackageCabal.collectLibraryExposedModules gpd)
      }
  let exposedNames = Set.fromList (HackageCabal.collectLibraryExposedModules gpd)
      ownExports =
        Map.filterWithKey
          (\moduleKey _ -> moduleKeyPackage moduleKey == resolvePackage && moduleKeyName moduleKey `Set.member` exposedNames)
          allExports
  pure
    InstalledPackage
      { installedResult = InstallResult storePath (Set.toAscList written) (Set.toAscList reused),
        installedExports = ownExports,
        installedTypes = Map.restrictKeys allTypes exposedNames,
        installedScopeHashes = Map.restrictKeys allScopeHashes exposedNames,
        installedTypeHashes = Map.restrictKeys allTypeHashes exposedNames,
        installedInstanceFacts = compiledInstanceFacts compiled,
        installedInstanceProviders = Map.restrictKeys (compiledInstanceProviders compiled) exposedNames
      }

compileModules :: ModuleCompileConfig -> ModuleCompileRequest -> IO ModuleCompileResult
compileModules config request = do
  dependencies <- mapM (loadInstalledPackage Set.empty) (compileDependencyRoots request)
  compiled <-
    compileModulesWithDependencies
      config
      (compileOutputRoot request)
      (compilePackageRoot request)
      (compilePackage request)
      (compileSourceFiles request)
      dependencies
  pure
    ModuleCompileResult
      { compileObjectPaths =
          sortOn
            id
            [ outputObjectPath (moduleOutputPaths (compileOutputRoot request) (compileTarget config) (sourceName source))
            | source <- compiledSources compiled
            ]
      }

compileModulesWithDependencies :: ModuleCompileConfig -> FilePath -> FilePath -> Package -> [HackageCabal.FileInfo] -> [InstalledPackage] -> IO CompiledPackageModules
compileModulesWithDependencies config outputRoot packageRoot resolvePackage files dependencies = do
  let verbose = compileVerbose config
  verbose ("Parse " <> show (length files) <> " modules")
  capabilities <- getNumCapabilities
  (parsed, importTimings) <- loadSourceModules (max 1 capabilities) packageRoot files
  loadedDependencies <- loadRequiredDependencies parsed dependencies
  let units = sourceModuleUnits parsed
      dependencyExports = Map.unions (map installedExports loadedDependencies)
      dependencyTypes = LazyMap.unions (map installedTypes loadedDependencies)
      dependencyScopeHashes = Map.unions (map installedScopeHashes loadedDependencies)
      dependencyTypeHashes = LazyMap.unions (map installedTypeHashes loadedDependencies)
      dependencyInstanceFacts = mergeTcInterfaces (map installedInstanceFacts loadedDependencies)
      dependencyInstanceProviders = Map.unions (map installedInstanceProviders loadedDependencies)
      primIdentity = packagePrimIdentity resolvePackage dependencyExports
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
            taskDependencyInstanceFacts = dependencyInstanceFacts,
            taskDependencyInstanceProviders = dependencyInstanceProviders,
            taskBackendPhaseTimings = backendPhaseTimings
          }
  verbose ("Compute " <> show (length units) <> " SCC units")
  (runtimes, taskTimings) <- runPackageTasks taskContext (max 1 capabilities) units
  resolveResults <- mapM (atomically . readTMVar . runtimeResolveResult) runtimes
  phaseTimings <- readIORef backendPhaseTimings
  compilePrintTimings
    config
    ( renderTaskTimeline (compileUseColor config) (importTimings <> taskTimings)
        <> renderBackendPhaseTotals phaseTimings
    )
  typeResults <- mapM (atomically . readTMVar . runtimeTypeResult) runtimes
  let parseDiagnostics = concatMap (concatMap sourceModuleParseDiagnostics . sourceUnitSources . runtimeUnit) runtimes
      resolveDiagnostics = concatMap resolveUnitErrors resolveResults
      typeDiagnostics = concatMap (filter ((== TcError) . diagSeverity) . typeUnitDiagnostics) typeResults
      frontendFailure = renderFrontendFailure parsed parseDiagnostics resolveDiagnostics typeDiagnostics
  unless (null frontendFailure) (ioError (userError frontendFailure))
  let localExports = Map.unions (map resolveUnitExports resolveResults)
      localScopeHashes = Map.unions (map resolveUnitScopeHashes resolveResults)
      localTypes = Map.unions (map typeUnitTypes typeResults)
      localTypeHashes = Map.unions (map typeUnitHashes typeResults)
      allExports = localExports `Map.union` dependencyExports
      allScopeHashes = localScopeHashes `Map.union` dependencyScopeHashes
      allTypes = localTypes `LazyMap.union` dependencyTypes
      allTypeHashes = localTypeHashes `LazyMap.union` dependencyTypeHashes
      packageInstanceInterface = mergeTcInterfaces (dependencyInstanceFacts : map typeUnitOwnInstanceInterface typeResults)
      instanceProviders =
        Map.fromList
          [ (sourceName source, interfaceInstanceProviders (typeUnitInstanceInterface result))
          | (runtime, result) <- zip runtimes typeResults,
            source <- sourceUnitSources (runtimeUnit runtime)
          ]
  writePackageInstanceArtifact verbose outputRoot allTypeHashes instanceProviders packageInstanceInterface
  pure
    CompiledPackageModules
      { compiledSources = parsed,
        compiledExports = allExports,
        compiledTypes = allTypes,
        compiledScopeHashes = allScopeHashes,
        compiledTypeHashes = allTypeHashes,
        compiledInstanceFacts = packageInstanceInterface,
        compiledInstanceProviders = instanceProviders,
        compiledWritten = Set.unions (map typeUnitWritten typeResults),
        compiledReused = Set.unions (map typeUnitReused typeResults)
      }

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

packageStoreDirectory :: [InstalledPackage] -> FilePath -> IO FilePath
packageStoreDirectory dependencies root = do
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <- case cabalFiles of
    [] -> ioError (userError ("No .cabal file found under " <> root))
    files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <- case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right value) -> pure value
    (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errors))
  let packageId = package (packageDescription gpd)
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
      dependencyIdentities = sortOn id (map (T.pack . takeFileName . installStorePath . installedResult) dependencies)
      packageHash = stableHash (map TE.encodeUtf8 (packageArtifactFormatVersion : dependencyIdentities))
  pure (T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash)

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
    loadDependency dependency = loadInstalledPackage requirements (installStorePath (installedResult dependency))

requiredDependencyModules :: [SourceModule] -> Set.Set (Maybe Text, Text)
requiredDependencyModules sources =
  Set.fromList
    ( [ (importDeclPackage importDecl, importDeclModule importDecl)
      | source <- sources,
        importDecl <- Syntax.moduleImports (sourceModuleAst source),
        not (localImport importDecl)
      ]
        <> [(Nothing, "Prelude") | any moduleUsesImplicitPrelude sources]
        <> [(Nothing, name) | name <- wiredTypeModules]
    )
  where
    localNames = Set.fromList (map sourceName sources)
    localImport importDecl =
      importDeclPackage importDecl == Just "this"
        || (isNothing (importDeclPackage importDecl) && importDeclModule importDecl `Set.member` localNames)

loadInstalledPackage :: Set.Set (Maybe Text, Text) -> FilePath -> IO InstalledPackage
loadInstalledPackage requirements storePath = do
  manifestResult <- readPackageManifest (packageManifestPath storePath)
  manifest <- either (ioError . userError . ("Invalid installed package manifest: " <>)) pure manifestResult
  let selectedModules = filter (moduleRequired manifest) (packageManifestModules manifest)
  entries <- mapM loadModule selectedModules
  (instanceFacts', instanceProviders) <-
    if null selectedModules
      then pure (mempty, Map.empty)
      else loadPackageInstances selectedModules
  let package = Package (packageManifestName manifest) (PackageId (packageManifestIdentity manifest))
      exports = Map.fromList [(ModuleKey package name, scope) | (name, scope, _) <- entries]
      types = LazyMap.fromList [(name, interface) | (name, _, interface) <- entries]
      scopeHashes = Map.fromList [(name, T.pack (stableHash [BL.toStrict (encodeResolveScope scope)])) | (name, scope, _) <- entries]
      typeHashes = LazyMap.fromList [(name, T.pack (stableHash [BL.toStrict (encodeTypeInterface interface)])) | (name, _, interface) <- entries]
  pure
    InstalledPackage
      { installedResult = InstallResult storePath [] (packageManifestModules manifest),
        installedExports = exports,
        installedTypes = types,
        installedScopeHashes = scopeHashes,
        installedTypeHashes = typeHashes,
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
      let typeArtifact = decodeTypeArtifact typeBytes
      unless (resolveArtifactModuleName resolveArtifact == name) (ioError (userError ("Resolve artifact module name does not match " <> resolvePath)))
      unless (typeArtifactModuleName typeArtifact == name) (ioError (userError ("Type artifact module name does not match " <> typePath)))
      pure (name, resolveArtifactScope resolveArtifact, typeArtifactInterface typeArtifact)

    loadPackageInstances selected = do
      let path = storePath </> "instances.cbor"
      exists <- doesFileExist path
      if not exists
        then pure (mempty, Map.empty)
        else do
          bytes <- BL.readFile path
          let artifact = decodeTypeArtifact bytes
          unless (typeArtifactModuleName artifact == "$package-instances") (ioError (userError ("Package instance artifact name does not match " <> path)))
          let providers = Map.restrictKeys (Map.map Set.fromList (typeArtifactInstanceProviders artifact)) (Set.fromList selected)
              visibleProviders = Set.unions (Map.elems providers)
          pure (selectInstanceProviders (typeArtifactInterface artifact) visibleProviders, providers)

parseSource :: FilePath -> HackageCabal.FileInfo -> IO SourceModule
parseSource root fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  ParsedInterfaceFile path modu sourceLines parseDiagnostics _ extensions <- parseInterfaceFile root fileInfo
  pure (SourceModule path (BS.length bytes) (T.pack (stableHash [bytes])) modu extensions sourceLines parseDiagnostics)

loadSourceModules :: Int -> FilePath -> [HackageCabal.FileInfo] -> IO ([SourceModule], [TaskTiming])
loadSourceModules workers root files = do
  results <- mapM (const newEmptyTMVarIO) files
  let tasks = zipWith3 loadTask [0 ..] files results
  timings <- runTaskGraph workers tasks
  sources <- mapM (atomically . readTMVar) results
  pure (sources, timings)
  where
    loadTask order fileInfo result =
      Task
        { taskId = TaskId ("imports:" <> HackageCabal.fileInfoPath fileInfo),
          taskKind = TaskParse,
          taskOrder = order,
          taskDependencies = Set.empty,
          taskAction = do
            source <- parseSource root fileInfo
            let ast = sourceModuleAst source
                imports = map importDeclModule (Syntax.moduleImports ast)
            _ <- evaluate (rnf (moduleName ast, imports))
            atomically (putTMVar result source)
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

renderResolveErrors :: [SourceModule] -> [ResolveError] -> String
renderResolveErrors sources errors =
  "Name resolution failed:\n"
    <> intercalate "\n\n" (map (renderResolveError sourceLines) errors)
    <> "\n"
  where
    sourceLines = Map.unions (map sourceModuleSourceLines sources)

renderResolveError :: Map.Map FilePath (Map.Map Int Text) -> ResolveError -> String
renderResolveError sourceLines resolveError =
  case resolveError of
    ResolveResolutionError sourceSpan name namespace message ->
      renderResolveLocation sourceSpan
        <> ": error: "
        <> renderResolveMessage message name namespace
        <> renderResolveExcerpt sourceLines sourceSpan
    ResolveNotImplemented message -> "error: not implemented: " <> message

renderResolveLocation :: SourceSpan -> String
renderResolveLocation sourceSpan =
  case sourceSpan of
    NoSourceSpan -> "<unknown location>"
    SourceSpan sourcePath startLine startColumn _ _ _ _ ->
      sourcePath <> ":" <> show startLine <> ":" <> show startColumn

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

renderResolveExcerpt :: Map.Map FilePath (Map.Map Int Text) -> SourceSpan -> String
renderResolveExcerpt sourceLines sourceSpan =
  case sourceSpan of
    NoSourceSpan -> ""
    SourceSpan sourcePath startLine startColumn endLine endColumn _ _ ->
      case Map.lookup sourcePath sourceLines >>= Map.lookup startLine of
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

renderFrontendFailure :: [SourceModule] -> [Value] -> [ResolveError] -> [TcDiagnostic] -> String
renderFrontendFailure sources parseDiagnostics resolveDiagnostics typeDiagnostics =
  case sections of
    [] -> ""
    _ -> intercalate "\n\n" (map dropFinalNewlines sections) <> "\n"
  where
    sections =
      [renderParseDiagnostics parseDiagnostics | not (null parseDiagnostics)]
        <> [renderResolveErrors sources resolveDiagnostics | not (null resolveDiagnostics)]
        <> [renderTypeErrors sources typeDiagnostics | not (null typeDiagnostics)]

    dropFinalNewlines = reverse . dropWhile (== '\n') . reverse

renderParseDiagnostics :: [Value] -> String
renderParseDiagnostics diagnostics =
  "Parse failed:\n" <> intercalate "\n" (map (renderHumanDiagnostic "parse") diagnostics)

renderTypeErrors :: [SourceModule] -> [TcDiagnostic] -> String
renderTypeErrors sources diagnostics =
  "Type check failed:\n"
    <> intercalate "\n\n" (map renderTypeError diagnostics)
    <> "\n"
  where
    sourceLines = Map.unions (map sourceModuleSourceLines sources)
    renderTypeError diagnostic =
      case diagLoc diagnostic of
        Nothing -> "<unknown location>: error: " <> renderTypeErrorKind (diagKind diagnostic)
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
    OtherError message ->
      message

runPackageTasks :: PackageTaskContext -> Int -> [SourceUnit] -> IO ([UnitRuntime], [TaskTiming])
runPackageTasks context workers units = do
  runtimes <-
    forM units $ \unit ->
      UnitRuntime unit <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  let runtimeMap = Map.fromList [(sourceUnitId (runtimeUnit runtime), runtime) | runtime <- runtimes]
      tasks = concatMap (unitTasks runtimeMap) runtimes
  timings <- runTaskGraph workers tasks
  pure (runtimes, timings)
  where
    unitTasks runtimeMap runtime =
      map (parseTask runtime) (sourceUnitSources unit)
        <> [resolveTask runtimeMap runtime, typeTask runtimeMap runtime]
        <> [backendTask runtime | not (compileNoCode config)]
      where
        unit = runtimeUnit runtime

    parseTask runtime source =
      Task
        { taskId = parseTaskId source,
          taskKind = TaskParse,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies = Set.empty,
          taskAction = evaluate (rnf (sourceModuleAst source, sourceModuleParseDiagnostics source))
        }

    resolveTask runtimeMap runtime =
      Task
        { taskId = resolveTaskId (runtimeUnit runtime),
          taskKind = TaskResolve,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( map parseTaskId (sourceUnitSources (runtimeUnit runtime))
                  <> map (resolveTaskId . runtimeUnit . lookupRuntime runtimeMap) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runResolveUnit
              context
              runtimeMap
              runtime
        }

    typeTask runtimeMap runtime =
      Task
        { taskId = typeTaskId (runtimeUnit runtime),
          taskKind = TaskTypeCheck,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( resolveTaskId (runtimeUnit runtime)
                  : map (typeTaskId . runtimeUnit . lookupRuntime runtimeMap) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runTypeUnit
              context
              runtimeMap
              runtime
        }

    backendTask runtime =
      Task
        { taskId = backendTaskId (runtimeUnit runtime),
          taskKind = TaskBackend,
          taskOrder = negate (sum (map sourceModuleSize (sourceUnitSources (runtimeUnit runtime)))),
          taskDependencies = Set.singleton (typeTaskId (runtimeUnit runtime)),
          taskAction = runBackendUnit context runtime
        }
    config = taskModuleCompileConfig context

parseTaskId :: SourceModule -> TaskId
parseTaskId = TaskId . ("parse:" <>) . sourceModulePath

resolveTaskId :: SourceUnit -> TaskId
resolveTaskId = TaskId . ("resolve:" <>) . T.unpack . unitLabel

typeTaskId :: SourceUnit -> TaskId
typeTaskId = TaskId . ("type-check:" <>) . T.unpack . unitLabel

backendTaskId :: SourceUnit -> TaskId
backendTaskId = TaskId . ("backend:" <>) . T.unpack . unitLabel

unitLabel :: SourceUnit -> Text
unitLabel = T.intercalate "+" . map sourceName . sourceUnitSources

sourceName :: SourceModule -> Text
sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

sourceDependencyNames :: SourceModule -> [Text]
sourceDependencyNames source =
  map importDeclModule (Syntax.moduleImports modu)
    <> ["Prelude" | moduleUsesImplicitPrelude source]
  where
    modu = sourceModuleAst source

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
      packageModules = modulesInPackage resolvePackage (map sourceModuleAst sources)
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredTypeModules)
      availableExports = Map.unions (map resolveUnitExports dependencyResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResults) `Map.union` dependencyScopeHashes
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      scopeHashes = availableScopeHashes
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      resolvePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      parseSuccess = all (null . sourceModuleParseDiagnostics) sources
      dependenciesSucceeded = all resolveUnitSuccess dependencyResults
  let builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
      resolved = resolveWithDeps builtinScope availableExports packageModules
      errors = resolveErrors resolved
      unitExports = extractInterfaceWithDeps availableExports resolved
      success = parseSuccess && dependenciesSucceeded && null errors
  when success (mapM_ (\source -> writeArtifact verbose hashes unitExports resolvePackage (resolvePath source) source) sources)
  let ownScopeHashes = updateScopeHashes resolvePackage unitExports Map.empty sources
  atomically $
    putTMVar
      (runtimeResolveResult runtime)
      ResolveUnitResult
        { resolveUnitExports = unitExports,
          resolveUnitScopeHashes = ownScopeHashes,
          resolveUnitResolved = Just resolved,
          resolveUnitErrors = errors,
          resolveUnitSuccess = success
        }
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

runTypeUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runTypeUnit context runtimes runtime = do
  resolvedOutput <- atomically (readTMVar (runtimeResolveResult runtime))
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
      dependencyNames = nub (importedNames <> wiredTypeModules)
      availableTypes = LazyMap.unions (map typeUnitTypes dependencyResults) `LazyMap.union` dependencyTypes
      availableTypeHashes = LazyMap.unions (map typeUnitHashes dependencyResults) `LazyMap.union` dependencyTypeHashes
      availableExports = Map.unions (map resolveUnitExports dependencyResolveResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResolveResults) `Map.union` dependencyScopeHashes
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      scopeInputs =
        [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableScopeHashes]]
      typeInputs =
        sortOn fst $
          sourceHashes
            <> scopeInputs
            <> [("type:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableTypeHashes]]
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
      externalInstanceProviders =
        Set.unions
          [ Map.findWithDefault Set.empty name dependencyInstanceProviders
          | name <- dependencyNames,
            name `notElem` unitNames
          ]
      externalInstanceInterface = selectInstanceProviders dependencyInstanceFacts externalInstanceProviders
      importedInstanceInterface =
        mergeTcInterfaces
          (externalInstanceInterface : map typeUnitInstanceInterface dependencyResults)
      importedTypes =
        mergeTcInterfaces
          ( importedInstanceInterface
              : [ interface
                | name <- dependencyNames,
                  name `notElem` unitNames,
                  Just interface <- [Map.lookup name availableTypes]
                ]
          )
      checkUnit = do
        resolved <-
          case resolveUnitResolved resolvedOutput of
            Just result -> pure result
            Nothing ->
              let packageModules = modulesInPackage resolvePackage (map sourceModuleAst sources)
                  builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
               in pure (resolveWithDeps builtinScope availableExports packageModules)
        let checked =
              typecheckModuleSccWithInterface
                (tcConfig primIdentity)
                importedTypes
                (map snd (resolvedModules resolved))
        _ <- evaluate (sum (map (length . tcModuleDiagnostics) (fst checked)))
        pure checked
      dependencySuccess = all typeUnitSuccess dependencyResults
      resolveSuccess = resolveUnitSuccess resolvedOutput
  initialChecked@(_, checkedInterface) <- checkUnit
  let completeInterface = mergeTcInterfaces [importedTypes, checkedInterface]
      unitTypes = map (moduleTypeInterface (resolveUnitExports resolvedOutput) resolvePackage completeInterface) sources
      ownInstanceInterface = addReferencedFacts completeInterface (instanceFacts checkedInterface)
      completeInstanceInterface = mergeTcInterfaces [importedInstanceInterface, ownInstanceInterface]
      diagnostics = concatMap tcModuleDiagnostics (fst initialChecked)
      typeSuccess = all tcModuleSuccess (fst initialChecked)
      success = resolveSuccess && dependencySuccess && typeSuccess
  ownTypeHashes <-
    if success
      then Map.fromList <$> zipWithM (writeTypeArtifact verbose typeInputs typePath) sources unitTypes
      else pure Map.empty
  pendingCompile <-
    if compileNoCode config || not success
      then pure Nothing
      else do
        let (checkedModules, _) = initialChecked
        pure (Just (PendingCompile True checkedModules))
  let unitSet = Set.fromList unitNames
  -- Force the type result before this type-check task ends.
  typeResult <-
    evaluate
      TypeUnitResult
        { typeUnitTypes = Map.fromList (zip unitNames unitTypes),
          typeUnitHashes = ownTypeHashes,
          typeUnitOwnInstanceInterface = ownInstanceInterface,
          typeUnitInstanceInterface = completeInstanceInterface,
          typeUnitDiagnostics = diagnostics,
          typeUnitWritten = unitSet,
          typeUnitReused = Set.empty,
          typeUnitPendingCompile = pendingCompile,
          typeUnitDesugarInterface = completeInterface,
          typeUnitSuccess = success
        }
  atomically (putTMVar (runtimeTypeResult runtime) typeResult)
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

runBackendUnit :: PackageTaskContext -> UnitRuntime -> IO ()
runBackendUnit context runtime = do
  started <- getMonotonicTimeNSec
  result <- atomically (readTMVar (runtimeTypeResult runtime))
  case typeUnitPendingCompile result of
    Just pending | typeUnitSuccess result -> do
      let config = taskModuleCompileConfig context
          storePath = taskStorePath context
      phaseTimings <-
        compileCheckedModules
          config
          (pendingWriteFc pending)
          (compileVerbose config)
          (taskPrimIdentity context)
          (typeUnitDesugarInterface result)
          (moduleOutputPaths storePath (compileTarget config))
          (pendingModules pending)
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended phaseTimings, ()))
    _ -> do
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended mempty, ()))

instanceFacts :: TcInterface -> TcInterface
instanceFacts interface =
  mempty
    { tcInterfaceInstances = tcInterfaceInstances interface,
      tcInterfaceDataFamilyInstances = tcInterfaceDataFamilyInstances interface,
      tcInterfaceTypeFamilyInstances = tcInterfaceTypeFamilyInstances interface
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

selectInstanceProviders :: TcInterface -> Set.Set InstanceProvider -> TcInterface
selectInstanceProviders complete providers
  | Set.null providers = mempty
  | otherwise =
      addReferencedFacts
        complete
        mempty
          { tcInterfaceInstances = filter ((`Set.member` providers) . first PackageId . iiDictOrigin) (tcInterfaceInstances complete),
            tcInterfaceDataFamilyInstances = filter ((`Set.member` providers) . tyConOrigin . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstances complete),
            tcInterfaceTypeFamilyInstances = filter ((`Set.member` providers) . tfiiOrigin) (tcInterfaceTypeFamilyInstances complete)
          }
  where
    first transform (left, right) = (transform left, right)
    tyConOrigin tyCon = (tyConPackageId tyCon, tyConModuleName tyCon)

writePackageInstanceArtifact :: (String -> IO ()) -> FilePath -> Map.Map Text Text -> Map.Map Text (Set.Set InstanceProvider) -> TcInterface -> IO ()
writePackageInstanceArtifact verbose storePath typeHashes providers interface = do
  let path = storePath </> "instances.cbor"
      hashes = sortOn fst [("type:" <> name, digest) | (name, digest) <- Map.toList typeHashes]
  createDirectoryIfMissing True storePath
  BL.writeFile path (encodeTypeArtifact (TypeArtifact "$package-instances" hashes (Map.map Set.toAscList providers) interface))
  verbose ("Write package instances: " <> path)

wiredTypeModules :: [Text]
wiredTypeModules = ["GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Num", "GHC.Tuple", "GHC.Types"]

builtinFunctionScope :: Package -> ModuleExports -> [(Package, Module)] -> Scope
builtinFunctionScope currentPackage dependencyExports packageModules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    allExports = collectModuleExportsWithDeps dependencyExports packageModules `Map.union` dependencyExports
    lookupBuiltin name = lookupImportedModule currentPackage Nothing name allExports
    builtinFunctionModules = ["GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Num"]

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

compileCheckedModules :: ModuleCompileConfig -> Bool -> (String -> IO ()) -> PackageId -> TcInterface -> (Text -> ModuleOutputPaths) -> [Module] -> IO BackendPhaseTimings
compileCheckedModules config writeFc verbose primIdentity interface outputPaths checkedModules = do
  (splitModules, desugarNs) <- measureTime $ do
    let bindings = concatMap tcModuleBindings checkedModules
        desugarResults = map (Fc.desugarModuleFc (DesugarConfig primIdentity) bindings interface) checkedModules
    unless (all dsSuccess desugarResults) (ioError (userError ("FC generation failed: " <> unlines (concatMap dsErrors desugarResults))))
    let moduleNames = map (fromMaybe "Main" . moduleName) checkedModules
        fcModules = zipWith FcModule moduleNames (map dsProgram desugarResults)
        fcErrors = [(fcModuleName fcModule, err) | fcModule <- fcModules, err <- Fc.lintProgram (fcProgram fcModule)]
        fcReport = ["    " <> T.unpack name <> ": " <> show err | (name, err) <- fcErrors]
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
    when writeFc (mapM_ writeFcModule fcModules)
    pure (spanEmptyModules fcModules)
  let (emptyFcModules, nonemptyFcModules) = splitModules
  (grinModules, grinNs) <- measureTime $ do
    grinModules <- mapM lowerGrinModule nonemptyFcModules
    when keepGrin (mapM_ writeGrinModule grinModules)
    pure grinModules
  (_, nativeNs) <- measureTime $ do
    mapM_ writeEmptyModule emptyFcModules
    nativeModules <- mapM (generateNativeModule target) grinModules
    mapM_ writeNativeSourceFile nativeModules
    mapM_ compileNativeSourceFile nativeModules
    when keepNative (mapM_ writeNativeDisassembly nativeModules)
    unless keepNative (mapM_ removeNativeSourceFile nativeModules)
  pure
    BackendPhaseTimings
      { backendDesugarNs = desugarNs,
        backendGrinNs = grinNs,
        backendNativeNs = nativeNs,
        backendOtherNs = 0
      }
  where
    keepGrin = compileKeepGrin config
    keepNative = compileKeepNative config
    lint = compileLint config
    target = compileTarget config
    spanEmptyModules = foldr split ([], [])
      where
        split fcModule (emptyModules, nonemptyModules)
          | null (Fc.programDecls (fcProgram fcModule)) = (fcModule : emptyModules, nonemptyModules)
          | otherwise = (emptyModules, fcModule : nonemptyModules)

    writeEmptyModule fcModule = do
      let name = fcModuleName fcModule
          paths = outputPaths name
      createDirectoryIfMissing True (takeDirectory (outputObjectPath paths))
      BS.writeFile (outputObjectPath paths) ""
      when (compileKeepGrin config) $ do
        writeFile (outputGrinPath paths) ""
        writeFile (outputCpsGrinPath paths) ""
        writeFile (outputGcGrinPath paths) ""
      when (compileKeepNative config) (writeFile (outputNativePath paths) "")
      verbose ("Write empty object: " <> T.unpack name)

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

    lowerGrinModule fcModule = do
      plainProgram <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram (fcProgram fcModule))
      when (compileLint config) $ do
        let plainErrors = Grin.lintProgram plainProgram
        unless (null plainErrors) (ioError (userError ("GRIN lint failed: " <> show plainErrors)))
      cpsProgram <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin plainProgram)
      let gcProgram = Grin.lowerGc cpsProgram
      when (compileLint config) $ do
        let gcErrors = Grin.lintProgram (Grin.gcGrinProgram gcProgram)
        unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed: " <> show gcErrors)))
      pure
        GrinModule
          { grinModuleName = fcModuleName fcModule,
            plainGrinProgram = plainProgram,
            cpsGrinProgram = cpsProgram,
            gcGrinProgram = gcProgram
          }

    writeGrinModule grinModule = do
      let name = grinModuleName grinModule
          paths = outputPaths name
      writeGrinFile (outputGrinPath paths) (plainGrinProgram grinModule)
      verbose ("Write GRIN: " <> T.unpack name)
      writeGrinFile (outputCpsGrinPath paths) (Grin.cpsGrinProgram (cpsGrinProgram grinModule))
      verbose ("Write CPS-GRIN: " <> T.unpack name)
      writeGrinFile (outputGcGrinPath paths) (Grin.gcGrinProgram (gcGrinProgram grinModule))
      verbose ("Write GC-GRIN: " <> T.unpack name)

    writeGrinFile path program = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (withFinalNewline (renderString (layoutPretty defaultLayoutOptions (Grin.prettyProgram program))))

    generateNativeModule selectedTarget grinModule = do
      let name = grinModuleName grinModule
          gcProgram = gcGrinProgram grinModule
      case selectedTarget of
        AppleArm64 -> do
          object <- either (ioError . userError . ("Apple ARM64 object generation failed: " <>) . show) pure (Arm64.compileModuleObject gcProgram)
          pure (NativeModule name Nothing (Just object))
        LinuxAmd64 -> do
          object <- either (ioError . userError . ("Linux AMD64 object generation failed: " <>) . show) pure (Amd64.compileModuleObject gcProgram)
          pure (NativeModule name Nothing (Just object))
        _ -> do
          source <- generateNativeCode selectedTarget gcProgram
          pure (NativeModule name (Just source) Nothing)

    writeNativeSourceFile nativeModule = do
      case nativeSource nativeModule of
        Nothing -> pure ()
        Just source -> do
          let name = nativeModuleName nativeModule
              path = outputNativePath (outputPaths name)
          createDirectoryIfMissing True (takeDirectory path)
          TIO.writeFile path source
          verbose ("Write native source: " <> T.unpack name)

    compileNativeSourceFile nativeModule = do
      let name = nativeModuleName nativeModule
          paths = outputPaths name
      case nativeObject nativeModule of
        Just object -> BL.writeFile (outputObjectPath paths) object
        Nothing -> do
          (compiler, compilerArguments) <- backendCompiler (compileTarget config)
          runTool compiler (compilerArguments <> ["-c", outputNativePath paths, "-o", outputObjectPath paths])
      verbose ("Write object: " <> T.unpack name)

    writeNativeDisassembly nativeModule =
      case nativeObject nativeModule of
        Nothing -> pure ()
        Just _ -> do
          let paths = outputPaths (nativeModuleName nativeModule)
          disassembleObject (outputObjectPath paths) (outputNativePath paths)
          verbose ("Write native disassembly: " <> T.unpack (nativeModuleName nativeModule))

    removeNativeSourceFile nativeModule =
      case nativeSource nativeModule of
        Nothing -> pure ()
        Just _ -> removeFile (outputNativePath (outputPaths (nativeModuleName nativeModule)))

generateNativeCode :: NativeTarget -> Grin.GcGrinProgram -> IO Text
generateNativeCode target gcProgram =
  case target of
    AppleArm64 -> ioError (userError "Apple ARM64 uses direct object generation")
    LinuxAmd64 -> ioError (userError "Linux AMD64 uses direct object generation")
    Llvm -> either (ioError . userError . ("LLVM generation failed: " <>) . show) pure (Llvm.compileModule gcProgram)
    Wasm32Wasip3 -> either (ioError . userError . ("WebAssembly generation failed: " <>) . show) pure (Wasm.compileModule gcProgram)

moduleOutputPaths :: FilePath -> NativeTarget -> Text -> ModuleOutputPaths
moduleOutputPaths storePath target name =
  ModuleOutputPaths
    { outputFcPath = directory </> "core",
      outputGrinPath = directory </> "grin",
      outputCpsGrinPath = directory </> "cps.grin",
      outputGcGrinPath = directory </> "gc.grin",
      outputNativePath = objectPath <> nativeSourceExtension target,
      outputObjectPath = objectPath
    }
  where
    directory = storePath </> moduleNameDirectory name
    objectPath = directory </> T.unpack name <> ".o"

withFinalNewline :: String -> String
withFinalNewline rendered
  | "\n" `isSuffixOf` rendered = rendered
  | otherwise = rendered <> "\n"

nativeSourceExtension :: NativeTarget -> String
nativeSourceExtension target =
  case target of
    Llvm -> ".ll"
    AppleArm64 -> ".objdump"
    LinuxAmd64 -> ".objdump"
    Wasm32Wasip3 -> ".s"

disassembleObject :: FilePath -> FilePath -> IO ()
disassembleObject objectPath outputPath = do
  llvmObjdump <- findExecutable "llvm-objdump"
  objdump <- maybe (findExecutable "objdump") (pure . Just) llvmObjdump
  executable <- maybe (ioError (userError "Keep-native requires llvm-objdump or objdump")) pure objdump
  (status, output, errors) <- readProcessWithExitCode executable ["-dr", objectPath] ""
  case status of
    ExitSuccess -> writeFile outputPath output
    ExitFailure _ -> ioError (userError (unlines ["Object disassembly failed: " <> executable, errors]))

buildLibraryArchive :: NativeTarget -> (String -> IO ()) -> FilePath -> [FilePath] -> IO ()
buildLibraryArchive target verbose archive moduleObjects = do
  createDirectoryIfMissing True (takeDirectory archive)
  archiveExists <- doesFileExist archive
  when archiveExists (removeFile archive)
  archiver <- backendArchiver target
  nonemptyObjects <- filterM (fmap (> 0) . getFileSize) moduleObjects
  withDeterministicArchiveEnvironment $
    runTool archiver (["rcs", archive] <> nonemptyObjects)
  verbose ("Write archive: " <> archive)

withDeterministicArchiveEnvironment :: IO value -> IO value
withDeterministicArchiveEnvironment action =
  bracket setDeterministic restoreEnvironment (const action)
  where
    setDeterministic = do
      previous <- lookupEnv "ZERO_AR_DATE"
      setEnv "ZERO_AR_DATE" "1"
      pure previous
    restoreEnvironment previous =
      case previous of
        Nothing -> unsetEnv "ZERO_AR_DATE"
        Just value -> setEnv "ZERO_AR_DATE" value

runTool :: FilePath -> [String] -> IO ()
runTool executable arguments = do
  (status, output, errors) <- readProcessWithExitCode executable arguments ""
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

moduleTypeInterface :: ModuleExports -> Package -> TcInterface -> SourceModule -> TcInterface
moduleTypeInterface exports package interface source =
  addReferencedFacts
    interface
    interface
      { tcInterfaceTerms = filter visibleTerm (tcInterfaceTerms interface),
        tcInterfaceTyCons = filter visibleTyCon (tcInterfaceTyCons interface),
        tcInterfaceDataTypes = filter (visibleTypeIdentity . dataTypeKey) (tcInterfaceDataTypes interface),
        tcInterfaceClasses = filter visibleClass (tcInterfaceClasses interface),
        tcInterfaceInstances = filter visibleInstance (tcInterfaceInstances interface),
        tcInterfaceDataFamilyInstances = filter visibleDataFamilyInstance (tcInterfaceDataFamilyInstances interface),
        tcInterfaceTypeFamilyInstances = filter visibleTypeFamilyInstance (tcInterfaceTypeFamilyInstances interface),
        tcInterfacePatSyns = filter (\info -> visibleTerm (patSynKey info, psiScheme info)) (tcInterfacePatSyns interface)
      }
  where
    name = fromMaybe "Main" (moduleName (sourceModuleAst source))
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
    localTyCon tyCon = tyConPackageId tyCon == packageId package && tyConModuleName tyCon == name
    visibleTerm (TcTermGlobal packageId' moduleName' identifier, _) =
      visibleTermIdentity (packageId', moduleName', identifier)
        || any (visibleTermIdentity . (packageId',moduleName',)) (patSynHelperBase identifier)
    visibleTerm (TcTermLocal {}, _) = False
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
      ResolvedTopLevel packageId' resolvedName -> Just (packageId', fromMaybe name (nameQualifier resolvedName), nameText resolvedName)
      _ -> Nothing

addReferencedFacts :: TcInterface -> TcInterface -> TcInterface
addReferencedFacts complete interface =
  interface
    { tcInterfaceTerms = tcInterfaceTerms interface <> callStackSupportTerms,
      tcInterfaceTyCons = Map.elems (existingTyCons <> supportTyCons),
      tcInterfaceDataTypes = tcInterfaceDataTypes interface <> supportDataTypes,
      tcInterfaceClasses = tcInterfaceClasses interface <> supportClasses
    }
  where
    existingTyCons = Map.fromList [(tciTyCon info, info) | info <- tcInterfaceTyCons interface]
    availableTyCons = Map.fromList [(tciTyCon info, info) | info <- tcInterfaceTyCons complete]
    existingDataTypes = Set.fromList (map dtiTyCon (tcInterfaceDataTypes interface))
    availableDataTypes = Map.fromList [(dtiTyCon info, info) | info <- tcInterfaceDataTypes complete]
    existingClasses = Set.fromList (map ciTyCon (tcInterfaceClasses interface))
    availableClasses = Map.fromList [(ciTyCon info, info) | info <- tcInterfaceClasses complete]
    -- A use of a function with a HasCallStack constraint desugars to calls
    -- of the call-stack helpers, even when the module does not import them.
    callStackModules =
      Set.fromList
        [ (tyConPackageId tyCon, tyConModuleName tyCon)
        | tyCon <- concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface),
          tyConName tyCon == "CallStack"
        ]
    existingTermKeys = Set.fromList (map fst (tcInterfaceTerms interface))
    callStackSupportTerms =
      [ entry
      | entry@(TcTermGlobal package' moduleName' identifier, _) <- tcInterfaceTerms complete,
        (package', moduleName') `Set.member` callStackModules,
        identifier `elem` ["pushCallSite", "emptyCallStack"],
        fst entry `Set.notMember` existingTermKeys
      ]
    callStackSupportTyCons =
      [ tyCon
      | tyCon <- Map.keys availableTyCons,
        (tyConPackageId tyCon, tyConModuleName tyCon) `Set.member` callStackModules,
        tyConName tyCon `elem` ["SrcLoc", "CallStack"]
      ]
    referenced =
      Set.fromList
        ( concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface)
            <> concatMap (typeSchemeTyCons . snd) callStackSupportTerms
            <> callStackSupportTyCons
            <> concatMap tyConInfoTyCons (tcInterfaceTyCons interface)
            <> concatMap dataTypeInfoTyCons (tcInterfaceDataTypes interface)
            <> concatMap classInfoTyCons (tcInterfaceClasses interface)
            <> concatMap instanceInfoTyCons (tcInterfaceInstances interface)
            <> concatMap dataFamilyInstanceInfoTyCons (tcInterfaceDataFamilyInstances interface)
            <> concatMap typeFamilyInstanceInfoTyCons (tcInterfaceTypeFamilyInstances interface)
        )
    reachable = closeTyCons Set.empty referenced
    supportTyCons = Map.restrictKeys availableTyCons (reachable `Set.difference` Map.keysSet existingTyCons)
    supportDataTypes =
      [ info
      | info <- tcInterfaceDataTypes complete,
        dtiTyCon info `Set.member` reachable,
        dtiTyCon info `Set.notMember` existingDataTypes
      ]
    supportClasses =
      [ info
      | info <- tcInterfaceClasses complete,
        ciTyCon info `Set.member` reachable,
        ciTyCon info `Set.notMember` existingClasses
      ]
    closeTyCons found pending
      | Set.null pending = found
      | otherwise =
          let (tyCon, pending') = Set.deleteFindMin pending
              dependencies =
                Set.fromList
                  ( maybe [] tyConInfoTyCons (Map.lookup tyCon availableTyCons)
                      <> maybe [] dataTypeInfoTyCons (Map.lookup tyCon availableDataTypes)
                      <> maybe [] classInfoTyCons (Map.lookup tyCon availableClasses)
                  )
              found' = Set.insert tyCon found
           in closeTyCons found' (pending' <> (dependencies `Set.difference` found'))

tyConInfoTyCons :: TyConInfo -> [TyCon]
tyConInfoTyCons info =
  typeSchemeTyCons (tciKindScheme info)
    <> maybe [] (maybe [] typeTyCons . tsiBody) (tciTypeSynonym info)

dataTypeInfoTyCons :: DataTypeInfo -> [TyCon]
dataTypeInfoTyCons info =
  dtiTyCon info
    : typeTyCons (dtiResultKind info)
      <> concatMap dataConInfoTyCons (dtiConstructors info)

dataConInfoTyCons :: DataConInfo -> [TyCon]
dataConInfoTyCons info =
  concatMap predTyCons (dciTheta info)
    <> concatMap (typeTyCons . dcfiType) (dciFields info)
    <> typeTyCons (dciResTy info)

classInfoTyCons :: ClassInfo -> [TyCon]
classInfoTyCons info =
  ciTyCon info
    : concatMap (typeTyCons . TcTyVar) (ciKindTyVars info)
      <> concatMap typeTyCons (ciSuperClassTypes info)
      <> concatMap (typeSchemeTyCons . snd) (ciMethods info)
      <> concatMap (typeSchemeTyCons . snd) (ciDefaultSignatures info)

instanceInfoTyCons :: InstanceInfo -> [TyCon]
instanceInfoTyCons info =
  typeTyCons (iiDictType info)
    <> concatMap predTyCons (iiContext info)
    <> concatMap typeTyCons (iiHead info)

dataFamilyInstanceInfoTyCons :: DataFamilyInstanceInfo -> [TyCon]
dataFamilyInstanceInfoTyCons info =
  dfiiRepresentationTyCon info : typeTyCons (dfiiFamilyType info)

typeFamilyInstanceInfoTyCons :: TypeFamilyInstanceInfo -> [TyCon]
typeFamilyInstanceInfoTyCons info = typeTyCons (tfiiLeft info) <> typeTyCons (tfiiRight info)

typeSchemeTyCons :: TypeScheme -> [TyCon]
typeSchemeTyCons (ForAll _ predicates body) = concatMap predTyCons predicates <> typeTyCons body

predTyCons :: Pred -> [TyCon]
predTyCons predicate = case predicate of
  ClassPred tyCon arguments -> tyCon : concatMap typeTyCons arguments
  EqPred left right -> typeTyCons left <> typeTyCons right
  IParamPred _ payload -> typeTyCons payload
  QuantifiedPred variables antecedents consequent ->
    concatMap (typeTyCons . tvKind) variables <> concatMap predTyCons antecedents <> predTyCons consequent

typeTyCons :: TcType -> [TyCon]
typeTyCons ty = case ty of
  TcTyVar {} -> []
  TcMetaTv {} -> []
  TcTyCon tyCon arguments -> tyCon : concatMap typeTyCons arguments
  TcFunTy argument result -> typeTyCons argument <> typeTyCons result
  TcForAllTy _ body -> typeTyCons body
  TcQualTy predicates body -> concatMap predTyCons predicates <> typeTyCons body
  TcAppTy function argument -> typeTyCons function <> typeTyCons argument

writeTypeArtifact :: (String -> IO ()) -> [(Text, Text)] -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO (Text, Text)
writeTypeArtifact verbose hashes artifactPath source interface = do
  let path = artifactPath source
      name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      (artifactBytes, interfaceBytes) = encodeTypeArtifactParts (TypeArtifact name hashes Map.empty interface)
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path artifactBytes
  verbose ("Write type interface: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict interfaceBytes]))

updateScopeHashes :: Package -> ModuleExports -> Map.Map Text Text -> [SourceModule] -> Map.Map Text Text
updateScopeHashes package exports = foldl' update
  where
    update hashes source =
      let name = fromMaybe "Main" (moduleName (sourceModuleAst source))
          scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
          scopeBytes = BL.toStrict (encodeResolveScope scope)
       in Map.insert name (T.pack (stableHash [scopeBytes])) hashes

moduleDirectory :: Module -> FilePath
moduleDirectory = moduleNameDirectory . fromMaybe "Main" . moduleName

moduleNameDirectory :: Text -> FilePath
moduleNameDirectory = foldl' (</>) "" . map T.unpack . T.splitOn "."

writeArtifact :: (String -> IO ()) -> [(Text, Text)] -> ModuleExports -> Package -> FilePath -> SourceModule -> IO ()
writeArtifact verbose hashes exports package path source = do
  createDirectoryIfMissing True (takeDirectory path)
  let name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
  BL.writeFile path (encodeResolveArtifact (ResolveArtifact name hashes scope))
  verbose ("Write resolve context: " <> T.unpack name)

stableHash :: [BS.ByteString] -> String
stableHash chunks = replicate (16 - length rendered) '0' <> rendered
  where
    rendered = showHex (foldl' hashChunk (14695981039346656037 :: Word64) chunks) ""
    hashChunk :: Word64 -> BS.ByteString -> Word64
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * 1099511628211)

packageArtifactFormatVersion :: Text
packageArtifactFormatVersion = "aihc-artifacts-10"
