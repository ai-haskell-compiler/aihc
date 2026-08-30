module Aihc.Cli.InstallV2
  ( InstallV2Result (..),
    installV2,
    runInstallV2,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.ArtifactCache (loadArtifact)
import Aihc.Cli.Options (InstallV2Options (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, writePackageManifest)
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
    renderTaskTimeline,
    runTaskGraph,
  )
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeInterface)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..))
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Download qualified as HackageDownload
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
    extractInterfaceWithDeps,
    modulesInPackage,
    resolveWithDeps,
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
    renderPred,
    renderTcType,
    restrictTcInterfaceToModules,
    tcConfig,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (tyConModuleName, tyConNamespace, tyConPackageId)
import Aihc.Wasm qualified as Wasm
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.DeepSeq (rnf)
import Control.Exception (bracket, evaluate)
import Control.Monad (filterM, forM, unless, when)
import Data.Aeson (Value)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (intercalate, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Numeric (showHex)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import System.IO (hIsTerminalDevice, stdout)
import System.Process (readProcessWithExitCode)

data InstallV2Result = InstallV2Result
  { installV2StorePath :: !FilePath,
    installV2WrittenModules :: ![Text],
    installV2ReusedModules :: ![Text]
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

data InstalledV2Package = InstalledV2Package
  { installedV2Result :: !InstallV2Result,
    installedV2Exports :: !ModuleExports,
    installedV2Types :: !(Map.Map Text TcInterface),
    installedV2ScopeHashes :: !(Map.Map Text Text),
    installedV2TypeHashes :: !(Map.Map Text Text)
  }

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
    nativeSource :: !Text
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
    resolveUnitChanged :: !Bool,
    resolveUnitSuccess :: !Bool
  }

data TypeUnitResult = TypeUnitResult
  { typeUnitTypes :: !(Map.Map Text TcInterface),
    typeUnitHashes :: !(Map.Map Text Text),
    typeUnitComplete :: !TcInterface,
    typeUnitLocalInterface :: !TcInterface,
    typeUnitBackendInterface :: !TcInterface,
    typeUnitDiagnostics :: ![TcDiagnostic],
    typeUnitWritten :: !(Set.Set Text),
    typeUnitReused :: !(Set.Set Text),
    typeUnitPendingCompile :: !(Maybe PendingCompile),
    typeUnitSuccess :: !Bool
  }

data UnitRuntime = UnitRuntime
  { runtimeUnit :: !SourceUnit,
    runtimeResolveResult :: !(TMVar ResolveUnitResult),
    runtimeTypeResult :: !(TMVar TypeUnitResult),
    runtimePreparedDesugar :: !(TMVar (Maybe Fc.PreparedDesugar))
  }

data InstallConfig = InstallConfig
  { installKeepGrin :: !Bool,
    installKeepNative :: !Bool,
    installLint :: !Bool,
    installNoCode :: !Bool,
    installTarget :: !NativeTarget,
    installVerbose :: String -> IO (),
    installPrintTimings :: String -> IO (),
    installUseColor :: !Bool,
    installArtifactCache :: !Bool
  }

data PackageTaskContext = PackageTaskContext
  { taskInstallConfig :: !InstallConfig,
    taskStorePath :: !FilePath,
    taskResolvePackage :: !Package,
    taskPrimIdentity :: !PackageId,
    taskPackageRoot :: !FilePath,
    taskDependencyExports :: !ModuleExports,
    taskDependencyScopeHashes :: !(Map.Map Text Text),
    taskDependencyTypes :: !(Map.Map Text TcInterface),
    taskDependencyTypeHashes :: !(Map.Map Text Text),
    taskDependencyPreparedDesugar :: !Fc.PreparedDesugar
  }

runInstallV2 :: InstallV2Options -> IO ()
runInstallV2 options = do
  result <- installV2 options
  putStrLn ("store: " <> installV2StorePath result)

installV2 :: InstallV2Options -> IO InstallV2Result
installV2 options = do
  storeRoot <- maybe defaultStoreRoot pure (installV2StoreRoot options)
  useColor <- hIsTerminalDevice stdout
  let target = installV2Target options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
  let root = installV2PackageDirectory options
      verbose message = when (installV2Verbose options) (putStrLn message)
      printTimings message = when (installV2PrintTimings options) (putStrLn message)
      fallbackResolver = networkDependencyResolver
      resolver = localDependencyResolverWithFallback fallbackResolver root
      config =
        InstallConfig
          { installKeepGrin = installV2KeepGrin options,
            installKeepNative = installV2KeepNative options,
            installLint = installV2Lint options,
            installNoCode = installV2NoCode options,
            installTarget = target,
            installVerbose = verbose,
            installPrintTimings = printTimings,
            installUseColor = useColor,
            installArtifactCache = not (installV2NoCache options)
          }
  spec <- packageSpecFromSource root
  plan <- buildPackagePlanWithResolver resolver spec
  installedV2Result <$> installPackagePlanV2 config targetStoreRoot plan

networkDependencyResolver :: DependencyResolver
networkDependencyResolver =
  DependencyResolver
    { resolverResolveVersion = resolveVersion,
      resolverSourcePath = HackageDownload.downloadPackageWithOptions HackageDownload.defaultDownloadOptions
    }
  where
    resolveVersion name = do
      result <- getLatestVersion Nothing name
      either (ioError . userError) pure result

installPackagePlanV2 :: InstallConfig -> FilePath -> PackagePlan -> IO InstalledV2Package
installPackagePlanV2 config storeRoot plan = do
  dependencies <- mapM (installPackagePlanV2 config storeRoot) (planDependencyPlans plan)
  installPackageV2 config storeRoot dependencies (planSourcePath plan)

installPackageV2 :: InstallConfig -> FilePath -> [InstalledV2Package] -> FilePath -> IO InstalledV2Package
installPackageV2 config storeRoot dependencies root = do
  let target = installTarget config
      verbose = installVerbose config
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
  verbose ("Parse " <> show (length files) <> " library modules")
  capabilities <- getNumCapabilities
  (parsed, importTimings) <- loadSourceModules (max 1 capabilities) root files
  let dependencyIdentities = sortOn id (map (T.pack . takeFileName . installV2StorePath . installedV2Result) dependencies)
      packageHash = stableHash (map TE.encodeUtf8 ("aihc-dependencies-v2" : dependencyIdentities))
      packageDirectory = T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash
      storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId (T.pack packageDirectory))
      units = sourceModuleUnits parsed
      dependencyExports = Map.unions (map installedV2Exports dependencies)
      dependencyTypes = Map.unions (map installedV2Types dependencies)
      dependencyScopeHashes = Map.unions (map installedV2ScopeHashes dependencies)
      dependencyTypeHashes = Map.unions (map installedV2TypeHashes dependencies)
      primIdentity =
        fromMaybe (PackageId "aihc-prim") $
          if packageName resolvePackage == "aihc-prim"
            then Just resolvePackageIdentity
            else
              listToMaybe
                [ dependencyIdentity
                | ModuleKey (Package dependencyName dependencyIdentity) _ <- Map.keys dependencyExports,
                  dependencyName == "aihc-prim"
                ]
      resolvePackageIdentity = case resolvePackage of Package _ identity -> identity
  dependencyPreparedDesugar <-
    either (ioError . userError . ("FC environment generation failed: " <>)) pure $
      Fc.prepareDesugar
        (DesugarConfig primIdentity)
        (if installNoCode config then mempty else mergeTcInterfaces (Map.elems dependencyTypes))
  let taskContext =
        PackageTaskContext
          { taskInstallConfig = config,
            taskStorePath = storePath,
            taskResolvePackage = resolvePackage,
            taskPrimIdentity = primIdentity,
            taskPackageRoot = root,
            taskDependencyExports = dependencyExports,
            taskDependencyScopeHashes = dependencyScopeHashes,
            taskDependencyTypes = dependencyTypes,
            taskDependencyTypeHashes = dependencyTypeHashes,
            taskDependencyPreparedDesugar = dependencyPreparedDesugar
          }
  verbose ("Compute " <> show (length units) <> " SCC units")
  (runtimes, taskTimings) <-
    runPackageTasks
      taskContext
      (max 1 capabilities)
      units
  resolveResults <- mapM (atomically . readTMVar . runtimeResolveResult) runtimes
  installPrintTimings config (renderTaskTimeline (installUseColor config) (importTimings <> taskTimings))
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
      allTypes = localTypes `Map.union` dependencyTypes
      allTypeHashes = localTypeHashes `Map.union` dependencyTypeHashes
      written = Set.unions (map typeUnitWritten typeResults)
      reused = Set.unions (map typeUnitReused typeResults)
      completeTypes = mergeTcInterfaces (Map.elems allTypes)
      packageInstances = mergeTcInterfaces (map (instanceFacts . typeUnitComplete) typeResults)
  writePackageInstanceArtifact verbose storePath allTypeHashes completeTypes (ownInstanceFacts resolvePackage packageInstances)
  unless (installNoCode config) $ do
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
            [ T.pack (takeFileName (installV2StorePath (installedV2Result dependency)))
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
    InstalledV2Package
      { installedV2Result = InstallV2Result storePath (Set.toAscList written) (Set.toAscList reused),
        installedV2Exports = ownExports,
        installedV2Types = Map.restrictKeys allTypes exposedNames,
        installedV2ScopeHashes = Map.restrictKeys allScopeHashes exposedNames,
        installedV2TypeHashes = Map.restrictKeys allTypeHashes exposedNames
      }

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
    OccursCheckError unique ty ->
      "occurs check failed: " <> show unique <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    OtherError message ->
      message

runPackageTasks :: PackageTaskContext -> Int -> [SourceUnit] -> IO ([UnitRuntime], [TaskTiming])
runPackageTasks context workers units = do
  runtimes <-
    forM units $ \unit ->
      UnitRuntime unit <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO
  let runtimeMap = Map.fromList [(sourceUnitId (runtimeUnit runtime), runtime) | runtime <- runtimes]
      tasks = concatMap (unitTasks runtimeMap) runtimes
  timings <- runTaskGraph workers tasks
  pure (runtimes, timings)
  where
    unitTasks runtimeMap runtime =
      map (parseTask runtime) (sourceUnitSources unit)
        <> [resolveTask runtimeMap runtime, typeTask runtimeMap runtime]
        <> if installNoCode config
          then []
          else [prepareTask runtimeMap runtime, backendTask runtime]
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
          taskDependencies = Set.singleton (prepareTaskId (runtimeUnit runtime)),
          taskAction = runBackendUnit context runtime
        }

    prepareTask runtimeMap runtime =
      Task
        { taskId = prepareTaskId (runtimeUnit runtime),
          taskKind = TaskBackend,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( typeTaskId (runtimeUnit runtime)
                  : map (prepareTaskId . runtimeUnit . lookupRuntime runtimeMap) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction = runPrepareUnit context runtimeMap runtime
        }
    config = taskInstallConfig context

parseTaskId :: SourceModule -> TaskId
parseTaskId = TaskId . ("parse:" <>) . sourceModulePath

resolveTaskId :: SourceUnit -> TaskId
resolveTaskId = TaskId . ("resolve:" <>) . T.unpack . unitLabel

typeTaskId :: SourceUnit -> TaskId
typeTaskId = TaskId . ("type-check:" <>) . T.unpack . unitLabel

prepareTaskId :: SourceUnit -> TaskId
prepareTaskId = TaskId . ("prepare-fc:" <>) . T.unpack . unitLabel

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
      verbose = installVerbose config
      cache = installArtifactCache config
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
  cachedExports <- readUnitArtifacts cache hashes resolvePackage resolvePath sources
  (unitExports, resolved, errors, changed) <- case cachedExports of
    Just exports -> do
      mapM_ (verbose . ("Reuse resolve context: " <>) . T.unpack) unitNames
      pure (exports, Nothing, [], False)
    Nothing -> do
      let result = resolveWithDeps availableExports packageModules
          resultErrors = resolveErrors result
          exports = extractInterfaceWithDeps availableExports result
          success = parseSuccess && dependenciesSucceeded && null resultErrors
      when success (mapM_ (\source -> writeArtifact verbose hashes exports resolvePackage (resolvePath source) source) sources)
      pure (exports, Just result, resultErrors, True)
  let ownScopeHashes = updateScopeHashes resolvePackage unitExports Map.empty sources
      success = parseSuccess && dependenciesSucceeded && null errors
  atomically $
    putTMVar
      (runtimeResolveResult runtime)
      ResolveUnitResult
        { resolveUnitExports = unitExports,
          resolveUnitScopeHashes = ownScopeHashes,
          resolveUnitResolved = resolved,
          resolveUnitErrors = errors,
          resolveUnitChanged = changed,
          resolveUnitSuccess = success
        }
  where
    config = taskInstallConfig context
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
      target = installTarget config
      verbose = installVerbose config
      cache = installArtifactCache config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredTypeModules)
      availableTypes = Map.unions (map typeUnitTypes dependencyResults) `Map.union` dependencyTypes
      availableTypeHashes = Map.unions (map typeUnitHashes dependencyResults) `Map.union` dependencyTypeHashes
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
      outputPaths = moduleOutputPaths storePath target
      importedInstances =
        mergeTcInterfaces
          ( instanceFacts (mergeTcInterfaces (Map.elems dependencyTypes))
              : map (instanceFacts . typeUnitComplete) dependencyResults
          )
      importedTypes =
        applyInstanceFacts
          importedInstances
          ( mergeTcInterfaces
              [ interface
              | name <- dependencyNames,
                name `notElem` unitNames,
                Just interface <- [Map.lookup name availableTypes]
              ]
          )
      checkUnit = do
        resolved <-
          case resolveUnitResolved resolvedOutput of
            Just result -> pure result
            Nothing -> pure (resolveWithDeps availableExports (modulesInPackage resolvePackage (map sourceModuleAst sources)))
        let checked =
              typecheckModuleSccWithInterface
                (tcConfig primIdentity)
                importedTypes
                (map snd (resolvedModules resolved))
        _ <- evaluate (sum (map (length . tcModuleDiagnostics) (fst checked)))
        pure checked
      dependencySuccess = all typeUnitSuccess dependencyResults
      resolveSuccess = resolveUnitSuccess resolvedOutput
  cachedTypes <-
    if resolveSuccess && dependencySuccess
      then readTypeArtifacts cache typeInputs typePath sources
      else pure Nothing
  (unitTypes, typeChanged, initialChecked) <- case cachedTypes of
    Just interfaces -> do
      mapM_ (verbose . ("Reuse type interface: " <>) . T.unpack) unitNames
      pure (interfaces, False, Nothing)
    Nothing -> do
      checked@(_, completeInterface) <- checkUnit
      let interfaces = map (moduleTypeInterface (resolveUnitExports resolvedOutput) resolvePackage completeInterface) sources
      pure (interfaces, True, Just checked)
  let diagnostics = maybe [] (concatMap tcModuleDiagnostics . fst) initialChecked
      typeSuccess = maybe True (all tcModuleSuccess . fst) initialChecked
      success = resolveSuccess && dependencySuccess && typeSuccess
  when (typeChanged && success) $
    mapM_ (uncurry (writeTypeArtifact verbose typeInputs typePath)) (zip sources unitTypes)
  let ownTypeHashes = updateTypeHashes Map.empty (zip unitNames unitTypes)
  (fcChanged, generatedOutputChanged, pendingCompile) <-
    if installNoCode config || not success
      then pure (False, False, Nothing)
      else do
        if typeChanged
          then do
            (checkedModules, _) <- maybe checkUnit pure initialChecked
            pure (True, installKeepGrin config, Just (PendingCompile True checkedModules))
          else do
            fcExists <- and <$> mapM (doesFileExist . outputFcPath . outputPaths . sourceName) sources
            grinStagesExist <-
              and
                <$> mapM
                  ( \source -> do
                      let paths = outputPaths (sourceName source)
                      and <$> mapM doesFileExist [outputGrinPath paths, outputCpsGrinPath paths, outputGcGrinPath paths]
                  )
                  sources
            objectExists <- and <$> mapM (doesFileExist . outputObjectPath . outputPaths . sourceName) sources
            nativeExists <- and <$> mapM (doesFileExist . outputNativePath . outputPaths . sourceName) sources
            if not fcExists || installLint config || repairRequired grinStagesExist nativeExists objectExists
              then do
                (checkedModules, _) <- maybe checkUnit pure initialChecked
                pure (not fcExists, repairRequired grinStagesExist nativeExists objectExists, Just (PendingCompile (not fcExists) checkedModules))
              else do
                mapM_ (verbose . ("Reuse FC: " <>) . T.unpack) unitNames
                pure (False, False, Nothing)
  let cachedInterface = mergeTcInterfaces unitTypes
      cachedInstances = mergeTcInterfaces [importedInstances, instanceFacts cachedInterface]
      completeInterface = maybe (applyInstanceFacts cachedInstances cachedInterface) snd initialChecked
      localInterface = restrictTcInterfaceToModules (packageId resolvePackage) unitNames completeInterface
      availableBackendFacts =
        mergeTcInterfaces
          (completeInterface : Map.elems availableTypes <> map typeUnitBackendInterface dependencyResults)
      backendInterface = addReferencedFacts availableBackendFacts completeInterface
      changed = resolveUnitChanged resolvedOutput || typeChanged || fcChanged || generatedOutputChanged
      unitSet = Set.fromList unitNames
      written' = if changed then written <> unitSet else written
      reused' = if changed then reused else reused <> unitSet
      written = Set.empty
      reused = Set.empty
  atomically $
    putTMVar
      (runtimeTypeResult runtime)
      TypeUnitResult
        { typeUnitTypes = Map.fromList (zip unitNames unitTypes),
          typeUnitHashes = ownTypeHashes,
          typeUnitComplete = completeInterface,
          typeUnitLocalInterface = localInterface,
          typeUnitBackendInterface = backendInterface,
          typeUnitDiagnostics = diagnostics,
          typeUnitWritten = written',
          typeUnitReused = reused',
          typeUnitPendingCompile = pendingCompile,
          typeUnitSuccess = success
        }
  where
    config = taskInstallConfig context
    unit = runtimeUnit runtime
    repairRequired grinStagesExist nativeExists objectExists =
      (installKeepGrin config && not grinStagesExist) || (installKeepNative config && not nativeExists) || not objectExists

runPrepareUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runPrepareUnit context runtimes runtime = do
  result <- atomically (readTMVar (runtimeTypeResult runtime))
  dependencyResults <- readDependencyResults runtimePreparedDesugar runtimes (sourceUnitDependencies (runtimeUnit runtime))
  prepared <-
    if typeUnitSuccess result
      then do
        let availableDependencies = catMaybes dependencyResults
            dependencies =
              if null availableDependencies
                then [taskDependencyPreparedDesugar context]
                else availableDependencies
        Just
          <$> either
            (ioError . userError . ("FC environment generation failed: " <>))
            pure
            ( Fc.prepareDesugarIncrementalWithAvailable
                (DesugarConfig (taskPrimIdentity context))
                dependencies
                (typeUnitLocalInterface result)
                (typeUnitBackendInterface result)
            )
      else pure Nothing
  atomically (putTMVar (runtimePreparedDesugar runtime) prepared)

runBackendUnit :: PackageTaskContext -> UnitRuntime -> IO ()
runBackendUnit context runtime = do
  result <- atomically (readTMVar (runtimeTypeResult runtime))
  prepared <- atomically (readTMVar (runtimePreparedDesugar runtime))
  case (typeUnitPendingCompile result, prepared) of
    (Just pending, Just preparedDesugar) -> do
      let config = taskInstallConfig context
          storePath = taskStorePath context
      compileCheckedModules
        config
        (pendingWriteFc pending)
        (installVerbose config)
        preparedDesugar
        (moduleOutputPaths storePath (installTarget config))
        (pendingModules pending)
    _ -> pure ()

applyInstanceFacts :: TcInterface -> TcInterface -> TcInterface
applyInstanceFacts instances direct =
  direct
    { tcInterfaceInstances = tcInterfaceInstances instances,
      tcInterfaceDataFamilyInstances = tcInterfaceDataFamilyInstances instances,
      tcInterfaceTypeFamilyInstances = tcInterfaceTypeFamilyInstances instances
    }

instanceFacts :: TcInterface -> TcInterface
instanceFacts interface =
  mempty
    { tcInterfaceInstances = tcInterfaceInstances interface,
      tcInterfaceDataFamilyInstances = tcInterfaceDataFamilyInstances interface,
      tcInterfaceTypeFamilyInstances = tcInterfaceTypeFamilyInstances interface
    }

ownInstanceFacts :: Package -> TcInterface -> TcInterface
ownInstanceFacts package interface =
  mempty
    { tcInterfaceInstances = filter ((== packageIdText identity) . fst . iiDictOrigin) (tcInterfaceInstances interface),
      tcInterfaceDataFamilyInstances = filter (localTyCon . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstances interface),
      tcInterfaceTypeFamilyInstances = filter (any localTyCon . typeFamilyInstanceInfoTyCons) (tcInterfaceTypeFamilyInstances interface)
    }
  where
    identity = packageId package
    localTyCon tyCon = tyConPackageId tyCon == identity

writePackageInstanceArtifact :: (String -> IO ()) -> FilePath -> Map.Map Text Text -> TcInterface -> TcInterface -> IO ()
writePackageInstanceArtifact verbose storePath typeHashes complete interface = do
  let path = storePath </> "instances.cbor"
      hashes = sortOn fst [("type:" <> name, digest) | (name, digest) <- Map.toList typeHashes]
      artifactInterface = addReferencedFacts complete interface
  createDirectoryIfMissing True storePath
  BL.writeFile path (encodeTypeArtifact (TypeArtifact "$package-instances" hashes artifactInterface))
  verbose ("Write package instances: " <> path)

wiredTypeModules :: [Text]
wiredTypeModules = ["GHC.Base", "GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Tuple", "GHC.Types"]

compileCheckedModules :: InstallConfig -> Bool -> (String -> IO ()) -> Fc.PreparedDesugar -> (Text -> ModuleOutputPaths) -> [Module] -> IO ()
compileCheckedModules config writeFc verbose prepared outputPaths checkedModules = do
  let keepGrin = installKeepGrin config
      keepNative = installKeepNative config
      lint = installLint config
      target = installTarget config
      bindings = concatMap tcModuleBindings checkedModules
      desugarResults = map (Fc.desugarModuleFcPrepared prepared bindings) checkedModules
  unless (all dsSuccess desugarResults) (ioError (userError ("FC generation failed: " <> unlines (concatMap dsErrors desugarResults))))
  let moduleNames = map (fromMaybe "Main" . moduleName) checkedModules
      fcModules = zipWith FcModule moduleNames (map dsProgram desugarResults)
      fcErrors = concatMap (Fc.lintProgram . fcProgram) fcModules
      fcReport = map (("    " <>) . show) fcErrors
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
  let (emptyFcModules, nonemptyFcModules) = spanEmptyModules fcModules
  mapM_ writeEmptyModule emptyFcModules
  grinModules <- mapM lowerGrinModule nonemptyFcModules
  when keepGrin (mapM_ writeGrinModule grinModules)
  nativeModules <- mapM (generateNativeModule target) grinModules
  mapM_ writeNativeSourceFile nativeModules
  mapM_ compileNativeSourceFile nativeModules
  unless keepNative (mapM_ removeNativeSourceFile nativeModules)
  where
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
      when (installKeepGrin config) $ do
        writeFile (outputGrinPath paths) ""
        writeFile (outputCpsGrinPath paths) ""
        writeFile (outputGcGrinPath paths) ""
      when (installKeepNative config) (writeFile (outputNativePath paths) "")
      verbose ("Write empty object: " <> T.unpack name)

    writeFcModule fcModule = do
      let name = fcModuleName fcModule
          path = outputFcPath (outputPaths name)
      writeFcFile path (fcProgram fcModule)
      verbose ("Write FC: " <> T.unpack name)

    writeFcFile path program = do
      let output = withFinalNewline (Fc.renderProgram program)
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path output

    lowerGrinModule fcModule = do
      plainProgram <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram (fcProgram fcModule))
      when (installLint config) $ do
        let plainErrors = Grin.lintProgram plainProgram
        unless (null plainErrors) (ioError (userError ("GRIN lint failed: " <> show plainErrors)))
      cpsProgram <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin plainProgram)
      let gcProgram = Grin.lowerGc cpsProgram
      when (installLint config) $ do
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
      source <- generateNativeCode selectedTarget (gcGrinProgram grinModule)
      pure (NativeModule (grinModuleName grinModule) source)

    writeNativeSourceFile nativeModule = do
      let name = nativeModuleName nativeModule
          path = outputNativePath (outputPaths name)
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path (nativeSource nativeModule)
      verbose ("Write native source: " <> T.unpack name)

    compileNativeSourceFile nativeModule = do
      let name = nativeModuleName nativeModule
          paths = outputPaths name
      (compiler, compilerArguments) <- backendCompiler (installTarget config)
      runTool compiler (compilerArguments <> ["-c", outputNativePath paths, "-o", outputObjectPath paths])
      verbose ("Write object: " <> T.unpack name)

    removeNativeSourceFile = removeFile . outputNativePath . outputPaths . nativeModuleName

generateNativeCode :: NativeTarget -> Grin.GcGrinProgram -> IO Text
generateNativeCode target gcProgram =
  case target of
    AppleArm64 -> either (ioError . userError . ("Apple ARM64 generation failed: " <>) . show) pure (Arm64.compileModule gcProgram)
    LinuxAmd64 -> either (ioError . userError . ("Linux AMD64 generation failed: " <>) . show) pure (Amd64.compileModule gcProgram)
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
    _ -> ".s"

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
        tcInterfaceTypeFamilyInstances = filter visibleTypeFamilyInstance (tcInterfaceTypeFamilyInstances interface)
      }
  where
    name = fromMaybe "Main" (moduleName (sourceModuleAst source))
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
    localTyCon tyCon = tyConPackageId tyCon == packageId package && tyConModuleName tyCon == name
    visibleTerm (TcTermGlobal packageId' moduleName' identifier, _) =
      let identity = (packageId', moduleName', identifier)
       in Map.member identifier (scopeTerms scope) || identity `Set.member` termIdentities || identity == localIdentity identifier
    visibleTerm (TcTermLocal {}, _) = False
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
    { tcInterfaceTerms = tcInterfaceTerms interface,
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
    referenced =
      Set.fromList
        ( concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface)
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
    : concatMap typeTyCons (ciSuperClassTypes info)
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

typeTyCons :: TcType -> [TyCon]
typeTyCons ty = case ty of
  TcTyVar {} -> []
  TcMetaTv {} -> []
  TcTyCon tyCon arguments -> tyCon : concatMap typeTyCons arguments
  TcFunTy argument result -> typeTyCons argument <> typeTyCons result
  TcForAllTy _ body -> typeTyCons body
  TcQualTy predicates body -> concatMap predTyCons predicates <> typeTyCons body
  TcAppTy function argument -> typeTyCons function <> typeTyCons argument

writeTypeArtifact :: (String -> IO ()) -> [(Text, Text)] -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO ()
writeTypeArtifact verbose hashes artifactPath source interface = do
  let path = artifactPath source
      name = fromMaybe "Main" (moduleName (sourceModuleAst source))
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encodeTypeArtifact (TypeArtifact name hashes interface))
  verbose ("Write type interface: " <> T.unpack name)

readTypeArtifacts :: Bool -> [(Text, Text)] -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe [TcInterface])
readTypeArtifacts cache expected artifactPath unit = fmap sequence (mapM readOne unit)
  where
    readOne source = do
      let path = artifactPath source
      fmap typeArtifactInterface <$> loadArtifact cache path decodeTypeArtifact ((== expected) . typeArtifactInputHashes)

updateTypeHashes :: Map.Map Text Text -> [(Text, TcInterface)] -> Map.Map Text Text
updateTypeHashes = foldl' insertHash
  where
    insertHash result (name, interface) =
      Map.insert name (T.pack (stableHash [BL.toStrict (encodeTypeInterface interface)])) result

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

readUnitArtifacts :: Bool -> [(Text, Text)] -> Package -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe ModuleExports)
readUnitArtifacts cache expected package artifactPath unit = do
  entries <- mapM readOne unit
  pure (Map.fromList <$> sequence entries)
  where
    readOne source = do
      let path = artifactPath source
      artifact <- loadArtifact cache path decodeResolveArtifact ((== expected) . resolveArtifactInputHashes)
      pure ((\value -> (ModuleKey package (resolveArtifactModuleName value), resolveArtifactScope value)) <$> artifact)

stableHash :: [BS.ByteString] -> String
stableHash chunks = replicate (16 - length rendered) '0' <> rendered
  where
    rendered = showHex (foldl' hashChunk (14695981039346656037 :: Word64) chunks) ""
    hashChunk :: Word64 -> BS.ByteString -> Word64
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * 1099511628211)
