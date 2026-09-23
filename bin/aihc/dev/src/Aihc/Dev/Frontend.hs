-- | Run the front end of the compiler over packages one phase at a time,
-- and say how long each phase took.
--
-- An install interleaves the phases per unit in one task graph, so no
-- phase has a duration of its own there. This driver runs preprocessing,
-- parsing, name resolution and type checking as four consecutive phases
-- over the whole package, each one on every unit before the next starts,
-- and stops at the first phase that reports an error.
--
-- The packages are processed in the order given, and each one is
-- resolved and checked against the packages before it on the command line
-- and nothing else: the core libraries are not added, so a package that
-- needs them is preceded by them.
module Aihc.Dev.Frontend
  ( FrontendOptions (..),
    runFrontend,
    resolveFrontendTarget,
  )
where

import Aihc.Cli.CompilerHeaders (ensureCompilerHeaders)
import Aihc.Cli.Install
  ( InstanceProvider,
    ModuleCompileConfig (..),
    PackageInputs (..),
    SourceModule (..),
    SourceUnit (..),
    UnitId (..),
    addReferencedFacts,
    buildEnvironmentIdentity,
    builtinFunctionScope,
    configMergeCheck,
    configurePackage,
    excerptSourceLoader,
    instanceFacts,
    interfaceInstanceProviders,
    moduleTypeInterface,
    packagePrimIdentity,
    parsePackageTarget,
    parseSource,
    preprocessPackage,
    primKinds,
    readPackageInputs,
    renderFrontendFailure,
    selectInstanceProviders,
    sourceDependencyNames,
    sourceModuleUnits,
    takePackageModuleUnits,
    typeLiteralKindTyCons,
    typeLiteralSupportTerms,
    unitLabel,
    wiredInterfaceModules,
  )
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Cli.TaskGraph (Task (..), TaskId (..), TaskKind (..), renderDuration, runTaskGraph)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Native (NativeTarget, defaultOptimizationLevel, hostNativeTarget, nativeTargetStoreDirectory)
import Aihc.PackagePlan (DependencyVersions, PackagePlan (..), PlanOrigin (..), dependencyVersionsFromManifests, parseSourcePackageDescriptionAt)
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap)
import Aihc.Prim.Wiring (primTcConfig)
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    ModuleUnit (..),
    Package (..),
    PackageId (..),
    ResolveError,
    ResolveResult (..),
    collectModuleExportsWithDeps,
    filterModuleExports,
    resolveUnit,
  )
import Aihc.Tc
  ( MergeCheck (..),
    TcDiagnostic (..),
    TcInterface,
    TcSeverity (..),
    mergeTcInterfaces,
    tcModuleDiagnostics,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Share (shareTcInterface)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.DeepSeq (force, rnf)
import Control.Exception (bracket, evaluate)
import Control.Monad (foldM, unless, when)
import Data.Aeson (Value)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Flag (mkFlagAssignment)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Temp (mkdtemp)

data FrontendOptions = FrontendOptions
  { -- | Package directories and versioned Hackage names, in the order
    -- they are processed.
    frontendPackages :: ![String],
    -- | How many units a phase works on at once; the capabilities of the
    -- process when not given.
    frontendJobs :: !(Maybe Int),
    frontendVerbose :: !Bool,
    -- | The target whose headers the preprocessor and CPP see; the host
    -- when not given.
    frontendTarget :: !(Maybe NativeTarget)
  }

-- | What the packages after one on the command line see of it: the same
-- view an installed package gives, without the artifacts.
data CheckedPackage = CheckedPackage
  { checkedName :: !Text,
    checkedVersion :: !Text,
    checkedExports :: !ModuleExports,
    checkedTypes :: !(Map.Map Text TcInterface),
    checkedInstanceFacts :: !TcInterface,
    checkedInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider))
  }

-- | The wall-clock time of each phase, in nanoseconds.
data PhaseTimes = PhaseTimes
  { preprocessNs :: !Word64,
    parseNs :: !Word64,
    resolveNs :: !Word64,
    typecheckNs :: !Word64
  }

instance Semigroup PhaseTimes where
  left <> right =
    PhaseTimes
      { preprocessNs = preprocessNs left + preprocessNs right,
        parseNs = parseNs left + parseNs right,
        resolveNs = resolveNs left + resolveNs right,
        typecheckNs = typecheckNs left + typecheckNs right
      }

instance Monoid PhaseTimes where
  mempty = PhaseTimes 0 0 0 0

-- | A unit after name resolution: what the units above it import from it
-- and the resolved modules the type checker takes.
data ResolvedUnit = ResolvedUnit
  { resolvedUnit :: !SourceUnit,
    resolvedUnitExports :: !ModuleExports,
    resolvedUnitResult :: !ResolveResult
  }

-- | A unit after type checking: the interface of each of its modules, the
-- instance facts it declares, the instance closure it saw plus its own, and
-- what the checker reported.
data CheckedUnit = CheckedUnit
  { checkedUnitTypes :: !(Map.Map Text TcInterface),
    checkedUnitOwnFacts :: !TcInterface,
    checkedUnitInstanceInterface :: !TcInterface,
    checkedUnitDiagnostics :: ![(Text, TcDiagnostic)]
  }

runFrontend :: FrontendOptions -> IO ()
runFrontend options = do
  target <-
    case frontendTarget options of
      Just chosen -> pure chosen
      Nothing -> maybe (die "This host is not a supported target; pass --target") pure hostNativeTarget
  -- Every argument is checked, and every release fetched, before any work
  -- starts, so a mistake in the last argument does not cost the others.
  roots <- mapM resolveFrontendTarget (frontendPackages options)
  capabilities <- getNumCapabilities
  let jobs = max 1 (fromMaybe capabilities (frontendJobs options))
  storeRoot <- defaultStoreRoot
  headerDirectory <- ensureCompilerHeaders target (storeRoot </> nativeTargetStoreDirectory target)
  buildIdentity <- buildEnvironmentIdentity target
  let config =
        ModuleCompileConfig
          { compileBuildIdentity = buildIdentity,
            compileKeepCore = False,
            compileKeepGrin = False,
            compileKeepLir = False,
            compileKeepNative = False,
            compileLint = False,
            compileCheckPrimBounds = False,
            compileLto = False,
            compilePasses = [],
            compileGrinPointsTo = False,
            compileNoCode = True,
            compileOptimization = defaultOptimizationLevel,
            compileTarget = target,
            compileHeaderDirectory = headerDirectory,
            compileVerbose = when (frontendVerbose options) . hPutStrLn stderr,
            compilePrintTimings = const (pure ()),
            compileUseColor = False
          }
  (_, totals) <-
    foldM
      ( \(checked, totals) root -> do
          (package, times) <- runPackage config jobs headerDirectory checked root
          pure (checked <> [package], totals <> times)
      )
      ([], mempty)
      roots
  when (length roots > 1) $ do
    putStrLn "total"
    reportTimes totals

-- | Turn a command-line argument into a package directory.
--
-- An existing directory is used as-is. Anything else must be a Hackage
-- package with a version, @NAME-VERSION@, which is downloaded into the
-- Hackage cache once and read from there afterwards. A name without a
-- version is refused rather than resolved to the preferred version, so a
-- run names exactly the sources it measured.
resolveFrontendTarget :: String -> IO FilePath
resolveFrontendTarget argument = do
  isDirectory <- doesDirectoryExist argument
  if isDirectory
    then pure argument
    else case parsePackageTarget argument of
      Just (name, Just version) ->
        HackageDownload.downloadPackageWithOptions
          HackageDownload.defaultDownloadOptions
          PackageSpec {pkgName = name, pkgVersion = version}
      Just (_, Nothing) ->
        die (argument <> " is not an existing directory, and a Hackage package needs its version (NAME-VERSION)")
      Nothing ->
        die (argument <> " is not an existing directory nor a Hackage package (NAME-VERSION)")

-- | Run the four phases over one package against the packages before it.
runPackage :: ModuleCompileConfig -> Int -> FilePath -> [CheckedPackage] -> FilePath -> IO (CheckedPackage, PhaseTimes)
runPackage config jobs headerDirectory dependencies root = do
  (cabalFile, gpd) <- parseSourcePackageDescriptionAt root
  let plan =
        PackagePlan
          { planName = CabalPackage.packageName (package (packageDescription gpd)),
            planSourcePath = root,
            planCabalFile = cabalFile,
            planDescription = gpd,
            planOrigin = PlanLocal,
            planRevision = Nothing,
            planFlags = mkFlagAssignment [],
            planDependencyPlans = []
          }
  inputs <- readPackageInputs config plan
  let (name, version) = packageNameAndVersion gpd
      identity = name <> "-" <> version
      resolvePackage = Package name (PackageId identity)
      versions = dependencyVersionsFromManifests [(checkedName dependency, checkedVersion dependency) | dependency <- dependencies]
  putStrLn (T.unpack identity <> " (" <> root <> ")")
  hFlush stdout
  withScratchDirectory $ \scratch -> do
    -- Preprocess: run configure when the package has a script, then turn
    -- each @.hsc@ source into a Haskell module. Both write under the
    -- scratch directory, so nothing is reused from a previous run.
    (files, preprocessTime) <- timed $ do
      (configured, cInfo) <- configurePackage config root scratch name inputs
      preprocessPackage config versions root scratch (inputConfigureScript inputs) "" cInfo configured
    let preprocessed = length (filter (isJust . HackageCabal.fileInfoPreprocessor) (inputSources inputs))
    reportPhase "preprocess" preprocessTime (show preprocessed <> " " <> plural preprocessed "file")
    let loader = excerptSourceLoader headerDirectory root versions files
    -- Parse: every module, in parallel, forced to the last leaf.
    (sources, parseTime) <- timed (parseModules jobs headerDirectory root versions files)
    reportPhase "parse" parseTime (show (length sources) <> " " <> plural (length sources) "module")
    stopOnFailure loader (concatMap sourceModuleParseDiagnostics sources) [] []
    -- Resolve: every unit, in dependency order, against the exports of
    -- the units below it and of the packages before this one.
    let units = sourceModuleUnits sources
        dependencyExports = mconcat (map checkedExports dependencies)
    (resolved, resolveTime) <- timed (resolveUnits jobs resolvePackage dependencyExports units)
    reportPhase "resolve" resolveTime (show (length units) <> " " <> plural (length units) "unit")
    stopOnFailure loader [] (concatMap (resolveErrors . resolvedUnitResult) resolved) []
    -- Type check: every unit, in dependency order, against the interfaces
    -- of what it imports.
    let primIdentity = packagePrimIdentity resolvePackage dependencyExports
        dependencyTypes = Map.unions (map checkedTypes dependencies)
        dependencyInstanceFacts = mergeTcInterfaces (configMergeCheck config) (map checkedInstanceFacts dependencies)
        dependencyInstanceProviders = Map.unions (map checkedInstanceProviders dependencies)
    (checked, typecheckTime) <-
      timed (typecheckUnits jobs config resolvePackage primIdentity dependencyTypes dependencyInstanceFacts dependencyInstanceProviders resolved)
    reportPhase "typecheck" typecheckTime ""
    stopOnFailure loader [] [] [diagnostic | unit <- checked, diagnostic@(_, TcDiagnostic {diagSeverity = TcError}) <- checkedUnitDiagnostics unit]
    let exposedNames = Set.fromList (HackageCabal.collectLibraryExposedModules gpd)
        ownExports =
          filterModuleExports
            (\moduleKey -> moduleKeyPackage moduleKey == resolvePackage && moduleKeyName moduleKey `Set.member` exposedNames)
            (mconcat (map resolvedUnitExports resolved))
        providers =
          Map.fromList
            [ (sourceModuleName source, interfaceInstanceProviders (checkedUnitInstanceInterface unit))
            | (resolvedOne, unit) <- zip resolved checked,
              source <- sourceUnitSources (resolvedUnit resolvedOne)
            ]
    package <-
      evaluate
        CheckedPackage
          { checkedName = name,
            checkedVersion = version,
            checkedExports = ownExports,
            checkedTypes = Map.restrictKeys (Map.unions (map checkedUnitTypes checked) `Map.union` dependencyTypes) exposedNames,
            checkedInstanceFacts = mergeTcInterfaces (configMergeCheck config) (dependencyInstanceFacts : map checkedUnitOwnFacts checked),
            checkedInstanceProviders = Map.restrictKeys providers exposedNames
          }
    pure
      ( package,
        PhaseTimes
          { preprocessNs = preprocessTime,
            parseNs = parseTime,
            resolveNs = resolveTime,
            typecheckNs = typecheckTime
          }
      )

packageNameAndVersion :: GenericPackageDescription -> (Text, Text)
packageNameAndVersion gpd =
  let packageId = package (packageDescription gpd)
   in ( T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId)),
        T.pack (prettyShow (CabalPackage.packageVersion packageId))
      )

-- | Parse every source of the package, each on its own task, and force
-- the whole tree of each: the parser is lazy past the module header, and
-- the time of the rest belongs to this phase, not to the resolver.
parseModules :: Int -> FilePath -> FilePath -> DependencyVersions -> [HackageCabal.FileInfo] -> IO [SourceModule]
parseModules jobs headerDirectory root versions files = do
  results <- mapM (const newEmptyTMVarIO) files
  let task order fileInfo result =
        Task
          { taskId = TaskId order,
            taskKind = TaskParse,
            taskOrder = order,
            taskDependencies = Set.empty,
            taskAction = do
              source <- parseSource headerDirectory root versions fileInfo
              parsed <- readMVar (sourceModuleParsed source)
              evaluate (rnf (parsed, sourceModuleParseDiagnostics source))
              atomically (putTMVar result source)
          }
  _ <- runTaskGraph jobs (zipWith3 task [0 ..] files results)
  mapM (atomically . readTMVar) results

-- | Resolve every unit, each once the units it imports are resolved.
resolveUnits :: Int -> Package -> ModuleExports -> [SourceUnit] -> IO [ResolvedUnit]
resolveUnits jobs resolvePackage dependencyExports units = do
  results <- unitResults units
  let task unit =
        unitTask TaskResolve unit $ do
          below <- readBelow results unit
          let availableExports = mconcat (map resolvedUnitExports below) <> dependencyExports
          packageModules <- takePackageModuleUnits resolvePackage (sourceUnitSources unit)
          let exports = collectModuleExportsWithDeps availableExports packageModules
              visibleExports = exports <> availableExports
              builtinScope = builtinFunctionScope resolvePackage visibleExports
              result = resolveUnit builtinScope visibleExports packageModules
          -- The resolver annotates lazily: the exports alone would leave
          -- the bodies to the type checker's clock.
          _ <- evaluate (force exports)
          _ <- evaluate (rnf (map moduleUnitAst (resolvedModules result)))
          _ <- evaluate (length (resolveErrors result))
          atomically (putTMVar (unitResult results unit) ResolvedUnit {resolvedUnit = unit, resolvedUnitExports = exports, resolvedUnitResult = result})
  _ <- runTaskGraph jobs (map task units)
  mapM (atomically . readTMVar . unitResult results) units

-- | Type check every unit, each once the units it imports are checked. The
-- interfaces a unit sees are assembled exactly as an install assembles
-- them: the imported modules' interfaces, the instance closure of the
-- units below, and the instances of the dependency packages whose modules
-- it imports.
typecheckUnits ::
  Int ->
  ModuleCompileConfig ->
  Package ->
  PackageId ->
  Map.Map Text TcInterface ->
  TcInterface ->
  Map.Map Text (Set.Set InstanceProvider) ->
  [ResolvedUnit] ->
  IO [CheckedUnit]
typecheckUnits jobs config resolvePackage primIdentity dependencyTypes dependencyInstanceFacts dependencyInstanceProviders resolvedUnits = do
  let units = map resolvedUnit resolvedUnits
      kinds = primKinds primIdentity
      supportTerms = typeLiteralSupportTerms primIdentity
      mergeCheck = configMergeCheck config
  results <- unitResults units
  let task resolvedOne =
        let unit = resolvedUnit resolvedOne
         in unitTask TaskTypeCheck unit $ do
              below <- readBelow results unit
              let sources = sourceUnitSources unit
                  unitNames = map sourceModuleName sources
                  importedNames = nub (concatMap sourceDependencyNames sources)
                  dependencyNames = [name | name <- nub (importedNames <> wiredInterfaceModules), name `notElem` unitNames]
                  availableTypes = Map.unions (map checkedUnitTypes below) `Map.union` dependencyTypes
                  externalProviders = Set.unions [Map.findWithDefault Set.empty name dependencyInstanceProviders | name <- dependencyNames]
                  externalInstanceInterface = selectInstanceProviders dependencyInstanceFacts externalProviders
                  importedInstanceInterface = mergeTcInterfaces TrustMergedFacts (externalInstanceInterface : map checkedUnitInstanceInterface below)
                  importedTypes =
                    mergeTcInterfaces
                      mergeCheck
                      (importedInstanceInterface : [interface | name <- dependencyNames, Just interface <- [Map.lookup name availableTypes]])
                  (checkedModules, newInterface) =
                    typecheckModuleSccWithInterface (primTcConfig primIdentity) importedTypes (resolvedModules (resolvedUnitResult resolvedOne))
                  checkedInterface = shareTcInterface newInterface
                  diagnostics = [(unitLabel unit, diagnostic) | diagnostic <- concatMap tcModuleDiagnostics checkedModules]
                  completeInterface = mergeTcInterfaces mergeCheck [importedTypes, checkedInterface]
                  ownFacts = addReferencedFacts (typeLiteralKindTyCons kinds) supportTerms completeInterface (instanceFacts checkedInterface)
                  unitTypes = map (moduleTypeInterface kinds supportTerms (resolvedUnitExports resolvedOne) resolvePackage completeInterface) sources
                  checked =
                    CheckedUnit
                      { checkedUnitTypes = Map.fromList (zip unitNames unitTypes),
                        checkedUnitOwnFacts = ownFacts,
                        checkedUnitInstanceInterface = mergeTcInterfaces TrustMergedFacts [importedInstanceInterface, ownFacts],
                        checkedUnitDiagnostics = diagnostics
                      }
              _ <- evaluate (length diagnostics)
              -- Dependencies already forced their facts. Force only the new
              -- facts, then the strict maps that select and combine them.
              _ <- evaluate (force checkedInterface)
              _ <- evaluate checked
              atomically (putTMVar (unitResult results unit) checked)
  _ <- runTaskGraph jobs (map task resolvedUnits)
  mapM (atomically . readTMVar . unitResult results) units

-- | One result slot per unit, by unit identity.
unitResults :: [SourceUnit] -> IO (Map.Map UnitId (TMVar value))
unitResults units = Map.fromList <$> mapM (\unit -> (,) (sourceUnitId unit) <$> newEmptyTMVarIO) units

unitResult :: Map.Map UnitId (TMVar value) -> SourceUnit -> TMVar value
unitResult results unit =
  fromMaybe (error "missing unit result") (Map.lookup (sourceUnitId unit) results)

-- | The results of the units a unit imports, which the task graph has
-- finished before the unit's own task starts.
readBelow :: Map.Map UnitId (TMVar value) -> SourceUnit -> IO [value]
readBelow results unit =
  mapM (\dependency -> atomically (readTMVar (fromMaybe (error "missing unit result") (Map.lookup dependency results)))) (sourceUnitDependencies unit)

-- | A task of one phase for one unit, waiting on the same phase of the
-- units it imports.
unitTask :: TaskKind -> SourceUnit -> IO () -> Task
unitTask kind unit action =
  Task
    { taskId = unitTaskId (sourceUnitId unit),
      taskKind = kind,
      taskOrder = sourceUnitOrder unit,
      taskDependencies = Set.fromList (map unitTaskId (sourceUnitDependencies unit)),
      taskAction = action
    }

unitTaskId :: UnitId -> TaskId
unitTaskId (UnitId order) = TaskId order

-- | Report the errors of a phase and exit, unless there are none.
stopOnFailure :: (FilePath -> IO DiagnosticSourceMap) -> [Value] -> [ResolveError] -> [(Text, TcDiagnostic)] -> IO ()
stopOnFailure loader parseDiagnostics resolveDiagnostics typeDiagnostics = do
  failure <- renderFrontendFailure loader parseDiagnostics resolveDiagnostics typeDiagnostics
  unless (null failure) $ do
    hFlush stdout
    hPutStrLn stderr failure
    exitFailure

timed :: IO value -> IO (value, Word64)
timed action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  pure (value, end - start)

reportPhase :: String -> Word64 -> String -> IO ()
reportPhase phase nanoseconds detail = do
  putStrLn ("  " <> padRight 11 phase <> padLeft 10 (renderDuration nanoseconds) <> (if null detail then "" else "  " <> detail))
  hFlush stdout

reportTimes :: PhaseTimes -> IO ()
reportTimes times = do
  reportPhase "preprocess" (preprocessNs times) ""
  reportPhase "parse" (parseNs times) ""
  reportPhase "resolve" (resolveNs times) ""
  reportPhase "typecheck" (typecheckNs times) ""

padRight :: Int -> String -> String
padRight width text = text <> replicate (width - length text) ' '

padLeft :: Int -> String -> String
padLeft width text = replicate (width - length text) ' ' <> text

plural :: Int -> String -> String
plural 1 noun = noun
plural _ noun = noun <> "s"

-- | A directory for what configure and the preprocessors write, gone when
-- the package is done.
withScratchDirectory :: (FilePath -> IO value) -> IO value
withScratchDirectory =
  bracket
    (getTemporaryDirectory >>= \temporary -> mkdtemp (temporary </> "aihc-dev-frontend-"))
    removeDirectoryRecursive
