module Aihc.Cli.Install
  ( ArtifactManifest (..),
    DependencyResolver (..),
    InstallFailure (..),
    InstallResult (..),
    PackageHash (..),
    PackagePlan (..),
    PackageVariantKey (..),
    PhaseManifest (..),
    PhaseStatus (..),
    ResolvedDependency (..),
    buildDryRunPackagePlanWithResolver,
    buildPackagePlanFromSource,
    buildPackagePlanWithResolver,
    defaultStoreRoot,
    dryRunInstallScaffold,
    renderInstallFailure,
    renderInstallFailureWithOptions,
    runInstall,
    writeInstallScaffold,
  )
where

import Aihc.Cli.Options (InstallErrorFormat (..), InstallOptions (..))
import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cache (sanitizeName)
import Aihc.Hackage.Cpp (cppMacrosFromOptions, injectSyntheticCppMacros, minVersionMacroNamesFromDeps)
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    LanguageEdition (..),
    Module,
    SourceSpan (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve
  ( ModuleExports,
    ResolveError (..),
    ResolveResult (..),
    Scope (..),
    extractInterface,
    resolveWithDeps,
  )
import Aihc.Tc
  ( Pred (..),
    TcBindingResult (..),
    TcDiagnostic (..),
    TcSeverity (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    Unique (..),
    renderTcType,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModule,
  )
import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Either (rights)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Distribution.PackageDescription (buildInfo, buildable, condExecutables, condLibrary, condSubLibraries, genPackageFlags, libBuildInfo)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.Flag (flagDefault, flagName, unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Numeric (showHex)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getXdgDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

data PackagePlan = PackagePlan
  { planPackageKey :: !PackageVariantKey,
    planSourcePath :: !FilePath,
    planCabalFile :: !FilePath,
    planSetupFile :: !(Maybe FilePath),
    planStoreRoot :: !FilePath,
    planStorePath :: !FilePath,
    planSourceFileCount :: !Int,
    planDependencyPlans :: ![PackagePlan]
  }
  deriving (Eq, Show)

data InstallResult = InstallResult
  { resultStorePath :: !FilePath,
    resultManifestPath :: !FilePath,
    resultInterfacePath :: !FilePath,
    resultFcPath :: !FilePath
  }
  deriving (Eq, Show)

data InstallFailure
  = InstallInterfaceFailure !PackageSpec ![(String, [Aeson.Value])]
  deriving (Eq, Show)

data ArtifactManifest = ArtifactManifest
  { manifestPackageKey :: !PackageVariantKey,
    manifestSourcePath :: !FilePath,
    manifestCabalFile :: !FilePath,
    manifestSetupFile :: !(Maybe FilePath),
    manifestStorePath :: !FilePath,
    manifestInterfacePath :: !FilePath,
    manifestFcPath :: !FilePath,
    manifestSourceFileCount :: !Int,
    manifestPhases :: ![PhaseManifest]
  }
  deriving (Eq, Show)

data PhaseManifest = PhaseManifest
  { phaseName :: !String,
    phaseStatus :: !PhaseStatus,
    phaseDescription :: !String
  }
  deriving (Eq, Show)

data PhaseStatus
  = Planned
  | Unimplemented
  | Complete
  deriving (Eq, Show)

data DependencyResolver = DependencyResolver
  { resolverResolveVersion :: String -> IO String,
    resolverSourcePath :: PackageSpec -> IO FilePath
  }

newtype PackageHash = PackageHash
  { unPackageHash :: String
  }
  deriving (Eq, Ord, Show)

data ResolvedDependency = ResolvedDependency
  { resolvedDependencySpec :: !PackageSpec,
    resolvedDependencyHash :: !PackageHash
  }
  deriving (Eq, Show)

data PackageVariantKey = PackageVariantKey
  { packageKeySpec :: !PackageSpec,
    packageKeyHash :: !PackageHash,
    packageKeyFlags :: ![(String, Bool)],
    packageKeyDependencies :: ![ResolvedDependency]
  }
  deriving (Eq, Show)

data SourceAnalysis = SourceAnalysis
  { sourceCabalFile :: !FilePath,
    sourceCabalBytes :: !BS.ByteString,
    sourceSetupFile :: !(Maybe FilePath),
    sourceSetupBytes :: !BS.ByteString,
    sourceFileCount :: !Int,
    sourceFlagAssignments :: ![(String, Bool)],
    sourceDependencyNames :: ![String]
  }

data SourceAnalysisMode
  = AllowSourceWrites
  | AvoidSourceWrites

data CoreProvider = CoreProvider
  { coreProviderName :: !String,
    coreProviderVersion :: !String,
    coreProviderSourceRel :: !FilePath
  }

data InterfaceBuildResult = InterfaceBuildResult
  { interfaceModuleExports :: !ModuleExports,
    interfaceModuleCount :: !Int,
    interfaceSourceFiles :: ![FilePath],
    interfaceParseDiagnostics :: ![Aeson.Value],
    interfaceCppDiagnostics :: ![Aeson.Value],
    interfaceResolveDiagnostics :: ![Aeson.Value],
    interfaceTcDiagnostics :: ![Aeson.Value],
    interfaceTcModules :: ![Aeson.Value]
  }

data PreparedInstall = PreparedInstall
  { preparedPlan :: !PackagePlan,
    preparedDependencies :: ![PreparedInstall],
    preparedInterface :: !InterfaceBuildResult
  }

instance ToJSON ArtifactManifest where
  toJSON manifest =
    object
      [ "schemaVersion" .= (1 :: Int),
        "package" .= packageSpecValue (packageKeySpec (manifestPackageKey manifest)),
        "packageKey" .= packageVariantKeyValue (manifestPackageKey manifest),
        "sourcePath" .= manifestSourcePath manifest,
        "cabalFile" .= manifestCabalFile manifest,
        "setupFile" .= manifestSetupFile manifest,
        "storePath" .= manifestStorePath manifest,
        "interfacePath" .= manifestInterfacePath manifest,
        "fcPath" .= manifestFcPath manifest,
        "sourceFileCount" .= manifestSourceFileCount manifest,
        "phases" .= manifestPhases manifest
      ]

instance ToJSON PhaseManifest where
  toJSON phase =
    object
      [ "name" .= phaseName phase,
        "status" .= phaseStatus phase,
        "description" .= phaseDescription phase
      ]

instance ToJSON PhaseStatus where
  toJSON status =
    Aeson.String $
      case status of
        Planned -> "planned"
        Unimplemented -> "unimplemented"
        Complete -> "complete"

runInstall :: InstallOptions -> IO ()
runInstall opts = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot opts)
  version <- resolveVersion opts
  let spec = PackageSpec (installPackageName opts) version
      resolver = hackageDependencyResolver opts
  plan <-
    if installDryRun opts
      then buildDryRunPackagePlanWithResolver resolver storeRoot spec
      else buildPackagePlanWithResolver resolver storeRoot spec
  result <-
    if installDryRun opts
      then dryRunInstallScaffold plan
      else do
        writeResult <- writeInstallScaffold plan
        case writeResult of
          Right installResult -> pure installResult
          Left failure -> do
            hPutStrLn stderr (renderInstallFailureWithOptions opts failure)
            exitFailure
  putStrLn ("store: " <> resultStorePath result)
  putStrLn ("manifest: " <> resultManifestPath result)
  putStrLn ("interfaces: " <> resultInterfacePath result)
  putStrLn ("system-fc: " <> resultFcPath result <> " (unimplemented)")
  when (installDryRun opts) $
    putStrLn "dry-run: no files written"

hackageDependencyResolver :: InstallOptions -> DependencyResolver
hackageDependencyResolver opts =
  DependencyResolver
    { resolverResolveVersion = resolveDependencyVersion opts,
      resolverSourcePath =
        HackageDownload.downloadPackageWithOptions
          HackageDownload.defaultDownloadOptions
            { HackageDownload.downloadAllowNetwork = not (installOffline opts || installDryRun opts)
            }
    }

resolveDependencyVersion :: InstallOptions -> String -> IO String
resolveDependencyVersion opts packageName
  | Just provider <- lookupCoreProvider packageName =
      pure (coreProviderVersion provider)
  | installOffline opts =
      ioError (userError ("aihc install --offline cannot resolve dependency " <> packageName <> " without cached dependency-plan metadata"))
  | otherwise = do
      result <- getLatestVersion Nothing packageName
      case result of
        Right version -> pure version
        Left err -> ioError (userError err)

resolveVersion :: InstallOptions -> IO String
resolveVersion opts =
  case installPackageVersion opts of
    Just version -> pure version
    Nothing
      | Just provider <- lookupCoreProvider (installPackageName opts) ->
          pure (coreProviderVersion provider)
      | installOffline opts ->
          ioError (userError "aihc install --offline requires --version because latest-version resolution needs Hackage metadata")
      | otherwise -> do
          result <- getLatestVersion Nothing (installPackageName opts)
          case result of
            Right version -> pure version
            Left err -> ioError (userError err)

defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  cacheDir <- getXdgDirectory XdgCache "aihc"
  pure (cacheDir </> "store")

buildPackagePlanWithResolver :: DependencyResolver -> FilePath -> PackageSpec -> IO PackagePlan
buildPackagePlanWithResolver resolver storeRoot spec =
  snd <$> buildPackagePlanRecursive AllowSourceWrites resolver storeRoot [] spec

buildDryRunPackagePlanWithResolver :: DependencyResolver -> FilePath -> PackageSpec -> IO PackagePlan
buildDryRunPackagePlanWithResolver resolver storeRoot spec =
  snd <$> buildPackagePlanRecursive AvoidSourceWrites resolver storeRoot [] spec

buildPackagePlanRecursive :: SourceAnalysisMode -> DependencyResolver -> FilePath -> [PackageSpec] -> PackageSpec -> IO (ResolvedDependency, PackagePlan)
buildPackagePlanRecursive mode resolver storeRoot stack rawSpec
  | packageSpecIdentity spec `elem` map packageSpecIdentity stack =
      ioError (userError ("Cyclic dependency while installing " <> formatPackage spec))
  | otherwise = do
      sourcePath <- sourcePathForSpec resolver spec
      analysis <- analyzeSourceWith mode sourcePath
      dependencySpecs <- mapM resolveDependencySpec (sourceDependencyNames analysis)
      dependencyPlans <- mapM (buildPackagePlanRecursive mode resolver storeRoot (spec : stack)) dependencySpecs
      let plan =
            buildPackagePlanFromAnalysis
              storeRoot
              spec
              sourcePath
              (map fst dependencyPlans)
              (map snd dependencyPlans)
              analysis
      pure (resolvedDependencyFromPlan plan, plan)
  where
    spec = canonicalPackageSpec rawSpec

    resolveDependencySpec dependencyName = do
      version <- resolveVersionForDependency dependencyName
      pure (canonicalPackageSpec (PackageSpec dependencyName version))

    resolveVersionForDependency dependencyName =
      case lookupCoreProvider dependencyName of
        Just provider -> pure (coreProviderVersion provider)
        Nothing -> resolverResolveVersion resolver dependencyName

sourcePathForSpec :: DependencyResolver -> PackageSpec -> IO FilePath
sourcePathForSpec resolver spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> coreProviderSourcePath provider
    Nothing -> resolverSourcePath resolver spec

lookupCoreProvider :: String -> Maybe CoreProvider
lookupCoreProvider name =
  case name of
    "aihc-base" -> Just aihcBaseProvider
    "ghc-prim" -> Just aihcPrimProvider
    "aihc-prim" -> Just aihcPrimProvider
    "ghc-internal" -> Just aihcInternalProvider
    "aihc-internal" -> Just aihcInternalProvider
    _ -> Nothing

canonicalPackageSpec :: PackageSpec -> PackageSpec
canonicalPackageSpec spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> PackageSpec (coreProviderName provider) (coreProviderVersion provider)
    Nothing -> spec

aihcBaseProvider :: CoreProvider
aihcBaseProvider =
  CoreProvider
    { coreProviderName = "aihc-base",
      coreProviderVersion = "4.21.2.0",
      coreProviderSourceRel = "core-libs" </> "aihc-base"
    }

aihcPrimProvider :: CoreProvider
aihcPrimProvider =
  CoreProvider
    { coreProviderName = "aihc-prim",
      coreProviderVersion = "0.13.0",
      coreProviderSourceRel = "core-libs" </> "aihc-prim"
    }

aihcInternalProvider :: CoreProvider
aihcInternalProvider =
  CoreProvider
    { coreProviderName = "aihc-internal",
      coreProviderVersion = "9.1204.0",
      coreProviderSourceRel = "core-libs" </> "aihc-internal"
    }

coreProviderSourcePath :: CoreProvider -> IO FilePath
coreProviderSourcePath provider = do
  override <- lookupEnv "AIHC_CORE_LIBS_ROOT"
  case override of
    Just root -> pure (root </> coreProviderSourceRel provider)
    Nothing -> do
      cwd <- getCurrentDirectory
      findAncestorContaining providerMarker cwd
  where
    providerRel = coreProviderSourceRel provider
    providerMarker = providerRel </> coreProviderName provider <> ".cabal"

    findAncestorContaining marker dir = do
      exists <- doesFileExist (dir </> marker)
      if exists
        then pure (dir </> providerRel)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then ioError (userError ("Could not find local core library " <> providerRel <> " from current directory"))
            else findAncestorContaining marker parent

packageSpecIdentity :: PackageSpec -> (String, String)
packageSpecIdentity spec =
  (pkgName spec, pkgVersion spec)

resolvedDependencyFromPlan :: PackagePlan -> ResolvedDependency
resolvedDependencyFromPlan plan =
  ResolvedDependency
    { resolvedDependencySpec = packageKeySpec key,
      resolvedDependencyHash = packageKeyHash key
    }
  where
    key = planPackageKey plan

buildPackagePlanFromSource :: FilePath -> PackageSpec -> FilePath -> IO PackagePlan
buildPackagePlanFromSource storeRoot spec sourcePath = do
  analysis <- analyzeSourceWith AllowSourceWrites sourcePath
  pure (buildPackagePlanFromAnalysis storeRoot spec sourcePath [] [] analysis)

buildPackagePlanFromAnalysis :: FilePath -> PackageSpec -> FilePath -> [ResolvedDependency] -> [PackagePlan] -> SourceAnalysis -> PackagePlan
buildPackagePlanFromAnalysis storeRoot spec sourcePath dependencies dependencyPlans analysis =
  let sortedDependencies = sortDependencies dependencies
      sortedDependencyPlans = sortPackagePlans dependencyPlans
      packageHash =
        computePackageHash
          spec
          (sourceFlagAssignments analysis)
          sortedDependencies
          (sourceCabalBytes analysis)
          (sourceSetupBytes analysis)
      storePath = storeRoot </> (unPackageHash packageHash <> "-" <> sanitizeName (formatPackage spec))
   in PackagePlan
        { planPackageKey =
            PackageVariantKey
              { packageKeySpec = spec,
                packageKeyHash = packageHash,
                packageKeyFlags = sourceFlagAssignments analysis,
                packageKeyDependencies = sortedDependencies
              },
          planSourcePath = sourcePath,
          planCabalFile = sourceCabalFile analysis,
          planSetupFile = sourceSetupFile analysis,
          planStoreRoot = storeRoot,
          planStorePath = storePath,
          planSourceFileCount = sourceFileCount analysis,
          planDependencyPlans = sortedDependencyPlans
        }

analyzeSourceWith :: SourceAnalysisMode -> FilePath -> IO SourceAnalysis
analyzeSourceWith mode sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right parsed) -> pure parsed
      (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))
  sourceFileCount <- analyzeSourceFileCount mode gpd sourcePath
  setupFile <- findSetupFile sourcePath
  setupBytes <- maybe (pure BS.empty) BS.readFile setupFile
  pure
    SourceAnalysis
      { sourceCabalFile = cabalFile,
        sourceCabalBytes = cabalBytes,
        sourceSetupFile = setupFile,
        sourceSetupBytes = setupBytes,
        sourceFileCount = sourceFileCount,
        sourceFlagAssignments = packageFlagAssignments gpd,
        sourceDependencyNames = packageDependencyNames gpd
      }

analyzeSourceFileCount :: SourceAnalysisMode -> GenericPackageDescription -> FilePath -> IO Int
analyzeSourceFileCount mode gpd sourcePath =
  case mode of
    AllowSourceWrites -> length <$> HackageCabal.collectComponentFiles gpd sourcePath
    AvoidSourceWrites -> pure 0

writeInstallScaffold :: PackagePlan -> IO (Either InstallFailure InstallResult)
writeInstallScaffold plan = do
  prepareResult <- prepareInstallScaffold plan
  case prepareResult of
    Left failure -> pure (Left failure)
    Right prepared -> do
      writePreparedInstallScaffold prepared
      pure (Right (installResultForPlan plan))

prepareInstallScaffold :: PackagePlan -> IO (Either InstallFailure PreparedInstall)
prepareInstallScaffold plan =
  fmap snd <$> prepareInstallScaffoldRecursive plan

prepareInstallScaffoldRecursive :: PackagePlan -> IO (Either InstallFailure (ModuleExports, PreparedInstall))
prepareInstallScaffoldRecursive plan = do
  dependencyResults <- traverse prepareInstallScaffoldRecursive (planDependencyPlans plan)
  case firstLeft dependencyResults of
    Just failure -> pure (Left failure)
    Nothing -> do
      let preparedDependencyPairs = rights dependencyResults
          depExports = foldl' Map.union Map.empty (map fst preparedDependencyPairs)
          preparedDependencies = map snd preparedDependencyPairs
      interfaceResult <- generatePackageInterface depExports plan
      pure $
        case blockingInterfaceFailures interfaceResult of
          [] ->
            Right
              ( interfaceModuleExports interfaceResult,
                PreparedInstall
                  { preparedPlan = plan,
                    preparedDependencies = preparedDependencies,
                    preparedInterface = interfaceResult
                  }
              )
          failures ->
            Left (InstallInterfaceFailure (packageKeySpec (planPackageKey plan)) failures)

firstLeft :: [Either a b] -> Maybe a
firstLeft [] = Nothing
firstLeft (Left value : _) = Just value
firstLeft (Right _ : rest) = firstLeft rest

writePreparedInstallScaffold :: PreparedInstall -> IO ()
writePreparedInstallScaffold prepared = do
  mapM_ writePreparedInstallScaffold (preparedDependencies prepared)
  writeOnePreparedInstallScaffold (preparedPlan prepared) (preparedInterface prepared)

writeOnePreparedInstallScaffold :: PackagePlan -> InterfaceBuildResult -> IO ()
writeOnePreparedInstallScaffold plan interfaceResult = do
  let result = installResultForPlan plan
      manifestPath = resultManifestPath result
      interfacePath = resultInterfacePath result
      fcPath = resultFcPath result
      manifest = artifactManifestForPlan result plan
  createDirectoryIfMissing True (takeDirectory manifestPath)
  createDirectoryIfMissing True (takeDirectory interfacePath)
  createDirectoryIfMissing True (takeDirectory fcPath)
  BL.writeFile manifestPath (Aeson.encode manifest)
  BL.writeFile interfacePath (Aeson.encode (interfaceArtifactValue plan interfaceResult))
  BL.writeFile fcPath (Aeson.encode (fcPlaceholder plan))

blockingInterfaceFailures :: InterfaceBuildResult -> [(String, [Aeson.Value])]
blockingInterfaceFailures result =
  [ ("rename", interfaceResolveDiagnostics result)
  | not (null (interfaceResolveDiagnostics result))
  ]
    <> [ ("type-check", interfaceTcDiagnostics result)
       | not (null (interfaceTcDiagnostics result))
       ]

renderInstallFailure :: InstallFailure -> String
renderInstallFailure =
  renderInstallFailureWith False InstallErrorsJson

renderInstallFailureWithOptions :: InstallOptions -> InstallFailure -> String
renderInstallFailureWithOptions opts =
  renderInstallFailureWith (installFirstErrorModule opts) (installErrorFormat opts)

renderInstallFailureWith :: Bool -> InstallErrorFormat -> InstallFailure -> String
renderInstallFailureWith limitToFirstModule errorFormat failure =
  case failure of
    InstallInterfaceFailure spec failures ->
      renderInterfaceBuildFailure limitToFirstModule errorFormat spec failures

renderInterfaceBuildFailure :: Bool -> InstallErrorFormat -> PackageSpec -> [(String, [Aeson.Value])] -> String
renderInterfaceBuildFailure limitToFirstModule errorFormat spec failures =
  unlines $
    [ "failed to install " <> formatPackage spec <> ": interface compilation failed"
    ]
      <> renderedFailures
  where
    diagnosticScopeLimit =
      if limitToFirstModule
        then firstDiagnosticScope failures
        else Nothing
    filteredFailures =
      [ (phase, filter (diagnosticMatchesScopeLimit diagnosticScopeLimit) diagnostics)
      | (phase, diagnostics) <- failures
      ]
    nonEmptyFailures = filter (not . null . snd) filteredFailures
    renderedFailures = concatMap renderFailure nonEmptyFailures
    renderFailure (phase, diagnostics) =
      ("  " <> phase <> " errors:") : concatMap (indentLines . renderDiagnostic errorFormat phase) diagnostics

indentLines :: String -> [String]
indentLines =
  map ("    " <>) . lines

renderDiagnostic :: InstallErrorFormat -> String -> Aeson.Value -> String
renderDiagnostic errorFormat phase diagnostic =
  case errorFormat of
    InstallErrorsJson -> renderDiagnosticValue diagnostic
    InstallErrorsHuman -> renderHumanDiagnostic phase diagnostic

renderDiagnosticValue :: Aeson.Value -> String
renderDiagnosticValue =
  T.unpack . TE.decodeUtf8 . BL.toStrict . Aeson.encode

renderHumanDiagnostic :: String -> Aeson.Value -> String
renderHumanDiagnostic phase diagnostic =
  unlines $
    T.unpack (locationPrefix <> severityText <> ": " <> modulePrefix <> messageText)
      : renderHumanDiagnosticExcerpt diagnostic
  where
    locationPrefix =
      case diagnosticLocation diagnostic of
        Just location -> location <> ": "
        Nothing -> ""
    modulePrefix =
      case diagnosticModule diagnostic of
        Just moduleName -> "[" <> moduleName <> "] "
        Nothing -> ""
    severityText = fromMaybe (T.pack phase) (stringField "severity" diagnostic)
    messageText = fromMaybe (diagnosticSummary diagnostic) (stringField "message" diagnostic)

renderHumanDiagnosticExcerpt :: Aeson.Value -> [String]
renderHumanDiagnosticExcerpt diagnostic =
  case (diagnosticSourceLines diagnostic, diagnosticSpanLines diagnostic) of
    (sourceLines@(_ : _), Just (startLine, startColumn, endLine, endColumn)) ->
      renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
    _ -> []

renderSourceExcerpt :: [DiagnosticSourceLine] -> Int -> Int -> Int -> Int -> [String]
renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
  | null selectedLines = []
  | otherwise = concatMap renderLine selectedLines
  where
    selectedLines =
      filter
        ( \sourceLine ->
            sourceLineNumber sourceLine >= startLine
              && sourceLineNumber sourceLine <= endLine
        )
        sourceLines
    width = length (show (maximum (map sourceLineNumber selectedLines)))
    renderLine sourceLine =
      [ "  " <> padLeft width ' ' (show lineNumber) <> " | " <> T.unpack lineText,
        "  " <> replicate width ' ' <> " | " <> T.unpack (caretIndicator lineNumber lineText)
      ]
      where
        lineNumber = sourceLineNumber sourceLine
        lineText = sourceLineText sourceLine
    caretIndicator lineNumber lineText =
      T.replicate (max 0 (lineStartColumn lineNumber - 1)) " "
        <> T.replicate (lineCaretWidth lineNumber lineText) "^"
    lineStartColumn lineNumber
      | lineNumber == startLine = max 1 startColumn
      | otherwise = 1
    lineCaretWidth lineNumber lineText
      | startLine == endLine =
          max 1 (endColumn - startColumn)
      | lineNumber == startLine =
          max 1 (T.length lineText - lineStartColumn lineNumber + 1)
      | lineNumber == endLine =
          max 1 (endColumn - 1)
      | otherwise =
          max 1 (T.length lineText)

data DiagnosticScope = DiagnosticScope
  { diagnosticScopeModule :: !(Maybe Text),
    diagnosticScopeFile :: !(Maybe Text)
  }

firstDiagnosticScope :: [(String, [Aeson.Value])] -> Maybe DiagnosticScope
firstDiagnosticScope =
  listToMaybe . concatMap (mapMaybe diagnosticScope . snd)

diagnosticScope :: Aeson.Value -> Maybe DiagnosticScope
diagnosticScope diagnostic =
  case (diagnosticModule diagnostic, diagnosticFile diagnostic) of
    (Nothing, Nothing) -> Nothing
    (moduleName, file) ->
      Just
        DiagnosticScope
          { diagnosticScopeModule = moduleName,
            diagnosticScopeFile = normalizeDiagnosticFile <$> file
          }

diagnosticMatchesScopeLimit :: Maybe DiagnosticScope -> Aeson.Value -> Bool
diagnosticMatchesScopeLimit Nothing _ = True
diagnosticMatchesScopeLimit (Just scope) diagnostic =
  moduleMatches || fileMatches
  where
    moduleMatches =
      case diagnosticScopeModule scope of
        Just moduleName ->
          diagnosticModule diagnostic == Just moduleName
            || maybe False (fileMatchesModule moduleName) (diagnosticFile diagnostic)
        Nothing -> False
    fileMatches =
      case diagnosticScopeFile scope of
        Just file -> (normalizeDiagnosticFile <$> diagnosticFile diagnostic) == Just file
        Nothing -> False

diagnosticModule :: Aeson.Value -> Maybe Text
diagnosticModule =
  stringField "module"

diagnosticFile :: Aeson.Value -> Maybe Text
diagnosticFile diagnostic =
  stringField "file" diagnostic
    <|> (objectField "span" diagnostic >>= stringField "file")

diagnosticLocation :: Aeson.Value -> Maybe Text
diagnosticLocation diagnostic =
  case diagnosticFile diagnostic of
    Nothing -> Nothing
    Just file ->
      Just $
        file
          <> maybe "" (":" <>) lineText
          <> maybe "" (":" <>) columnText
  where
    spanValue = objectField "span" diagnostic
    lineText = scalarFieldText "line" diagnostic <|> (spanValue >>= scalarFieldText "startLine")
    columnText = spanValue >>= scalarFieldText "startColumn"

diagnosticSpanLines :: Aeson.Value -> Maybe (Int, Int, Int, Int)
diagnosticSpanLines diagnostic = do
  spanValue <- objectField "span" diagnostic
  startLine <- intField "startLine" spanValue
  startColumn <- intField "startColumn" spanValue
  endLine <- intField "endLine" spanValue
  endColumn <- intField "endColumn" spanValue
  pure (startLine, startColumn, endLine, endColumn)

data DiagnosticSourceLine = DiagnosticSourceLine
  { sourceLineNumber :: !Int,
    sourceLineText :: !Text
  }

instance Aeson.FromJSON DiagnosticSourceLine where
  parseJSON =
    Aeson.withObject "DiagnosticSourceLine" $ \obj ->
      DiagnosticSourceLine
        <$> obj .: "line"
        <*> obj .: "text"

diagnosticSourceLines :: Aeson.Value -> [DiagnosticSourceLine]
diagnosticSourceLines diagnostic =
  case objectField "sourceLines" diagnostic of
    Just value ->
      case Aeson.fromJSON value of
        Aeson.Success sourceLines -> sourceLines
        Aeson.Error {} -> []
    Nothing -> []

fileMatchesModule :: Text -> Text -> Bool
fileMatchesModule moduleName file =
  any (`T.isSuffixOf` normalizeDiagnosticFile file) suffixes
  where
    modulePath = T.map dotToSlash moduleName
    suffixes = [modulePath <> ".hs", modulePath <> ".lhs"]
    dotToSlash '.' = '/'
    dotToSlash c = c

normalizeDiagnosticFile :: Text -> Text
normalizeDiagnosticFile =
  T.map slash
  where
    slash '\\' = '/'
    slash c = c

stringField :: String -> Aeson.Value -> Maybe Text
stringField name value =
  case objectField name value of
    Just (Aeson.String text) -> Just text
    _ -> Nothing

scalarFieldText :: String -> Aeson.Value -> Maybe Text
scalarFieldText name value =
  scalarValueText =<< objectField name value

intField :: String -> Aeson.Value -> Maybe Int
intField name value =
  case objectField name value of
    Just fieldValue ->
      case Aeson.fromJSON fieldValue of
        Aeson.Success int -> Just int
        Aeson.Error {} -> Nothing
    Nothing -> Nothing

scalarValueText :: Aeson.Value -> Maybe Text
scalarValueText value =
  case value of
    Aeson.String text -> Just text
    Aeson.Number {} ->
      let parsedInt :: Aeson.Result Int
          parsedInt = Aeson.fromJSON value
       in case parsedInt of
            Aeson.Success int -> Just (T.pack (show int))
            Aeson.Error {} -> Just (diagnosticSummary value)
    _ -> Nothing

objectField :: String -> Aeson.Value -> Maybe Aeson.Value
objectField name value =
  case value of
    Aeson.Object obj -> KeyMap.lookup (Key.fromString name) obj
    _ -> Nothing

diagnosticSummary :: Aeson.Value -> Text
diagnosticSummary =
  TE.decodeUtf8 . BL.toStrict . Aeson.encode

addTcModuleDiagnosticSourceLines :: Map.Map FilePath [Text] -> Aeson.Value -> Aeson.Value
addTcModuleDiagnosticSourceLines sourceLinesByFile value =
  case value of
    Aeson.Object obj ->
      case KeyMap.lookup "diagnostics" obj of
        Just (Aeson.Array diagnostics) ->
          Aeson.Object $
            KeyMap.insert
              "diagnostics"
              (Aeson.toJSON (map (addDiagnosticSourceLines sourceLinesByFile) (toList diagnostics)))
              obj
        _ -> value
    _ -> value

addDiagnosticSourceLines :: Map.Map FilePath [Text] -> Aeson.Value -> Aeson.Value
addDiagnosticSourceLines sourceLinesByFile diagnostic =
  case (diagnostic, diagnosticFile diagnostic, diagnosticLineRange diagnostic) of
    (Aeson.Object obj, Just file, Just (startLine, endLine)) ->
      case lookupSourceLines file sourceLinesByFile of
        Just sourceLines ->
          Aeson.Object $
            KeyMap.insert
              "sourceLines"
              (Aeson.toJSON (sourceLineExcerpt sourceLines startLine endLine))
              obj
        Nothing -> diagnostic
    _ -> diagnostic

lookupSourceLines :: Text -> Map.Map FilePath [Text] -> Maybe [Text]
lookupSourceLines file sourceLinesByFile =
  Map.lookup (T.unpack file) sourceLinesByFile
    <|> lookupNormalized
  where
    normalizedFile = normalizeDiagnosticFile file
    lookupNormalized =
      snd
        <$> listToMaybe
          [ (path, sourceLines)
          | (path, sourceLines) <- Map.toList sourceLinesByFile,
            normalizeDiagnosticFile (T.pack path) == normalizedFile
          ]

diagnosticLineRange :: Aeson.Value -> Maybe (Int, Int)
diagnosticLineRange diagnostic =
  spanLineRange <|> lineOnlyRange
  where
    spanLineRange = do
      spanValue <- objectField "span" diagnostic
      startLine <- intField "startLine" spanValue
      endLine <- intField "endLine" spanValue
      pure (startLine, endLine)
    lineOnlyRange = do
      line <- intField "line" diagnostic
      pure (line, line)

sourceLineExcerpt :: [Text] -> Int -> Int -> [Aeson.Value]
sourceLineExcerpt sourceLines startLine endLine =
  [ object
      [ "line" .= lineNumber,
        "text" .= lineText
      ]
  | (lineNumber, lineText) <- zip [1 :: Int ..] sourceLines,
    lineNumber >= startLine,
    lineNumber <= endLine
  ]

dryRunInstallScaffold :: PackagePlan -> IO InstallResult
dryRunInstallScaffold =
  pure . installResultForPlan

installResultForPlan :: PackagePlan -> InstallResult
installResultForPlan plan =
  InstallResult
    { resultStorePath = storePath,
      resultManifestPath = manifestPath,
      resultInterfacePath = interfacePath,
      resultFcPath = fcPath
    }
  where
    storePath = planStorePath plan
    manifestPath = storePath </> "manifest.json"
    interfacePath = storePath </> "interfaces" </> "package-interface.json"
    fcPath = storePath </> "fc" </> "package-fc.json"

artifactManifestForPlan :: InstallResult -> PackagePlan -> ArtifactManifest
artifactManifestForPlan result plan =
  ArtifactManifest
    { manifestPackageKey = planPackageKey plan,
      manifestSourcePath = planSourcePath plan,
      manifestCabalFile = planCabalFile plan,
      manifestSetupFile = planSetupFile plan,
      manifestStorePath = resultStorePath result,
      manifestInterfacePath = resultInterfacePath result,
      manifestFcPath = resultFcPath result,
      manifestSourceFileCount = planSourceFileCount plan,
      manifestPhases = plannedPhases
    }

plannedPhases :: [PhaseManifest]
plannedPhases =
  [ PhaseManifest "resolve-dependency-closure" Complete "Resolve dependency versions recursively and key each package variant by direct dependency hashes",
    PhaseManifest "compile-setup" Unimplemented "Compile Setup.hs or Setup.lhs with ghc in an isolated work directory",
    PhaseManifest "configure-package" Unimplemented "Use the Cabal library to configure package components without invoking cabal-install",
    PhaseManifest "run-external-processors" Planned "Reserve processors such as happy, alex, and c2hs for reproducible generated sources",
    PhaseManifest "compile-interfaces" Complete "Generate name-resolution, type, and fixity interface data",
    PhaseManifest "desugar-system-fc" Unimplemented "Generate desugared System-FC data files"
  ]

fcPlaceholder :: PackagePlan -> Aeson.Value
fcPlaceholder plan =
  object
    [ "schemaVersion" .= (1 :: Int),
      "packageKey" .= packageVariantKeyValue (planPackageKey plan),
      "status" .= ("unimplemented" :: String),
      "contains" .= (["system-fc"] :: [String])
    ]

generatePackageInterface :: ModuleExports -> PackagePlan -> IO InterfaceBuildResult
generatePackageInterface depExports plan = do
  files <- collectPlanFiles plan
  parsedFiles <- mapM (parseInterfaceFile (planSourcePath plan)) files
  let parsedModules = [modu | ParsedFileOk _ modu _ _ <- parsedFiles]
      sourceLinesByFile = Map.fromList (map parsedFileSourceLines parsedFiles)
      enrichDiagnostics = map (addDiagnosticSourceLines sourceLinesByFile)
      parseDiagnostics = enrichDiagnostics (concatMap parsedFileParseDiagnostics parsedFiles)
      cppDiagnostics = enrichDiagnostics (concatMap parsedFileCppDiagnostics parsedFiles)
      resolveResult = resolveWithDeps depExports parsedModules
      ownExports = extractInterface resolveResult
  (tcModules, tcDiagnostics) <- typecheckInterfaceModules (resolvedModules resolveResult)
  let resolveDiagnostics = enrichDiagnostics (map resolveErrorValue (resolveErrors resolveResult))
      enrichedTcDiagnostics = enrichDiagnostics tcDiagnostics
      enrichedTcModules = map (addTcModuleDiagnosticSourceLines sourceLinesByFile) tcModules
  pure
    InterfaceBuildResult
      { interfaceModuleExports = ownExports,
        interfaceModuleCount = length parsedModules,
        interfaceSourceFiles = map HackageCabal.fileInfoPath files,
        interfaceParseDiagnostics = parseDiagnostics,
        interfaceCppDiagnostics = cppDiagnostics,
        interfaceResolveDiagnostics = resolveDiagnostics,
        interfaceTcDiagnostics = enrichedTcDiagnostics,
        interfaceTcModules = enrichedTcModules
      }

typecheckInterfaceModules :: [Module] -> IO ([Aeson.Value], [Aeson.Value])
typecheckInterfaceModules modules = do
  currentModule <- newIORef Nothing
  completedModules <- newIORef []
  result <- timeout typecheckPhaseTimeoutMicros (go currentModule completedModules [] modules)
  case result of
    Just tcModules -> pure (tcModules, concatMap tcModuleDiagnosticValues tcModules)
    Nothing -> do
      acc <- readIORef completedModules
      current <- readIORef currentModule
      let tcModules = reverse acc
      pure (tcModules, concatMap tcModuleDiagnosticValues tcModules <> [typecheckTimeoutDiagnostic current])
  where
    go _current _completed acc [] =
      pure (reverse acc)
    go current completed acc (modu : rest) = do
      writeIORef current (Just modu)
      tcModule <- evaluate (forceJsonValue (typecheckInterfaceModule modu))
      let acc' = tcModule : acc
      writeIORef completed acc'
      go current completed acc' rest

typecheckInterfaceModule :: Module -> Aeson.Value
typecheckInterfaceModule modu =
  tcModuleValue modu (typecheckModule modu)

typecheckPhaseTimeoutMicros :: Int
typecheckPhaseTimeoutMicros = 1000 * 1000

forceJsonValue :: Aeson.Value -> Aeson.Value
forceJsonValue value =
  length (show value) `seq` value

typecheckTimeoutDiagnostic :: Maybe Module -> Aeson.Value
typecheckTimeoutDiagnostic modu =
  let name = fromMaybe "<unknown>" (Syntax.moduleName =<< modu)
   in object
        [ "span" .= sourceSpanValue NoSourceSpan,
          "severity" .= ("error" :: String),
          "module" .= name,
          "message" .= ("typecheck timed out after 1s while checking " <> T.unpack name <> "; this is a bug" :: String)
        ]

data ParsedInterfaceFile
  = ParsedFileOk !FilePath !Module ![Text] ![Aeson.Value]
  | ParsedFileFailed !FilePath ![Text] ![Aeson.Value] ![Aeson.Value]

parsedFileSourceLines :: ParsedInterfaceFile -> (FilePath, [Text])
parsedFileSourceLines parsed =
  case parsed of
    ParsedFileOk path _ sourceLines _ -> (path, sourceLines)
    ParsedFileFailed path sourceLines _ _ -> (path, sourceLines)

parsedFileParseDiagnostics :: ParsedInterfaceFile -> [Aeson.Value]
parsedFileParseDiagnostics parsed =
  case parsed of
    ParsedFileOk {} -> []
    ParsedFileFailed _ _ parseDiagnostics _ -> parseDiagnostics

parsedFileCppDiagnostics :: ParsedInterfaceFile -> [Aeson.Value]
parsedFileCppDiagnostics parsed =
  case parsed of
    ParsedFileOk _ _ _ cppDiagnostics -> cppDiagnostics
    ParsedFileFailed _ _ _ cppDiagnostics -> cppDiagnostics

collectPlanFiles :: PackagePlan -> IO [HackageCabal.FileInfo]
collectPlanFiles plan = do
  cabalBytes <- BS.readFile (planCabalFile plan)
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right parsed) -> pure parsed
      (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> planCabalFile plan <> ": " <> show errs))
  HackageCabal.collectComponentFiles gpd (planSourcePath plan)

parseInterfaceFile :: FilePath -> HackageCabal.FileInfo -> IO ParsedInterfaceFile
parseInterfaceFile packageRoot fileInfo = do
  rawSource <- HackageUtil.readTextFileLenient path
  let normalized = normalizeSource path rawSource
      cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HackageCabal.fileInfoExtensions fileInfo)
      cppEnabledGlobally = any isCppExtension cabalExtSettings
      cppEnabledInFile = any isCppExtension (headerExtensionSettings (readModuleHeaderPragmas normalized))
  (source, cppDiagnostics) <-
    if cppEnabledGlobally || cppEnabledInFile
      then preprocessInterfaceSource packageRoot fileInfo normalized
      else pure (normalized, [])
  let headerPragmas = readModuleHeaderPragmas source
      allExtSettings = cabalExtSettings <> headerExtensionSettings headerPragmas
      language =
        headerLanguageEdition headerPragmas
          `orElse` (HackageCabal.fileInfoLanguage fileInfo >>= parseLanguageEdition . T.pack)
      extensions = effectiveExtensions (fromMaybe Haskell98Edition language) allExtSettings
      cfg = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (parseErrs, modu) = parseModule cfg source
      parseDiagnostics = map (parseDiagnosticValue path) parseErrs
      sourceLines = T.lines source
  pure $
    if null parseErrs
      then ParsedFileOk path modu sourceLines cppDiagnostics
      else ParsedFileFailed path sourceLines parseDiagnostics cppDiagnostics
  where
    path = HackageCabal.fileInfoPath fileInfo

    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

    isCppExtension setting =
      case setting of
        EnableExtension CPP -> True
        _ -> False

preprocessInterfaceSource :: FilePath -> HackageCabal.FileInfo -> Text -> IO (Text, [Aeson.Value])
preprocessInterfaceSource packageRoot fileInfo source = do
  drive (Cpp.preprocess cppConfig (TE.encodeUtf8 injectedSource))
  where
    path = HackageCabal.fileInfoPath fileInfo
    cppOptions = HackageCabal.fileInfoCppOptions fileInfo
    minVersionMacros = minVersionMacroNamesFromDeps (HackageCabal.fileInfoDependencies fileInfo)
    injectedSource = injectSyntheticCppMacros cppOptions minVersionMacros source
    cppConfig =
      Cpp.defaultConfig
        { Cpp.configInputFile = path,
          Cpp.configMacros = cppMacrosFromOptions cppOptions
        }

    drive step =
      case step of
        Cpp.Done result ->
          pure (Cpp.resultOutput result, map cppDiagnosticValue (Cpp.resultDiagnostics result))
        Cpp.NeedInclude req k -> do
          content <- resolveInclude packageRoot (HackageCabal.fileInfoIncludeDirs fileInfo) path req
          drive (k content)

resolveInclude :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> IO (Maybe BS.ByteString)
resolveInclude packageRoot includeDirs currentFile req =
  findFirst (includeCandidates packageRoot includeDirs currentFile req)
  where
    findFirst [] = pure Nothing
    findFirst (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then Just <$> BS.readFile candidate
        else findFirst rest

includeCandidates :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> [FilePath]
includeCandidates packageRoot includeDirs currentFile req =
  map normalise $
    nub
      [ dir </> Cpp.includePath req
      | dir <- searchDirs
      ]
  where
    includeDir = takeDirectory (Cpp.includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      includeDirs
        <> [ packageRoot </> "include",
             packageRoot </> "includes",
             packageRoot </> "cbits",
             packageRoot
           ]
    searchDirs =
      case Cpp.includeKind req of
        Cpp.IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        Cpp.IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

normalizeSource :: FilePath -> Text -> Text
normalizeSource path source =
  let withoutBom = T.dropWhile (== '\xfeff') source
   in if takeExtension path == ".lhs"
        then T.unlines (unlitBird (T.lines withoutBom))
        else withoutBom

unlitBird :: [Text] -> [Text]
unlitBird =
  map $ \line ->
    case T.uncons line of
      Just ('>', rest) -> rest
      _ -> ""

interfaceArtifactValue :: PackagePlan -> InterfaceBuildResult -> Aeson.Value
interfaceArtifactValue plan result =
  object
    [ "schemaVersion" .= (1 :: Int),
      "packageKey" .= packageVariantKeyValue (planPackageKey plan),
      "status" .= interfaceStatus result,
      "contains" .= (["name-resolution", "types", "fixities"] :: [String]),
      "sourceFiles" .= interfaceSourceFiles result,
      "moduleCount" .= interfaceModuleCount result,
      "modules" .= moduleExportsValue (interfaceModuleExports result),
      "diagnostics"
        .= object
          [ "cpp" .= interfaceCppDiagnostics result,
            "parse" .= interfaceParseDiagnostics result,
            "resolve" .= interfaceResolveDiagnostics result,
            "typecheck" .= interfaceTcDiagnostics result
          ],
      "typecheck" .= interfaceTcModules result
    ]

interfaceStatus :: InterfaceBuildResult -> String
interfaceStatus result =
  if null (interfaceParseDiagnostics result)
    && null (interfaceResolveDiagnostics result)
    && null (interfaceTcDiagnostics result)
    then "complete"
    else "partial"

moduleExportsValue :: ModuleExports -> [Aeson.Value]
moduleExportsValue exports =
  [ object
      [ "module" .= moduleNameText,
        "terms" .= Map.keys (scopeTerms scope),
        "types" .= Map.keys (scopeTypes scope),
        "constructors" .= scopeConstructors scope,
        "recordFields" .= scopeRecordFields scope,
        "methods" .= scopeMethods scope
      ]
  | (moduleNameText, scope) <- Map.toAscList exports
  ]

tcModuleValue :: Module -> Module -> Aeson.Value
tcModuleValue modu result =
  object
    [ "module" .= moduleDisplayName modu,
      "success" .= tcModuleSuccess result,
      "bindings" .= map tcBindingValue (tcModuleBindings result),
      "diagnostics" .= map (tcDiagnosticValue (moduleDisplayName modu)) (tcModuleDiagnostics result)
    ]

tcModuleDiagnosticValues :: Aeson.Value -> [Aeson.Value]
tcModuleDiagnosticValues (Aeson.Object obj) =
  case KeyMap.lookup "diagnostics" obj of
    Just (Aeson.Array arr) -> foldr (:) [] arr
    _ -> []
tcModuleDiagnosticValues _ = []

tcBindingValue :: TcBindingResult -> Aeson.Value
tcBindingValue binding =
  object
    [ "name" .= tbName binding,
      "type" .= renderTcType (tbType binding),
      "typeJson" .= tcTypeValue (tbType binding)
    ]

tcTypeValue :: TcType -> Aeson.Value
tcTypeValue ty =
  case ty of
    TcTyVar tv ->
      object
        [ "tag" .= ("var" :: String),
          "name" .= tvName tv,
          "unique" .= uniqueValue (tvUnique tv)
        ]
    TcMetaTv unique ->
      object
        [ "tag" .= ("meta" :: String),
          "unique" .= uniqueValue unique
        ]
    TcTyCon tyCon args ->
      object
        [ "tag" .= ("con" :: String),
          "name" .= tyConName tyCon,
          "arity" .= tyConArity tyCon,
          "args" .= map tcTypeValue args
        ]
    TcFunTy arg result ->
      object
        [ "tag" .= ("fun" :: String),
          "arg" .= tcTypeValue arg,
          "result" .= tcTypeValue result
        ]
    TcForAllTy tv body ->
      object
        [ "tag" .= ("forall" :: String),
          "binder" .= tyVarValue tv,
          "body" .= tcTypeValue body
        ]
    TcQualTy preds body ->
      object
        [ "tag" .= ("qual" :: String),
          "predicates" .= map predValue preds,
          "body" .= tcTypeValue body
        ]
    TcAppTy fun arg ->
      object
        [ "tag" .= ("app" :: String),
          "fun" .= tcTypeValue fun,
          "arg" .= tcTypeValue arg
        ]

predValue :: Pred -> Aeson.Value
predValue pred' =
  case pred' of
    ClassPred cls args ->
      object
        [ "tag" .= ("class" :: String),
          "class" .= cls,
          "args" .= map tcTypeValue args
        ]
    EqPred left right ->
      object
        [ "tag" .= ("eq" :: String),
          "left" .= tcTypeValue left,
          "right" .= tcTypeValue right
        ]

tyVarValue :: TyVarId -> Aeson.Value
tyVarValue tv =
  object
    [ "name" .= tvName tv,
      "unique" .= uniqueValue (tvUnique tv)
    ]

uniqueValue :: Unique -> Int
uniqueValue (Unique unique) = unique

tcDiagnosticValue :: Text -> TcDiagnostic -> Aeson.Value
tcDiagnosticValue moduleName diag =
  object
    [ "span" .= sourceSpanValue (fromMaybe NoSourceSpan (diagLoc diag)),
      "severity" .= tcSeverityText (diagSeverity diag),
      "module" .= moduleName,
      "message" .= show (diagKind diag)
    ]

tcSeverityText :: TcSeverity -> String
tcSeverityText severity =
  case severity of
    TcError -> "error"
    TcWarning -> "warning"

resolveErrorValue :: ResolveError -> Aeson.Value
resolveErrorValue err =
  case err of
    ResolveResolutionError span' name namespace message ->
      object
        [ "span" .= sourceSpanValue span',
          "name" .= name,
          "namespace" .= show namespace,
          "message" .= message
        ]
    ResolveNotImplemented message ->
      object
        [ "span" .= sourceSpanValue NoSourceSpan,
          "message" .= message
        ]

parseDiagnosticValue :: FilePath -> (SourceSpan, Text) -> Aeson.Value
parseDiagnosticValue path (span', message) =
  object
    [ "file" .= path,
      "span" .= sourceSpanValue span',
      "message" .= message
    ]

cppDiagnosticValue :: Cpp.Diagnostic -> Aeson.Value
cppDiagnosticValue diag =
  object
    [ "file" .= Cpp.diagFile diag,
      "line" .= Cpp.diagLine diag,
      "severity" .= show (Cpp.diagSeverity diag),
      "message" .= Cpp.diagMessage diag
    ]

sourceSpanValue :: SourceSpan -> Aeson.Value
sourceSpanValue span' =
  case span' of
    NoSourceSpan -> Aeson.Null
    SourceSpan file startLine startCol endLine endCol startOffset endOffset ->
      object
        [ "file" .= file,
          "startLine" .= startLine,
          "startColumn" .= startCol,
          "endLine" .= endLine,
          "endColumn" .= endCol,
          "startOffset" .= startOffset,
          "endOffset" .= endOffset
        ]

moduleDisplayName :: Module -> Text
moduleDisplayName modu =
  fromMaybe "Main" (Syntax.moduleName modu)

findSetupFile :: FilePath -> IO (Maybe FilePath)
findSetupFile sourcePath = do
  let setupHs = sourcePath </> "Setup.hs"
      setupLhs = sourcePath </> "Setup.lhs"
  setupHsExists <- doesFileExist setupHs
  setupLhsExists <- doesFileExist setupLhs
  pure $
    if setupHsExists
      then Just setupHs
      else
        if setupLhsExists
          then Just setupLhs
          else Nothing

stableHash :: [BS.ByteString] -> String
stableHash chunks =
  padLeft 16 '0' (showHex digest "")
  where
    digest = foldl' hashChunk fnvOffset chunks
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * fnvPrime)

fnvOffset :: Word64
fnvOffset = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

padLeft :: Int -> Char -> String -> String
padLeft width char value =
  replicate (max 0 (width - length value)) char <> value

packageSpecValue :: PackageSpec -> Aeson.Value
packageSpecValue spec =
  object
    [ "name" .= pkgName spec,
      "version" .= pkgVersion spec
    ]

packageVariantKeyValue :: PackageVariantKey -> Aeson.Value
packageVariantKeyValue key =
  object
    [ "hash" .= unPackageHash (packageKeyHash key),
      "package" .= packageSpecValue (packageKeySpec key),
      "flags" .= map flagAssignmentValue (packageKeyFlags key),
      "dependencies" .= map resolvedDependencyValue (packageKeyDependencies key)
    ]

resolvedDependencyValue :: ResolvedDependency -> Aeson.Value
resolvedDependencyValue dependency =
  object
    [ "package" .= packageSpecValue (resolvedDependencySpec dependency),
      "hash" .= unPackageHash (resolvedDependencyHash dependency)
    ]

flagAssignmentValue :: (String, Bool) -> Aeson.Value
flagAssignmentValue (flag, enabled) =
  object
    [ "name" .= flag,
      "enabled" .= enabled
    ]

packageFlagAssignments :: GenericPackageDescription -> [(String, Bool)]
packageFlagAssignments gpd =
  sort [(unFlagName (flagName flag), flagDefault flag) | flag <- genPackageFlags gpd]

packageDependencyNames :: GenericPackageDescription -> [String]
packageDependencyNames gpd =
  sort . nub . map T.unpack $
    concatMap libraryDependencies libraryTrees
      <> concatMap (executableDependencies . snd) (condExecutables gpd)
  where
    evalCond = HackageCabal.conditionEvaluator gpd
    libraryTrees =
      maybe [] pure (condLibrary gpd)
        <> map snd (condSubLibraries gpd)

    libraryDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond libBuildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []

    executableDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond buildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []

sortDependencies :: [ResolvedDependency] -> [ResolvedDependency]
sortDependencies =
  sortOn (\dependency -> (pkgName (resolvedDependencySpec dependency), pkgVersion (resolvedDependencySpec dependency), unPackageHash (resolvedDependencyHash dependency)))

sortPackagePlans :: [PackagePlan] -> [PackagePlan]
sortPackagePlans =
  sortOn (\plan -> let spec = packageKeySpec (planPackageKey plan) in (pkgName spec, pkgVersion spec, unPackageHash (packageKeyHash (planPackageKey plan))))

computePackageHash :: PackageSpec -> [(String, Bool)] -> [ResolvedDependency] -> BS.ByteString -> BS.ByteString -> PackageHash
computePackageHash spec flags dependencies cabalBytes setupBytes =
  PackageHash $
    stableHash
      [ BSC.pack "aihc-package-hash-v1",
        BSC.pack (pkgName spec),
        BSC.pack (pkgVersion spec),
        BSC.pack (show flags),
        BSC.pack (show [(pkgName depSpec, pkgVersion depSpec, unPackageHash (resolvedDependencyHash dep)) | dep <- dependencies, let depSpec = resolvedDependencySpec dep]),
        cabalBytes,
        setupBytes,
        BSC.pack "tools:happy,alex,c2hs:planned",
        BSC.pack "phases:setup,configure,build,interfaces,system-fc"
      ]
