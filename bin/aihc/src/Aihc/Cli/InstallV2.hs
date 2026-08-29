module Aihc.Cli.InstallV2
  ( InstallV2Result (..),
    installV2,
    runInstallV2,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Install
  ( DependencyResolver (..),
    PackagePlan (..),
    ParsedInterfaceFile (..),
    buildPackagePlanWithResolver,
    localDependencyResolverWithFallback,
    packageSpecFromSource,
    parseInterfaceFile,
  )
import Aihc.Cli.Options (InstallV2Options (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, writePackageManifest)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifact, encodeResolveScope)
import Aihc.Cli.Store (defaultStoreRoot)
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
import Aihc.Parser.Syntax (ImportDecl (..), Module, Name (..), SourceSpan (..), moduleName)
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
    TcInterface (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    dataTypeKey,
    mergeTcInterfaces,
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
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (IOException, SomeException, bracket, bracket_, throwIO, try)
import Control.Monad (filterM, foldM, unless, when)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
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
    sourceModuleAst :: !Module,
    sourceModuleSourceLines :: !(Map.Map FilePath (Map.Map Int Text))
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
    pendingSourceSize :: !Int,
    pendingModules :: ![Module]
  }

type InstallUnitState =
  ( ModuleExports,
    Map.Map Text Text,
    Map.Map Text TcInterface,
    Map.Map Text Text,
    Set.Set Text,
    Set.Set Text,
    [PendingCompile],
    TcInterface
  )

runInstallV2 :: InstallV2Options -> IO ()
runInstallV2 options = do
  result <- installV2 options
  putStrLn ("store: " <> installV2StorePath result)

installV2 :: InstallV2Options -> IO InstallV2Result
installV2 options = do
  storeRoot <- maybe defaultStoreRoot pure (installV2StoreRoot options)
  let target = installV2Target options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
  let root = installV2PackageDirectory options
      verbose message = when (installV2Verbose options) (putStrLn message)
      fallbackResolver = networkDependencyResolver
      resolver = localDependencyResolverWithFallback fallbackResolver root
  spec <- packageSpecFromSource root
  plan <- buildPackagePlanWithResolver resolver targetStoreRoot spec
  installedV2Result <$> installPackagePlanV2 (installV2KeepGrin options) (installV2KeepNative options) (installV2Lint options) target verbose targetStoreRoot plan

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

installPackagePlanV2 :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> PackagePlan -> IO InstalledV2Package
installPackagePlanV2 keepGrin keepNative lint target verbose storeRoot plan = do
  dependencies <- mapM (installPackagePlanV2 keepGrin keepNative lint target verbose storeRoot) (planDependencyPlans plan)
  installPackageV2 keepGrin keepNative lint target verbose storeRoot dependencies (planSourcePath plan)

installPackageV2 :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> [InstalledV2Package] -> FilePath -> IO InstalledV2Package
installPackageV2 keepGrin keepNative lint target verbose storeRoot dependencies root = do
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
  parsed <- mapM (parseSource root) files
  let dependencyIdentities = sortOn id (map (T.pack . takeFileName . installV2StorePath . installedV2Result) dependencies)
      packageHash = stableHash (map TE.encodeUtf8 ("aihc-dependencies-v2" : dependencyIdentities))
      packageDirectory = T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash
      storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId (T.pack packageDirectory))
      units = sourceModuleSccs parsed
      dependencyExports = Map.unions (map installedV2Exports dependencies)
      dependencyTypes = Map.unions (map installedV2Types dependencies)
      dependencyScopeHashes = Map.unions (map installedV2ScopeHashes dependencies)
      dependencyTypeHashes = Map.unions (map installedV2TypeHashes dependencies)
      dependencyInstances = mergeTypeInterfaces (map instanceFacts (Map.elems dependencyTypes))
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
  verbose ("Compute " <> show (length units) <> " SCC units")
  (allExports, allScopeHashes, allTypes, allTypeHashes, written, reused, pendingCompiles, packageInstances) <-
    foldM
      (installUnit keepGrin keepNative lint target verbose storePath resolvePackage primIdentity root)
      (dependencyExports, dependencyScopeHashes, dependencyTypes, dependencyTypeHashes, Set.empty, Set.empty, [], dependencyInstances)
      units
  let completeTypes = mergeTypeInterfaces (Map.elems allTypes)
  writePackageInstanceArtifact verbose storePath allTypeHashes completeTypes (ownInstanceFacts resolvePackage packageInstances)
  compilePendingModules keepGrin keepNative lint target verbose primIdentity completeTypes (moduleOutputPaths storePath target) pendingCompiles
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
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

parseSource :: FilePath -> HackageCabal.FileInfo -> IO SourceModule
parseSource root fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  parsed <- parseInterfaceFile root fileInfo
  case parsed of
    ParsedFileOk path modu sourceLines _ -> pure (SourceModule path (BS.length bytes) (T.pack (stableHash [bytes])) modu sourceLines)
    ParsedFileFailed path _ _ _ -> ioError (userError ("Failed to parse " <> path))

sourceModuleSccs :: [SourceModule] -> [[SourceModule]]
sourceModuleSccs = map flatten . stronglyConnComp . map node
  where
    node source = (source, sourceName source, moduleDependencies source)
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst
    moduleImportsOf = Syntax.moduleImports . sourceModuleAst
    moduleDependencies source =
      nub (filter (/= sourceName source) wiredTypeModules <> map importDeclModule (moduleImportsOf source))
    flatten (AcyclicSCC value) = [value]
    flatten (CyclicSCC values) = values

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
    SourceSpan sourceName startLine startColumn _ _ _ _ ->
      sourceName <> ":" <> show startLine <> ":" <> show startColumn

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
    SourceSpan sourceName startLine startColumn endLine endColumn _ _ ->
      case Map.lookup sourceName sourceLines >>= Map.lookup startLine of
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

installUnit :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> Package -> PackageId -> FilePath -> InstallUnitState -> [SourceModule] -> IO InstallUnitState
installUnit keepGrin keepNative lint target verbose storePath resolvePackage primIdentity root (dependencyExports, scopeHashes, dependencyTypes, typeHashes, written, reused, pendingCompiles, globalInstances) unit = do
  let packageModules = modulesInPackage resolvePackage (map sourceModuleAst unit)
      unitNames = map sourceName unit
      importedNames = nub (concatMap (map importDeclModule . Syntax.moduleImports . sourceModuleAst) unit)
      dependencyNames = nub (importedNames <> wiredTypeModules)
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- unit]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      resolvePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
      outputPaths = moduleOutputPaths storePath target
      importedTypes =
        applyInstanceFacts
          globalInstances
          ( mergeTypeInterfaces
              [ interface
              | name <- dependencyNames,
                name `notElem` unitNames,
                Just interface <- [Map.lookup name dependencyTypes]
              ]
          )
  cachedExports <- tryReadUnitArtifacts hashes resolvePackage resolvePath unit
  (diskExports, resolveResult, resolveChanged) <- case cachedExports of
    Just exports -> do
      mapM_ (verbose . ("Reuse resolve context: " <>) . T.unpack) unitNames
      pure (exports, Nothing, False)
    Nothing -> do
      let result = resolveWithDeps dependencyExports packageModules
      unless (null (resolveErrors result)) (ioError (userError (renderResolveErrors unit (resolveErrors result))))
      let exports = extractInterfaceWithDeps dependencyExports result
      mapM_ (\source -> writeArtifact verbose hashes exports resolvePackage (resolvePath source) source) unit
      storedExports <- readUnitArtifacts hashes resolvePackage resolvePath unit
      pure (storedExports, Just result, True)
  updatedScopeHashes <- updateScopeHashes resolvePath scopeHashes unit
  let typeInputs =
        sortOn fst $
          sourceHashes
            <> Map.toList dependencyHashes
            <> [("type:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name typeHashes]]
      checkUnit = do
        resolved <- case resolveResult of
          Just result -> pure result
          Nothing -> do
            let result = resolveWithDeps dependencyExports packageModules
            unless (null (resolveErrors result)) (ioError (userError (renderResolveErrors unit (resolveErrors result))))
            pure result
        let checked@(checkedModules, _) =
              typecheckModuleSccWithInterface
                (tcConfig primIdentity)
                importedTypes
                (map snd (resolvedModules resolved))
        unless (all tcModuleSuccess checkedModules) (ioError (userError ("Type check failed: " <> show (concatMap tcModuleDiagnostics checkedModules))))
        pure checked
  cachedTypes <- tryReadTypeArtifacts typeInputs typePath unit
  (unitTypes, typeChanged, checkedResult) <- case cachedTypes of
    Just interfaces -> do
      mapM_ (verbose . ("Reuse type interface: " <>) . T.unpack) unitNames
      pure (interfaces, False, Nothing)
    Nothing -> do
      checked@(_, completeInterface) <- checkUnit
      let interfaces = map (moduleTypeInterface diskExports resolvePackage completeInterface) unit
      mapM_ (uncurry (writeTypeArtifact verbose typeInputs typePath)) (zip unit interfaces)
      pure (interfaces, True, Just checked)
  let updatedTypeHashes = updateTypeHashes typeHashes (zip unitNames unitTypes)
  fcExists <- and <$> mapM (doesFileExist . outputFcPath . outputPaths . sourceName) unit
  grinStagesExist <-
    and
      <$> mapM
        ( \source -> do
            let paths = outputPaths (sourceName source)
            and <$> mapM doesFileExist [outputGrinPath paths, outputCpsGrinPath paths, outputGcGrinPath paths]
        )
        unit
  objectExists <- and <$> mapM (doesFileExist . outputObjectPath . outputPaths . sourceName) unit
  nativeExists <- and <$> mapM (doesFileExist . outputNativePath . outputPaths . sourceName) unit
  (fcChanged, generatedOutputChanged, pendingCompile) <-
    if typeChanged || not fcExists
      then do
        (checkedModules, _) <- maybe checkUnit pure checkedResult
        pure (True, keepGrin, Just (PendingCompile True unitSourceSize checkedModules))
      else
        if lint || repairRequired grinStagesExist nativeExists objectExists
          then do
            (checkedModules, _) <- maybe checkUnit pure checkedResult
            pure (False, repairRequired grinStagesExist nativeExists objectExists, Just (PendingCompile False unitSourceSize checkedModules))
          else do
            mapM_ (verbose . ("Reuse FC: " <>) . T.unpack) unitNames
            pure (False, False, Nothing)
  let changed = resolveChanged || typeChanged || fcChanged || generatedOutputChanged
      unitSet = Set.fromList unitNames
      localUnitTypes = Map.fromList (zip unitNames unitTypes)
      nextTypes = localUnitTypes `Map.union` dependencyTypes
      nextInstances =
        case checkedResult of
          Just (_, completeInterface) -> instanceFacts completeInterface
          Nothing -> globalInstances <> instanceFacts (mergeTypeInterfaces unitTypes)
      written' = if changed then written <> unitSet else written
      reused' = if changed then reused else reused <> unitSet
  pure
    ( diskExports `Map.union` dependencyExports,
      updatedScopeHashes,
      nextTypes,
      updatedTypeHashes,
      written',
      reused',
      maybe pendingCompiles (: pendingCompiles) pendingCompile,
      nextInstances
    )
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst
    unitSourceSize = sum (map sourceModuleSize unit)
    repairRequired grinStagesExist nativeExists objectExists =
      (keepGrin && not grinStagesExist) || (keepNative && not nativeExists) || not objectExists

mergeTypeInterfaces :: [TcInterface] -> TcInterface
mergeTypeInterfaces = mergeTcInterfaces

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

compilePendingModules :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> PackageId -> TcInterface -> (Text -> ModuleOutputPaths) -> [PendingCompile] -> IO ()
compilePendingModules keepGrin keepNative lint target verbose primIdentity interface outputPaths pending = do
  prepared <- either (ioError . userError . ("FC environment generation failed: " <>)) pure (Fc.prepareDesugar (DesugarConfig primIdentity) interface)
  capabilities <- getNumCapabilities
  semaphore <- newQSem (max 1 (capabilities * 3))
  results <- mapConcurrently (runCompile semaphore prepared) scheduled
  mapM_ (report . snd) (sortOn fst results)
  where
    scheduled = sortOn scheduleKey pending
    pendingNames = map (fromMaybe "Main" . moduleName) . pendingModules
    scheduleKey item = (Down (pendingSourceSize item), pendingNames item)
    runCompile semaphore prepared item = do
      messages <- newIORef []
      let saveMessage message = modifyIORef' messages (message :)
          action =
            compileCheckedModules
              (pendingWriteFc item)
              keepGrin
              keepNative
              lint
              target
              saveMessage
              prepared
              outputPaths
              (pendingModules item)
      result <- try (bracket_ (waitQSem semaphore) (signalQSem semaphore) action)
      saved <- reverse <$> readIORef messages
      pure (pendingNames item, (result :: Either SomeException (), saved))
    report (result, messages) = do
      mapM_ verbose messages
      either throwIO pure result

compileCheckedModules :: Bool -> Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> Fc.PreparedDesugar -> (Text -> ModuleOutputPaths) -> [Module] -> IO ()
compileCheckedModules writeFc keepGrin keepNative lint target verbose prepared outputPaths checkedModules = do
  let bindings = concatMap tcModuleBindings checkedModules
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
      when keepGrin $ do
        writeFile (outputGrinPath paths) ""
        writeFile (outputCpsGrinPath paths) ""
        writeFile (outputGcGrinPath paths) ""
      when keepNative (writeFile (outputNativePath paths) "")
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
      when lint $ do
        let plainErrors = Grin.lintProgram plainProgram
        unless (null plainErrors) (ioError (userError ("GRIN lint failed: " <> show plainErrors)))
      cpsProgram <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin plainProgram)
      let gcProgram = Grin.lowerGc cpsProgram
      when lint $ do
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
      (compiler, compilerArguments) <- backendCompiler target
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

tryReadTypeArtifacts :: [(Text, Text)] -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe [TcInterface])
tryReadTypeArtifacts expected artifactPath unit = do
  result <- try (readTypeArtifacts expected artifactPath unit) :: IO (Either IOException [TcInterface])
  pure (either (const Nothing) Just result)

readTypeArtifacts :: [(Text, Text)] -> (SourceModule -> FilePath) -> [SourceModule] -> IO [TcInterface]
readTypeArtifacts expected artifactPath = mapM readOne
  where
    readOne source = do
      let path = artifactPath source
      bytes <- BS.readFile path
      artifact <- either (\message -> ioError (userError ("Invalid type artifact " <> path <> ": " <> message))) pure (decodeTypeArtifact bytes)
      unless (typeArtifactInputHashes artifact == expected) (ioError (userError ("Invalid type artifact " <> path <> ": input hashes do not match")))
      pure (typeArtifactInterface artifact)

updateTypeHashes :: Map.Map Text Text -> [(Text, TcInterface)] -> Map.Map Text Text
updateTypeHashes = foldl' insertHash
  where
    insertHash result (name, interface) =
      Map.insert name (T.pack (stableHash [BL.toStrict (encodeTypeInterface interface)])) result

updateScopeHashes :: (SourceModule -> FilePath) -> Map.Map Text Text -> [SourceModule] -> IO (Map.Map Text Text)
updateScopeHashes artifactPath previous unit = do
  scopeHashes <- mapM artifactScopeHash unit
  pure (foldl' (\hashes (name, digest) -> Map.insert name digest hashes) previous scopeHashes)
  where
    artifactScopeHash source = do
      bytes <- BS.readFile (artifactPath source)
      artifact <- either (\message -> ioError (userError ("Invalid resolve artifact while hashing scope: " <> message))) pure (decodeResolveArtifact bytes)
      let scopeBytes = BL.toStrict (encodeResolveScope (resolveArtifactScope artifact))
      pure (resolveArtifactModuleName artifact, T.pack (stableHash [scopeBytes]))

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

tryReadUnitArtifacts :: [(Text, Text)] -> Package -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe ModuleExports)
tryReadUnitArtifacts expected package artifactPath unit = do
  result <- try (readUnitArtifacts expected package artifactPath unit) :: IO (Either IOException ModuleExports)
  pure (either (const Nothing) Just result)

readUnitArtifacts :: [(Text, Text)] -> Package -> (SourceModule -> FilePath) -> [SourceModule] -> IO ModuleExports
readUnitArtifacts expected package artifactPath unit = do
  entries <- mapM readOne unit
  pure (Map.fromList entries)
  where
    readOne source = do
      let path = artifactPath source
      bytes <- BS.readFile path
      artifact <- either (\message -> ioError (userError ("Invalid resolve artifact " <> path <> ": " <> message))) pure (decodeResolveArtifact bytes)
      unless (resolveArtifactInputHashes artifact == expected) (ioError (userError ("Invalid resolve artifact " <> path <> ": input hashes do not match")))
      pure (ModuleKey package (resolveArtifactModuleName artifact), resolveArtifactScope artifact)

stableHash :: [BS.ByteString] -> String
stableHash chunks = replicate (16 - length rendered) '0' <> rendered
  where
    rendered = showHex (foldl' hashChunk (14695981039346656037 :: Word64) chunks) ""
    hashChunk :: Word64 -> BS.ByteString -> Word64
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * 1099511628211)
