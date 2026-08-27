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
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..), desugarModuleFc)
import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as FcType
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
    Pred (..),
    TcInterface (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConInfo (..),
    TypeScheme (..),
    dataTypeKey,
    tcConfig,
    tcInterfaceBindings,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Types (tyConModuleName, tyConNamespace, tyConPackageId)
import Aihc.Wasm qualified as Wasm
import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (intercalate, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
import System.Directory (createDirectoryIfMissing, createFileLink, doesFileExist, removeFile)
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
    sourceModuleHash :: !Text,
    sourceModuleAst :: !Module,
    sourceModuleSourceLines :: !(Map.Map FilePath (Map.Map Int Text))
  }

data InstalledV2Package = InstalledV2Package
  { installedV2Result :: !InstallV2Result,
    installedV2Exports :: !ModuleExports,
    installedV2Types :: !TcInterface,
    installedV2ScopeHashes :: !(Map.Map Text Text),
    installedV2TypeHashes :: !(Map.Map Text Text)
  }

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
  installedV2Result <$> installPackagePlanV2 (installV2KeepGrin options) (installV2KeepNative options) target verbose targetStoreRoot plan

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

installPackagePlanV2 :: Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> PackagePlan -> IO InstalledV2Package
installPackagePlanV2 keepGrin keepNative target verbose storeRoot plan = do
  dependencies <- mapM (installPackagePlanV2 keepGrin keepNative target verbose storeRoot) (planDependencyPlans plan)
  installPackageV2 keepGrin keepNative target verbose storeRoot dependencies (planSourcePath plan)

installPackageV2 :: Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> [InstalledV2Package] -> FilePath -> IO InstalledV2Package
installPackageV2 keepGrin keepNative target verbose storeRoot dependencies root = do
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
      dependencyTypes = mconcat (map installedV2Types dependencies)
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
  verbose ("Compute " <> show (length units) <> " SCC units")
  (allExports, allScopeHashes, _, allTypeHashes, written, reused) <-
    foldM
      (installUnit keepGrin keepNative target verbose storePath resolvePackage primIdentity root)
      (dependencyExports, dependencyScopeHashes, dependencyTypes, dependencyTypeHashes, Set.empty, Set.empty)
      units
  buildLibraryArchive keepNative target verbose storePath packageNameText resolvePackage parsed
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
            ]
      }
  let exposedNames = Set.fromList (HackageCabal.collectLibraryExposedModules gpd)
      ownExports =
        Map.filterWithKey
          (\moduleKey _ -> moduleKeyPackage moduleKey == resolvePackage && moduleKeyName moduleKey `Set.member` exposedNames)
          allExports
      exposedSources = filter ((`Set.member` exposedNames) . sourceName) parsed
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
  exposedTypes <- mapM (readStoredTypeInterface . typePath) exposedSources
  pure
    InstalledV2Package
      { installedV2Result = InstallV2Result storePath (Set.toAscList written) (Set.toAscList reused),
        installedV2Exports = ownExports,
        installedV2Types = mconcat exposedTypes,
        installedV2ScopeHashes = Map.restrictKeys allScopeHashes exposedNames,
        installedV2TypeHashes = Map.restrictKeys allTypeHashes exposedNames
      }
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

readStoredTypeInterface :: FilePath -> IO TcInterface
readStoredTypeInterface path = do
  bytes <- BS.readFile path
  artifact <- either (\message -> ioError (userError ("Invalid type artifact " <> path <> ": " <> message))) pure (decodeTypeArtifact bytes)
  pure (typeArtifactInterface artifact)

parseSource :: FilePath -> HackageCabal.FileInfo -> IO SourceModule
parseSource root fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  parsed <- parseInterfaceFile root fileInfo
  case parsed of
    ParsedFileOk path modu sourceLines _ -> pure (SourceModule path (T.pack (stableHash [bytes])) modu sourceLines)
    ParsedFileFailed path _ _ _ -> ioError (userError ("Failed to parse " <> path))

sourceModuleSccs :: [SourceModule] -> [[SourceModule]]
sourceModuleSccs = map flatten . stronglyConnComp . map node
  where
    node source = (source, sourceName source, moduleDependencies source)
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst
    moduleImportsOf = Syntax.moduleImports . sourceModuleAst
    moduleDependencies source
      | sourceName source == "GHC.Types" = map importDeclModule (moduleImportsOf source)
      | otherwise = "GHC.Types" : map importDeclModule (moduleImportsOf source)
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

installUnit :: Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> Package -> PackageId -> FilePath -> (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text) -> [SourceModule] -> IO (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text)
installUnit keepGrin keepNative target verbose storePath resolvePackage primIdentity root (dependencyExports, scopeHashes, dependencyTypes, typeHashes, written, reused) unit = do
  let packageModules = modulesInPackage resolvePackage (map sourceModuleAst unit)
      unitNames = map sourceName unit
      importedNames = nub (concatMap (map importDeclModule . Syntax.moduleImports . sourceModuleAst) unit)
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- unit]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      resolvePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
      corePath modu = storePath </> moduleDirectory modu </> "core"
      grinPath modu = storePath </> moduleDirectory modu </> "grin"
      objectPath modu = storePath </> moduleDirectory modu </> T.unpack (fromMaybe "Main" (moduleName modu)) <> ".o"
      nativePath modu = objectPath modu <> nativeSourceExtension target
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
            <> [("type:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name typeHashes]]
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
                dependencyTypes
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
      storedInterfaces <- readTypeArtifacts typeInputs typePath unit
      pure (storedInterfaces, True, Just checked)
  updatedTypeHashes <- updateTypeHashes typePath typeHashes unit
  coreExists <- and <$> mapM (doesFileExist . corePath . sourceModuleAst) unit
  grinExists <- and <$> mapM (doesFileExist . grinPath . sourceModuleAst) unit
  objectExists <- and <$> mapM (doesFileExist . objectPath . sourceModuleAst) unit
  nativeExists <- and <$> mapM (doesFileExist . nativePath . sourceModuleAst) unit
  (coreChanged, grinChanged) <-
    if typeChanged || not coreExists
      then do
        (checkedModules, completeInterface) <- maybe checkUnit pure checkedResult
        writeCoreFiles True keepGrin keepNative target verbose (packageId resolvePackage) primIdentity completeInterface (takeDirectory storePath) corePath grinPath objectPath checkedModules
        pure (True, keepGrin)
      else
        if (keepGrin && not grinExists) || (keepNative && not nativeExists) || not objectExists
          then do
            (checkedModules, completeInterface) <- maybe checkUnit pure checkedResult
            writeCoreFiles False keepGrin keepNative target verbose (packageId resolvePackage) primIdentity completeInterface (takeDirectory storePath) corePath grinPath objectPath checkedModules
            pure (False, True)
          else do
            mapM_ (verbose . ("Reuse Core: " <>) . T.unpack) unitNames
            pure (False, False)
  let changed = resolveChanged || typeChanged || coreChanged || grinChanged
      unitSet = Set.fromList unitNames
      localUnitTypes = mconcat unitTypes
      written' = if changed then written <> unitSet else written
      reused' = if changed then reused else reused <> unitSet
  pure
    ( diskExports `Map.union` dependencyExports,
      updatedScopeHashes,
      dependencyTypes <> localUnitTypes,
      updatedTypeHashes,
      written',
      reused'
    )
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

writeCoreFiles :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> PackageId -> PackageId -> TcInterface -> FilePath -> (Module -> FilePath) -> (Module -> FilePath) -> (Module -> FilePath) -> [Module] -> IO ()
writeCoreFiles keepCore keepGrin keepNative target verbose currentPackage primIdentity interface storeRoot corePath grinPath objectPath checkedModules = do
  let bindings = tcInterfaceBindings interface <> concatMap tcModuleBindings checkedModules
      config = DesugarConfig primIdentity
      results = map (desugarModuleFc config bindings interface) checkedModules
      currentModules = Set.fromList (map (fromMaybe "Main" . moduleName) checkedModules)
      storeLoader = Fc.storeModuleLoader storeRoot
      dependencyLoader package name
        | package == currentPackage && name `Set.member` currentModules = pure Nothing
        | otherwise = storeLoader package name
  unless (all dsSuccess results) (ioError (userError ("Core generation failed: " <> unlines (concatMap dsErrors results))))
  loadedFc <- Fc.loadScopeClosure dependencyLoader (map dsProgram results)
  let fcErrors = Fc.lintPrograms loadedFc
      fcReport = map (("    " <>) . show) fcErrors
  unless (null fcErrors) $ do
    mapM_ writeBadFc (zip checkedModules results)
    ioError
      ( userError
          ( unlines
              ( ["Core lint failed:"]
                  <> fcReport
              )
          )
      )
  when keepCore (mapM_ writeCore (zip checkedModules results))
  when keepGrin $ do
    let typeEnv = FcType.typeEnvFromPrograms loadedFc
    mapM_ (writeGrin typeEnv) (zip checkedModules results)
  let typeEnv = FcType.typeEnvFromPrograms loadedFc
  mapM_ (writeObject target typeEnv) (zip checkedModules results)
  where
    writeBadFc (modu, result) = do
      let badPath = corePath modu <> ".bad"
          name = fromMaybe "Main" (moduleName modu)
      removeFileIfExists (corePath modu)
      writeCoreFile badPath result
      verbose ("Write bad Core: " <> T.unpack name <> " -> " <> badPath)

    writeCore (modu, result) = do
      let outputPath = corePath modu
          name = fromMaybe "Main" (moduleName modu)
      removeFileIfExists (outputPath <> ".bad")
      writeCoreFile outputPath result
      verbose ("Write Core: " <> T.unpack name)

    writeCoreFile path result = do
      let rendered = Fc.renderProgram (dsProgram result)
          output = if "\n" `isSuffixOf` rendered then rendered else rendered <> "\n"
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path output

    writeGrin typeEnv (modu, result) = do
      program <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram typeEnv (dsProgram result))
      let errors = Grin.lintProgram program
      unless (null errors) (ioError (userError ("GRIN lint failed: " <> show errors)))
      let path = grinPath modu
          rendered = Grin.renderProgram program
          output = if "\n" `isSuffixOf` rendered then rendered else rendered <> "\n"
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path output
      verbose ("Write GRIN: " <> T.unpack (fromMaybe "Main" (moduleName modu)))

    writeObject selectedTarget typeEnv (modu, result) = do
      program <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram typeEnv (dsProgram result))
      cps <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin program)
      let gcProgram = Grin.lowerGc cps
          gcErrors = Grin.lintProgram (Grin.gcGrinProgram gcProgram)
      unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed: " <> show gcErrors)))
      assembly <- compileNativeModule selectedTarget gcProgram
      let path = objectPath modu
          assemblyPath = path <> nativeSourceExtension selectedTarget
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile assemblyPath assembly
      (compiler, compilerArguments) <- backendCompiler selectedTarget
      runTool compiler (compilerArguments <> ["-c", assemblyPath, "-o", path])
      unless keepNative (removeFile assemblyPath)
      verbose ("Write object: " <> T.unpack (fromMaybe "Main" (moduleName modu)))

    removeFileIfExists path = do
      exists <- doesFileExist path
      when exists (removeFile path)

compileNativeModule :: NativeTarget -> Grin.GcGrinProgram -> IO Text
compileNativeModule target gcProgram =
  case target of
    AppleArm64 -> either (ioError . userError . ("Apple ARM64 generation failed: " <>) . show) pure (Arm64.compileModule gcProgram)
    LinuxAmd64 -> either (ioError . userError . ("Linux AMD64 generation failed: " <>) . show) pure (Amd64.compileModule gcProgram)
    Llvm -> either (ioError . userError . ("LLVM generation failed: " <>) . show) pure (Llvm.compileModule gcProgram)
    Wasm32Wasip3 -> either (ioError . userError . ("WebAssembly generation failed: " <>) . show) pure (Wasm.compileModule gcProgram)

nativeSourceExtension :: NativeTarget -> String
nativeSourceExtension target =
  case target of
    Llvm -> ".ll"
    _ -> ".s"

buildLibraryArchive :: Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> Text -> Package -> [SourceModule] -> IO ()
buildLibraryArchive keepNative target verbose storePath packageNameText resolvePackage sources = do
  let archive = storePath </> "lib" </> "lib" <> T.unpack packageNameText <> ".a"
      moduleObjects =
        [ storePath </> moduleDirectory modu </> T.unpack (fromMaybe "Main" (moduleName modu)) <> ".o"
        | source <- sources,
          let modu = sourceModuleAst source
        ]
      object = case moduleObjects of
        [singleObject] -> singleObject
        _ -> storePath </> "lib" </> T.unpack packageNameText <> ".o"
      loader = Fc.storeModuleLoader (takeDirectory storePath)
      packageIdentity = packageId resolvePackage
  ownPrograms <-
    forM sources $ \source -> do
      let name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      loaded <- loader packageIdentity name
      maybe (ioError (userError ("Missing stored Core module: " <> T.unpack name))) pure loaded
  loadedPrograms <- Fc.loadScopeClosure loader ownPrograms
  let typeEnv = FcType.typeEnvFromPrograms loadedPrograms
      combinedProgram =
        Fc.Program
          { Fc.programScopes = maybe Fc.emptyScopeTable Fc.programScopes (listToMaybe ownPrograms),
            Fc.programDecls = concatMap Fc.programDecls ownPrograms
          }
  grin <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram typeEnv combinedProgram)
  cps <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin grin)
  let gcProgram = Grin.lowerGc cps
      gcErrors = Grin.lintProgram (Grin.gcGrinProgram gcProgram)
  unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed: " <> show gcErrors)))
  assembly <- compileNativeModule target gcProgram
  let assemblyPath = object <> nativeSourceExtension target
  createDirectoryIfMissing True (takeDirectory object)
  TIO.writeFile assemblyPath assembly
  (compiler, compilerArguments) <- backendCompiler target
  runTool compiler (compilerArguments <> ["-c", assemblyPath, "-o", object])
  unless keepNative (removeFile assemblyPath)
  forM_ moduleObjects $ \moduleObject ->
    unless (moduleObject == object) $ do
      createDirectoryIfMissing True (takeDirectory moduleObject)
      exists <- doesFileExist moduleObject
      when exists (removeFile moduleObject)
      createFileLink object moduleObject
  createDirectoryIfMissing True (takeDirectory archive)
  archiveExists <- doesFileExist archive
  when archiveExists (removeFile archive)
  archiver <- backendArchiver target
  runTool archiver ["rcs", archive, object]
  verbose ("Write archive: " <> archive)

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
  addReferencedTyCons
    interface
    interface
      { tcInterfaceTerms = filter visibleTerm (tcInterfaceTerms interface),
        tcInterfaceTyCons = filter visibleTyCon (tcInterfaceTyCons interface),
        tcInterfaceDataTypes = filter (visibleTypeIdentity . dataTypeKey) (tcInterfaceDataTypes interface),
        tcInterfaceClasses = filter visibleClass (tcInterfaceClasses interface)
      }
  where
    name = fromMaybe "Main" (moduleName (sourceModuleAst source))
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
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
    resolvedIdentity resolved = case resolved of
      ResolvedTopLevel packageId' resolvedName -> Just (packageId', fromMaybe name (nameQualifier resolvedName), nameText resolvedName)
      _ -> Nothing

addReferencedTyCons :: TcInterface -> TcInterface -> TcInterface
addReferencedTyCons complete interface =
  interface {tcInterfaceTyCons = Map.elems (existing <> support)}
  where
    existing = Map.fromList [(tciTyCon info, info) | info <- tcInterfaceTyCons interface]
    available = Map.fromList [(tciTyCon info, info) | info <- tcInterfaceTyCons complete]
    referenced =
      Set.fromList
        ( concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface)
            <> concatMap (typeSchemeTyCons . tciKindScheme) (tcInterfaceTyCons interface)
        )
    reachable = closeTyCons Set.empty referenced
    support = Map.restrictKeys available (reachable `Set.difference` Map.keysSet existing)
    closeTyCons found pending
      | Set.null pending = found
      | otherwise =
          let (tyCon, pending') = Set.deleteFindMin pending
              dependencies =
                maybe
                  Set.empty
                  (Set.fromList . typeSchemeTyCons . tciKindScheme)
                  (Map.lookup tyCon available)
              found' = Set.insert tyCon found
           in closeTyCons found' (pending' <> (dependencies `Set.difference` found'))

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

updateTypeHashes :: (SourceModule -> FilePath) -> Map.Map Text Text -> [SourceModule] -> IO (Map.Map Text Text)
updateTypeHashes artifactPath previous unit = do
  hashes <- mapM artifactHash unit
  pure (foldl' (\result (name, digest) -> Map.insert name digest result) previous hashes)
  where
    artifactHash source = do
      bytes <- BS.readFile (artifactPath source)
      artifact <- either (\message -> ioError (userError ("Invalid type artifact while hashing interface: " <> message))) pure (decodeTypeArtifact bytes)
      pure (typeArtifactModuleName artifact, T.pack (stableHash [BL.toStrict (encodeTypeInterface (typeArtifactInterface artifact))]))

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
moduleDirectory = foldl' (</>) "" . map T.unpack . T.splitOn "." . fromMaybe "Main" . moduleName

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
