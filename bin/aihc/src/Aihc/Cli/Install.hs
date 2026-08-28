module Aihc.Cli.Install
  ( InstallResult (..),
    install,
    runInstall,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, writePackageManifest)
import Aihc.Cli.PackagePlan
  ( DependencyResolver (..),
    PackagePlan (..),
    ParsedInterfaceFile (..),
    buildPackagePlanWithResolver,
    localDependencyResolverWithFallback,
    packageSpecFromSource,
    parseInterfaceFile,
  )
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifact, encodeResolveScope)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeInterface)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..), desugarModuleFc)
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
    tcConfig,
    tcInterfaceBindings,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (tyConModuleName, tyConNamespace, tyConPackageId)
import Aihc.Wasm qualified as Wasm
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when)
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
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import System.Process (readProcessWithExitCode)

data InstallResult = InstallResult
  { installStorePath :: !FilePath,
    installWrittenModules :: ![Text],
    installReusedModules :: ![Text]
  }
  deriving (Eq, Show)

data SourceModule = SourceModule
  { sourceModulePath :: !FilePath,
    sourceModuleHash :: !Text,
    sourceModuleAst :: !Module,
    sourceModuleSourceLines :: !(Map.Map FilePath (Map.Map Int Text))
  }

data InstalledPackage = InstalledPackage
  { installedResult :: !InstallResult,
    installedV2Exports :: !ModuleExports,
    installedV2Types :: !TcInterface,
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

runInstall :: InstallOptions -> IO ()
runInstall options = do
  result <- install options
  putStrLn ("store: " <> installStorePath result)

install :: InstallOptions -> IO InstallResult
install options = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot options)
  let target = installTarget options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
  let root = installPackageDirectory options
      verbose message = when (installVerbose options) (putStrLn message)
      fallbackResolver = networkDependencyResolver
      resolver = localDependencyResolverWithFallback fallbackResolver root
  spec <- packageSpecFromSource root
  plan <- buildPackagePlanWithResolver resolver targetStoreRoot spec
  installedResult <$> installPackagePlan (installKeepGrin options) (installKeepNative options) (installLint options) target verbose targetStoreRoot plan

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

installPackagePlan :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> PackagePlan -> IO InstalledPackage
installPackagePlan keepGrin keepNative lint target verbose storeRoot plan = do
  dependencies <- mapM (installPackagePlan keepGrin keepNative lint target verbose storeRoot) (planDependencyPlans plan)
  installPackageV2 keepGrin keepNative lint target verbose storeRoot dependencies (planSourcePath plan)

installPackageV2 :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> [InstalledPackage] -> FilePath -> IO InstalledPackage
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
  let dependencyIdentities = sortOn id (map (T.pack . takeFileName . installStorePath . installedResult) dependencies)
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
      (installUnit keepGrin keepNative lint target verbose storePath resolvePackage primIdentity root)
      (dependencyExports, dependencyScopeHashes, dependencyTypes, dependencyTypeHashes, Set.empty, Set.empty)
      units
  let archive = storePath </> "lib" </> "lib" <> T.unpack packageNameText <> ".a"
      moduleObjects =
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
      exposedSources = filter ((`Set.member` exposedNames) . sourceName) parsed
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
  exposedTypes <- mapM (readStoredTypeInterface . typePath) exposedSources
  pure
    InstalledPackage
      { installedResult = InstallResult storePath (Set.toAscList written) (Set.toAscList reused),
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

installUnit :: Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> FilePath -> Package -> PackageId -> FilePath -> (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text) -> [SourceModule] -> IO (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text)
installUnit keepGrin keepNative lint target verbose storePath resolvePackage primIdentity root (dependencyExports, scopeHashes, dependencyTypes, typeHashes, written, reused) unit = do
  let packageModules = modulesInPackage resolvePackage (map sourceModuleAst unit)
      unitNames = map sourceName unit
      importedNames = nub (concatMap (map importDeclModule . Syntax.moduleImports . sourceModuleAst) unit)
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- unit]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      resolvePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
      outputPaths = moduleOutputPaths storePath target
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
  (fcChanged, generatedOutputChanged) <-
    if typeChanged || not fcExists
      then do
        (checkedModules, completeInterface) <- maybe checkUnit pure checkedResult
        compileCheckedModules True keepGrin keepNative lint target verbose primIdentity completeInterface outputPaths checkedModules
        pure (True, keepGrin)
      else
        if lint || repairRequired grinStagesExist nativeExists objectExists
          then do
            (checkedModules, completeInterface) <- maybe checkUnit pure checkedResult
            compileCheckedModules False keepGrin keepNative lint target verbose primIdentity completeInterface outputPaths checkedModules
            pure (False, repairRequired grinStagesExist nativeExists objectExists)
          else do
            mapM_ (verbose . ("Reuse FC: " <>) . T.unpack) unitNames
            pure (False, False)
  let changed = resolveChanged || typeChanged || fcChanged || generatedOutputChanged
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
    repairRequired grinStagesExist nativeExists objectExists =
      (keepGrin && not grinStagesExist) || (keepNative && not nativeExists) || not objectExists

compileCheckedModules :: Bool -> Bool -> Bool -> Bool -> NativeTarget -> (String -> IO ()) -> PackageId -> TcInterface -> (Text -> ModuleOutputPaths) -> [Module] -> IO ()
compileCheckedModules writeFc keepGrin keepNative lint target verbose primIdentity interface outputPaths checkedModules = do
  let bindings = tcInterfaceBindings interface <> concatMap tcModuleBindings checkedModules
      config = DesugarConfig primIdentity
      desugarResults = map (desugarModuleFc config bindings interface) checkedModules
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
  grinModules <- mapM lowerGrinModule fcModules
  when keepGrin (mapM_ writeGrinModule grinModules)
  nativeModules <- mapM (generateNativeModule target) grinModules
  mapM_ writeNativeSourceFile nativeModules
  mapM_ compileNativeSourceFile nativeModules
  unless keepNative (mapM_ removeNativeSourceFile nativeModules)
  where
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
      writeFile path (withFinalNewline (Grin.renderProgram program))

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
  runTool archiver (["rcs", archive] <> moduleObjects)
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
  addReferencedFacts
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
