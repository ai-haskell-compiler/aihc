{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Installed-library compilation and loading for @aihc compile@.
-- Incremental caches retain only frontend and linking interfaces; a companion
-- whole-program cache keeps Core and GRIN bodies for backend construction and
-- explicit whole-program compilation.
module Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    DependencyUnit (..),
    LibraryPackage (..),
    buildDependencies,
    installLibraries,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Runtime (readWasmClangProcessWithExitCode, wasmClangCommand)
import Aihc.Cli.Store (installedLibrariesActivePath, installedLibrariesRoot)
import Aihc.Fc (AxiomInterface, DesugarResult (..), FcProgram (..), NewtypeInterface, ReachabilityInterface, desugarModuleWithBindings, extractAxiomInterface, extractNewtypeInterface, extractReachabilityInterface, lowerNewtypesWithInterface, lowerPseudoOps, optimizeProgram)
import Aihc.Grin qualified as Grin
import Aihc.Llvm qualified as Llvm
import Aihc.Native
  ( LinkInterface,
    LinkLayout,
    NativeTarget (..),
    backendCompiler,
    buildLinkLayoutFromInterfaces,
    extractLinkInterface,
    nativeTargetTriple,
    renderLinkedFunctionSymbol,
  )
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( ImportDecl (importDeclModule),
    LanguageEdition (Haskell98Edition),
    Module (..),
    Name (..),
    NameType,
    UnqualifiedName (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    OperatorFixity,
    Package (..),
    PackageId (..),
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    extractInterfaceWithDeps,
    resolveWithDeps,
  )
import Aihc.Tc
  ( TcBindingResult (..),
    TcInterface,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Wasm qualified as Wasm
import Control.Exception (bracket, bracketOnError)
import Control.Monad (foldM)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (intercalate, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
    renameFile,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath
  ( makeRelative,
    takeDirectory,
    (</>),
  )
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

newtype CompileEnvironment = CompileEnvironment
  { compileInstalledStoreRoot :: FilePath
  }
  deriving (Eq, Show)

data DependencyArtifact = DependencyArtifact
  { dependencyExports :: !ModuleExports,
    dependencyTcInterface :: !TcInterface,
    dependencyBindings :: ![TcBindingResult],
    dependencyAxiomInterface :: !AxiomInterface,
    dependencyNewtypeInterface :: !NewtypeInterface,
    dependencyGrinInterface :: !Grin.GrinInterface,
    dependencyReachabilityInterface :: !ReachabilityInterface,
    dependencyLinkInterfaces :: ![LinkInterface],
    dependencyRuntimePrimitiveNames :: !(Set.Set Text),
    dependencyUnits :: ![DependencyUnit],
    dependencyUnitMetadata :: ![DependencyUnitMetadata],
    dependencyInitializerSymbols :: ![Text],
    dependencyArchivePaths :: ![FilePath]
  }

data DependencyUnit = DependencyUnit
  { dependencyUnitLibraries :: ![Text],
    dependencyUnitModules :: ![Text],
    dependencyUnitInitializer :: !Text,
    dependencyUnitProgram :: !FcProgram,
    dependencyUnitGrin :: !Grin.GrinProgram,
    dependencyUnitCpsGrin :: !Grin.CpsGrinProgram,
    dependencyUnitAxiomInterface :: !AxiomInterface,
    dependencyUnitNewtypeInterface :: !NewtypeInterface,
    dependencyUnitGrinInterface :: !Grin.GrinInterface,
    dependencyUnitReachabilityInterface :: !ReachabilityInterface,
    dependencyUnitLinkInterface :: !LinkInterface
  }
  deriving (Eq, Show, Read)

data DependencyUnitMetadata = DependencyUnitMetadata
  { dependencyMetadataLibraries :: ![Text],
    dependencyMetadataModules :: ![Text],
    dependencyMetadataInitializer :: !Text
  }
  deriving (Eq, Show, Read)

data StoredDependencyArtifact = StoredDependencyArtifact
  { storedSchemaVersion :: !Int,
    storedExports :: !StoredModuleExports,
    storedTcInterface :: !TcInterface,
    storedBindings :: ![TcBindingResult],
    storedAxiomInterface :: !AxiomInterface,
    storedNewtypeInterface :: !NewtypeInterface,
    storedGrinInterface :: !Grin.GrinInterface,
    storedReachabilityInterface :: !ReachabilityInterface,
    storedLinkInterfaces :: ![LinkInterface],
    storedRuntimePrimitiveNames :: !(Set.Set Text),
    storedUnitMetadata :: ![DependencyUnitMetadata],
    storedUnits :: !(Maybe [DependencyUnit])
  }
  deriving (Show, Read)

newtype StoredModuleExports = StoredModuleExports [(Text, Text, Text, StoredScope)]
  deriving (Show, Read)

data StoredScope = StoredScope
  { storedScopeTerms :: ![(Text, StoredResolvedName)],
    storedScopeTypes :: ![(Text, StoredResolvedName)],
    storedScopeConstructors :: ![(Text, [Text])],
    storedScopeRecordFields :: ![(Text, [Text])],
    storedScopeMethods :: ![(Text, [Text])],
    storedScopeFixities :: ![(Text, OperatorFixity)],
    storedScopeQualifiedModules :: ![(Text, StoredScope)]
  }
  deriving (Show, Read)

data StoredResolvedName
  = StoredTopLevel !Text !(Maybe Text) !NameType !Text
  | StoredLocal !Int !NameType !Text
  | StoredBuiltin !Text
  | StoredError !String
  deriving (Show, Read)

data LoadedModule = LoadedModule
  { loadedLibrary :: !Text,
    loadedLibraryId :: ![Text],
    loadedModuleExposed :: !Bool,
    loadedModule :: !Module
  }

-- | One Cabal-selected library package in an installation closure.
data LibraryPackage = LibraryPackage
  { libraryPackageName :: !Text,
    libraryPackageId :: ![Text],
    libraryPackageRoot :: !FilePath,
    libraryPackageFiles :: ![FilePath],
    libraryPackageExposedModules :: ![Text]
  }
  deriving (Eq, Show)

cacheSchemaVersion :: Int
cacheSchemaVersion = 34

buildDependencies :: NativeTarget -> CompileEnvironment -> Bool -> Bool -> Module -> IO (Either String DependencyArtifact)
buildDependencies target environment usesImplicitPrelude buildBackend mainModule = do
  let importedRoots = map importDeclModule (moduleImports mainModule)
      defaultRoots = ["GHC.Prim", "GHC.TopHandler"] <> ["Prelude" | usesImplicitPrelude]
      requiredModules = sort (Set.toList (Set.fromList (defaultRoots <> importedRoots)))
  installed <- readInstalledLibraries environment buildBackend
  case installed of
    Left err -> pure (Left err)
    Right (artifactRoot, artifact) ->
      case filter (\name -> not (any ((== name) . moduleKeyName) (Map.keys (dependencyExports artifact)))) requiredModules of
        missing@(_ : _) -> pure (Left ("library modules are not installed: " <> T.unpack (T.intercalate ", " missing)))
        []
          | buildBackend -> attachInstalledBackend target artifactRoot artifact
          | otherwise -> pure (Right artifact)

-- | Compile a package closure once, then install its frontend interfaces,
-- whole-program bodies, and one archive set per requested target.
installLibraries :: FilePath -> [LibraryPackage] -> [NativeTarget] -> IO (Either String FilePath)
installLibraries storeRoot packages targets
  | all (null . libraryPackageFiles) packages = pure (Left "the package closure contains no library modules")
  | otherwise = do
      graphHash <- dependencyGraphHash packages
      let librariesRoot = installedLibrariesRoot storeRoot
          artifactRoot = librariesRoot </> graphHash
          incrementalPath = artifactRoot </> "interfaces.cache"
          wholePath = artifactRoot </> "whole.cache"
      cached <- readCache wholePath
      artifactResult <-
        case cached of
          Just artifact
            | not (null (dependencyUnits artifact)) -> pure (Right artifact)
          _ -> do
            loadedResult <- loadLibraryPackages packages
            pure (loadedResult >>= compileLoadedModules)
      case artifactResult of
        Left err -> pure (Left err)
        Right artifact -> do
          createDirectoryIfMissing True artifactRoot
          writeCache artifactRoot incrementalPath False artifact
          writeCache artifactRoot wholePath True artifact
          backendResults <-
            mapM
              (\target -> buildBackendArtifacts target (installedBackendRoot artifactRoot target) (dependencyUnits artifact))
              (sort (Set.toList (Set.fromList targets)))
          case sequence backendResults of
            Left err -> pure (Left err)
            Right _ -> do
              writeActiveLibraries storeRoot graphHash
              pure (Right artifactRoot)

readInstalledLibraries :: CompileEnvironment -> Bool -> IO (Either String (FilePath, DependencyArtifact))
readInstalledLibraries environment incremental = do
  let storeRoot = compileInstalledStoreRoot environment
      activePath = installedLibrariesActivePath storeRoot
  activeExists <- doesFileExist activePath
  if not activeExists
    then pure (Left ("libraries are not installed in " <> storeRoot <> "; run `aihc install PACKAGE` first"))
    else do
      activeContents <- readFile activePath
      case words activeContents of
        [graphHash]
          | length graphHash == 16 && all isHexDigit graphHash -> do
              let artifactRoot = installedLibrariesRoot storeRoot </> graphHash
                  artifactPath = artifactRoot </> if incremental then "interfaces.cache" else "whole.cache"
              cached <- readCache artifactPath
              pure $
                case cached of
                  Nothing -> Left ("installed library artifact is missing or incompatible: " <> artifactPath)
                  Just artifact -> Right (artifactRoot, artifact)
        _ -> pure (Left ("invalid installed library selector: " <> activePath))

attachInstalledBackend :: NativeTarget -> FilePath -> DependencyArtifact -> IO (Either String DependencyArtifact)
attachInstalledBackend target artifactRoot artifact = do
  let backendRoot = installedBackendRoot artifactRoot target
      (initializers, archives) = backendMetadataArtifacts backendRoot (dependencyUnitMetadata artifact)
      finished = artifact {dependencyInitializerSymbols = initializers, dependencyArchivePaths = archives}
  archivesExist <- and <$> mapM doesFileExist archives
  pure $
    if archivesExist
      then Right finished
      else Left ("compiled libraries for target " <> nativeTargetTriple target <> " are not installed")

installedBackendRoot :: FilePath -> NativeTarget -> FilePath
installedBackendRoot artifactRoot target = artifactRoot </> "backends" </> nativeTargetTriple target

writeActiveLibraries :: FilePath -> String -> IO ()
writeActiveLibraries storeRoot graphHash = do
  let destination = installedLibrariesActivePath storeRoot
      directory = takeDirectory destination
  createDirectoryIfMissing True directory
  bracketOnError
    (openTempFile directory "active.tmp")
    (\(path, handle) -> hClose handle >> removeFile path)
    ( \(path, handle) -> do
        hPutStr handle (graphHash <> "\n")
        hClose handle
        renameFile path destination
    )

backendMetadataArtifacts :: FilePath -> [DependencyUnitMetadata] -> ([Text], [FilePath])
backendMetadataArtifacts artifactRoot metadata =
  ( map metadataInitializerSymbol metadata,
    [artifactRoot </> "archives" </> "lib" <> T.unpack library <> ".a" | library <- libraries]
  )
  where
    libraries = sort (Set.toList (Set.fromList (map metadataPrimaryLibrary metadata)))

metadataInitializerSymbol :: DependencyUnitMetadata -> Text
metadataInitializerSymbol = dependencyMetadataInitializer

metadataPrimaryLibrary :: DependencyUnitMetadata -> Text
metadataPrimaryLibrary = fromMaybe "dependencies" . listToMaybe . dependencyMetadataLibraries

loadLibraryPackages :: [LibraryPackage] -> IO (Either String [LoadedModule])
loadLibraryPackages packages = do
  parsed <- mapM loadPackage packages
  pure $ sequence parsed >>= rejectDuplicateModules . concat
  where
    loadPackage package =
      sequence
        <$> mapM
          (parseModuleFile (libraryPackageName package) (libraryPackageId package) (Set.fromList (libraryPackageExposedModules package)))
          (sort (libraryPackageFiles package))

    rejectDuplicateModules loaded = snd <$> foldM insertModule (Map.empty, []) loaded
    insertModule (seen, loaded) modu =
      case moduleName (loadedModule modu) of
        Nothing -> Left "installed library modules must have explicit module names"
        Just name ->
          case Map.lookup name seen of
            Nothing -> Right (Map.insert name (loadedLibrary modu) seen, loaded <> [modu])
            Just previous ->
              Left
                ( "module "
                    <> T.unpack name
                    <> " is provided by both "
                    <> T.unpack previous
                    <> " and "
                    <> T.unpack (loadedLibrary modu)
                )

parseModuleFile :: Text -> [Text] -> Set.Set Text -> FilePath -> IO (Either String LoadedModule)
parseModuleFile library libraryId exposedModules path = do
  source <- Utf8.readFile path
  pure $
    case parseModule (parserConfig path source) source of
      ([], modu) -> Right (LoadedModule library libraryId (maybe False (`Set.member` exposedModules) (moduleName modu)) modu)
      (errors, _) -> Left ("failed to parse library module " <> path <> ": " <> show errors)

parserConfig :: FilePath -> Text -> ParserConfig
parserConfig sourceName source =
  defaultConfig
    { parserSourceName = sourceName,
      parserExtensions = effectiveExtensions language (headerExtensionSettings header)
    }
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)

compileLoadedModules :: [LoadedModule] -> Either String DependencyArtifact
compileLoadedModules loaded = finish <$> foldM compileScc initialState (loadedModuleSccs loaded)
  where
    initialState = CompileState Map.empty mempty [] mempty mempty mempty mempty [] []

    compileScc state members =
      case resolveWithDeps (compileStateExports state) [(loadedResolvePackage member, loadedModule member) | member <- members] of
        ResolveResult {resolveErrors = errors@(_ : _)} -> Left ("library resolve error: " <> show errors)
        resolved@ResolveResult {resolvedModules} ->
          let moduleAsts = map snd resolvedModules
              (checkedModules, tcInterface) =
                typecheckModuleSccWithInterface (compileStateTcInterface state) moduleAsts
           in if not (all tcModuleSuccess checkedModules)
                then Left ("library typecheck error: " <> show (concatMap tcModuleDiagnostics checkedModules))
                else
                  let localBindings = concatMap tcModuleBindings checkedModules
                      bindings = compileStateBindings state <> localBindings
                      desugared = zipWith (desugarModuleWithBindings bindings) checkedModules moduleAsts
                   in if not (all dsSuccess desugared)
                        then Left ("library desugar error: " <> unlines (concatMap dsErrors desugared))
                        else
                          let libraries = sort (Set.toList (Set.fromList (map loadedLibrary members)))
                              libraryIds = sort (Set.toList (Set.fromList (map loadedLibraryId members)))
                              modules = sort (map loadedModuleName members)
                              initializer = unitInitializerSymbol libraryIds modules
                              linkNames =
                                mconcat
                                  [ Grin.linkNamesForProgram
                                      (loadedLibraryId member)
                                      (T.splitOn "." (loadedModuleName member))
                                      (dsProgram desugaredModule)
                                  | (member, desugaredModule) <- zip members desugared
                                  ]
                              sourceCore = FcProgram (concatMap (fcTopBinds . dsProgram) desugared)
                              core = optimizeProgram (lowerPseudoOps (lowerNewtypesWithInterface (compileStateNewtypes state) sourceCore))
                              axioms = extractAxiomInterface core
                              newtypes = extractNewtypeInterface core
                              grinInterface = Grin.extractGrinInterfaceWithLinkNames linkNames core
                              reachabilityInterface = extractReachabilityInterface core
                              grin = Grin.lowerProgramWithInterfaceAndLinkNames linkNames (compileStateGrin state) core
                              linkInterface = extractLinkInterface grin
                           in case Grin.toCpsGrin grin of
                                Left err -> Left ("library CPS-GRIN error: " <> show err)
                                Right cpsGrin ->
                                  let unit =
                                        DependencyUnit
                                          { dependencyUnitLibraries = libraries,
                                            dependencyUnitModules = modules,
                                            dependencyUnitInitializer = initializer,
                                            dependencyUnitProgram = core,
                                            dependencyUnitGrin = grin,
                                            dependencyUnitCpsGrin = cpsGrin,
                                            dependencyUnitAxiomInterface = axioms,
                                            dependencyUnitNewtypeInterface = newtypes,
                                            dependencyUnitGrinInterface = grinInterface,
                                            dependencyUnitReachabilityInterface = reachabilityInterface,
                                            dependencyUnitLinkInterface = linkInterface
                                          }
                                   in Right
                                        CompileState
                                          { compileStateExports = compileStateExports state <> extractInterfaceWithDeps (compileStateExports state) resolved,
                                            compileStateTcInterface = tcInterface,
                                            compileStateBindings = bindings,
                                            compileStateAxioms = compileStateAxioms state <> axioms,
                                            compileStateNewtypes = compileStateNewtypes state <> newtypes,
                                            compileStateGrin = compileStateGrin state <> grinInterface,
                                            compileStateReachability = compileStateReachability state <> reachabilityInterface,
                                            compileStateLinks = compileStateLinks state <> [linkInterface],
                                            compileStateUnits = compileStateUnits state <> [unit]
                                          }

    finish state =
      DependencyArtifact
        { dependencyExports = Map.filterWithKey (\key _ -> moduleKeyName key `Set.member` exposedModules) (compileStateExports state),
          dependencyTcInterface = compileStateTcInterface state,
          dependencyBindings = compileStateBindings state,
          dependencyAxiomInterface = compileStateAxioms state,
          dependencyNewtypeInterface = compileStateNewtypes state,
          dependencyGrinInterface = compileStateGrin state,
          dependencyReachabilityInterface = compileStateReachability state,
          dependencyLinkInterfaces = compileStateLinks state,
          dependencyRuntimePrimitiveNames =
            Set.fromList
              [ Grin.grinVarName primitive
              | unit <- compileStateUnits state,
                (primitive, _) <- Grin.grinPrimitives (dependencyUnitGrin unit)
              ],
          dependencyUnits = compileStateUnits state,
          dependencyUnitMetadata = map dependencyMetadata (compileStateUnits state),
          dependencyInitializerSymbols = [],
          dependencyArchivePaths = []
        }
      where
        exposedModules =
          Set.fromList
            [ name
            | modu <- loaded,
              loadedModuleExposed modu,
              Just name <- [moduleName (loadedModule modu)]
            ]

data CompileState = CompileState
  { compileStateExports :: !ModuleExports,
    compileStateTcInterface :: !TcInterface,
    compileStateBindings :: ![TcBindingResult],
    compileStateAxioms :: !AxiomInterface,
    compileStateNewtypes :: !NewtypeInterface,
    compileStateGrin :: !Grin.GrinInterface,
    compileStateReachability :: !ReachabilityInterface,
    compileStateLinks :: ![LinkInterface],
    compileStateUnits :: ![DependencyUnit]
  }

loadedModuleSccs :: [LoadedModule] -> [[LoadedModule]]
loadedModuleSccs = map flatten . stronglyConnComp . map graphNode
  where
    graphNode loaded =
      ( loaded,
        loadedModuleName loaded,
        map importDeclModule (moduleImports (loadedModule loaded))
      )
    flatten (AcyclicSCC member) = [member]
    flatten (CyclicSCC members) = members

loadedModuleName :: LoadedModule -> Text
loadedModuleName = fromMaybe "Main" . moduleName . loadedModule

loadedResolvePackage :: LoadedModule -> Package
loadedResolvePackage loadedModule' =
  Package
    { packageName = loadedLibrary loadedModule',
      packageId = PackageId (T.intercalate "-" (loadedLibraryId loadedModule'))
    }

buildBackendArtifacts :: NativeTarget -> FilePath -> [DependencyUnit] -> IO (Either String ([Text], [FilePath]))
buildBackendArtifacts target artifactRoot units = do
  let layout = buildLinkLayoutFromInterfaces (map dependencyUnitLinkInterface units)
      objectRoot = artifactRoot </> "objects"
      archiveRoot = artifactRoot </> "archives"
      backendUnits = map (backendUnit objectRoot) units
  objectResults <- mapM (buildObject target layout) backendUnits
  case sequence objectResults of
    Left err -> pure (Left err)
    Right _ -> do
      createDirectoryIfMissing True archiveRoot
      let archiveMembers =
            foldl'
              (\archives unit -> Map.insertWith (flip (<>)) (backendUnitLibrary unit) [backendObjectPath unit] archives)
              Map.empty
              backendUnits
      archives <- mapM (buildArchive archiveRoot) (Map.toAscList archiveMembers)
      pure $ do
        archivePaths <- sequence archives
        Right (map backendInitializerSymbol backendUnits, archivePaths)

data BackendUnit = BackendUnit
  { backendDependencyUnit :: !DependencyUnit,
    backendProgram :: !Grin.GcGrinProgram,
    backendInitializerSymbol :: !Text,
    backendObjectPath :: !FilePath
  }

backendUnit :: FilePath -> DependencyUnit -> BackendUnit
backendUnit objectRoot unit =
  BackendUnit
    { backendDependencyUnit = unit,
      backendProgram = Grin.lowerGc (dependencyUnitCpsGrin unit),
      backendInitializerSymbol = initializer,
      backendObjectPath = objectRoot </> T.unpack library </> T.unpack unitName <> ".o"
    }
  where
    library = dependencyUnitPrimaryLibrary unit
    unitName = T.intercalate "+" (dependencyUnitModules unit)
    initializer = dependencyUnitInitializer unit

unitInitializerSymbol :: [[Text]] -> [Text] -> Text
unitInitializerSymbol libraryIds modules =
  "_aihc_init_" <> renderLinkedFunctionSymbol (T.intercalate "\0" components)
  where
    components =
      case libraryIds of
        [] -> "dependencies" : moduleComponents
        ids -> intercalate ["with"] ids <> moduleComponents
    moduleComponents = concatMap (T.splitOn ".") modules

dependencyMetadata :: DependencyUnit -> DependencyUnitMetadata
dependencyMetadata unit =
  DependencyUnitMetadata
    { dependencyMetadataLibraries = dependencyUnitLibraries unit,
      dependencyMetadataModules = dependencyUnitModules unit,
      dependencyMetadataInitializer = dependencyUnitInitializer unit
    }

backendUnitLibrary :: BackendUnit -> Text
backendUnitLibrary = dependencyUnitPrimaryLibrary . backendDependencyUnit

dependencyUnitPrimaryLibrary :: DependencyUnit -> Text
dependencyUnitPrimaryLibrary unit = fromMaybe "dependencies" (listToMaybe (dependencyUnitLibraries unit))

buildObject :: NativeTarget -> LinkLayout -> BackendUnit -> IO (Either String ())
buildObject target layout unit = do
  let destination = backendObjectPath unit
      directory = takeDirectory destination
  exists <- doesFileExist destination
  if exists
    then pure (Right ())
    else do
      case compileBackendModule target layout (backendInitializerSymbol unit) (backendProgram unit) of
        Left err -> pure (Left ("backend dependency code generation failed for " <> dependencyUnitLabel (backendDependencyUnit unit) <> ": " <> err))
        Right backendSource -> do
          createDirectoryIfMissing True directory
          withTemporaryDirectory directory "module-build" $ \temporary -> do
            let sourcePath = temporary </> "module" <> backendSourceExtension target
                objectPath = temporary </> "module.o"
            TIO.writeFile sourcePath backendSource
            (compiler, arguments) <- objectCompiler target sourcePath objectPath
            (exitCode, _stdout, stderr) <-
              case target of
                Wasm32Wasip3 -> readWasmClangProcessWithExitCode compiler arguments
                _ -> readProcessWithExitCode compiler arguments ""
            case exitCode of
              ExitSuccess -> renameFile objectPath destination >> pure (Right ())
              ExitFailure _ -> pure (Left ("failed to compile dependency unit " <> dependencyUnitLabel (backendDependencyUnit unit) <> ": " <> stderr))

compileBackendModule :: NativeTarget -> LinkLayout -> Text -> Grin.GcGrinProgram -> Either String Text
compileBackendModule target layout initializer program =
  case target of
    AppleArm64 -> either (Left . show) Right (Arm64.compileModule layout initializer program)
    LinuxAmd64 -> either (Left . show) Right (Amd64.compileModule layout initializer program)
    Llvm -> either (Left . show) Right (Llvm.compileModule layout initializer program)
    Wasm32Wasip3 -> either (Left . show) Right (Wasm.compileModule layout initializer program)

backendSourceExtension :: NativeTarget -> String
backendSourceExtension Llvm = ".ll"
backendSourceExtension Wasm32Wasip3 = ".s"
backendSourceExtension _ = ".s"

objectCompiler :: NativeTarget -> FilePath -> FilePath -> IO (FilePath, [String])
objectCompiler target sourcePath objectPath = do
  (compiler, targetArguments) <-
    case target of
      Wasm32Wasip3 -> wasmClangCommand <$> lookupEnv "AIHC_WASM_CLANG"
      _ -> backendCompiler target
  case target of
    Llvm -> pure (compiler, targetArguments <> ["-c", sourcePath, "-o", objectPath])
    AppleArm64 -> pure (compiler, nativeArguments targetArguments)
    LinuxAmd64 -> pure (compiler, nativeArguments targetArguments)
    Wasm32Wasip3 -> pure (compiler, targetArguments <> ["-mtail-call", "-c", sourcePath, "-o", objectPath])
  where
    nativeArguments targetArguments = targetArguments <> ["-c", sourcePath, "-o", objectPath]

dependencyUnitLabel :: DependencyUnit -> String
dependencyUnitLabel = T.unpack . T.intercalate "," . dependencyUnitModules

buildArchive :: FilePath -> (Text, [FilePath]) -> IO (Either String FilePath)
buildArchive archiveRoot (library, objects) = do
  let destination = archiveRoot </> "lib" <> T.unpack library <> ".a"
  exists <- doesFileExist destination
  if exists
    then pure (Right destination)
    else withTemporaryDirectory archiveRoot "archive-build" $ \temporary -> do
      let archivePath = temporary </> "library.a"
      (exitCode, _stdout, stderr) <- readProcessWithExitCode "ar" (["rcs", archivePath] <> objects) ""
      case exitCode of
        ExitSuccess -> renameFile archivePath destination >> pure (Right destination)
        ExitFailure _ -> pure (Left ("failed to archive dependency library " <> T.unpack library <> ": " <> stderr))

withTemporaryDirectory :: FilePath -> String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory parent template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      (path, handle) <- openTempFile parent template
      hClose handle
      removeFile path
      createDirectoryIfMissing True path
      pure path

dependencyGraphHash :: [LibraryPackage] -> IO String
dependencyGraphHash packages = do
  chunks <- concat <$> mapM packageChunks (sortOn libraryPackageName packages)
  pure
    ( stableHash
        ( Text.encodeUtf8 (frameText (T.pack (show cacheSchemaVersion)))
            : chunks
        )
    )
  where
    packageChunks package = do
      files <- concat <$> mapM (fileChunks package) (sort (libraryPackageFiles package))
      pure
        ( [Text.encodeUtf8 (frameText (libraryPackageName package))]
            <> map (Text.encodeUtf8 . frameText) (libraryPackageId package)
            <> [Text.encodeUtf8 (frameText (T.intercalate "," (sort (libraryPackageExposedModules package))))]
            <> files
        )

    fileChunks package path = do
      bytes <- BS.readFile path
      pure
        [ Text.encodeUtf8 (frameText (T.pack (makeRelative (libraryPackageRoot package) path))),
          Text.encodeUtf8 (frameText (T.pack (show (BS.length bytes)))),
          bytes
        ]

frameText :: Text -> Text
frameText value = T.pack (show (T.length value)) <> ":" <> value

stableHash :: [BS.ByteString] -> String
stableHash chunks = padLeft 16 '0' (showHex digest "")
  where
    digest = foldl' hashChunk fnvOffset chunks
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * fnvPrime)

fnvOffset :: Word64
fnvOffset = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

padLeft :: Int -> Char -> String -> String
padLeft width char value = replicate (max 0 (width - length value)) char <> value

readCache :: FilePath -> IO (Maybe DependencyArtifact)
readCache path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      contents <- readFile path
      pure $ do
        stored <- readMaybe contents
        if storedSchemaVersion stored == cacheSchemaVersion
          then Just (fromStoredArtifact stored)
          else Nothing

writeCache :: FilePath -> FilePath -> Bool -> DependencyArtifact -> IO ()
writeCache directory destination includeUnits artifact =
  bracketOnError
    (openTempFile directory "dependency.cache.tmp")
    (\(path, handle) -> hClose handle >> removeFile path)
    ( \(path, handle) -> do
        hPutStr handle (show (toStoredArtifact includeUnits artifact))
        hClose handle
        renameFile path destination
    )

toStoredArtifact :: Bool -> DependencyArtifact -> StoredDependencyArtifact
toStoredArtifact includeUnits artifact =
  StoredDependencyArtifact
    { storedSchemaVersion = cacheSchemaVersion,
      storedExports = toStoredExports (dependencyExports artifact),
      storedTcInterface = dependencyTcInterface artifact,
      storedBindings = dependencyBindings artifact,
      storedAxiomInterface = dependencyAxiomInterface artifact,
      storedNewtypeInterface = dependencyNewtypeInterface artifact,
      storedGrinInterface = dependencyGrinInterface artifact,
      storedReachabilityInterface = dependencyReachabilityInterface artifact,
      storedLinkInterfaces = dependencyLinkInterfaces artifact,
      storedRuntimePrimitiveNames = dependencyRuntimePrimitiveNames artifact,
      storedUnitMetadata = dependencyUnitMetadata artifact,
      storedUnits = if includeUnits then Just (dependencyUnits artifact) else Nothing
    }

fromStoredArtifact :: StoredDependencyArtifact -> DependencyArtifact
fromStoredArtifact stored =
  DependencyArtifact
    { dependencyExports = fromStoredExports (storedExports stored),
      dependencyTcInterface = storedTcInterface stored,
      dependencyBindings = storedBindings stored,
      dependencyAxiomInterface = storedAxiomInterface stored,
      dependencyNewtypeInterface = storedNewtypeInterface stored,
      dependencyGrinInterface = storedGrinInterface stored,
      dependencyReachabilityInterface = storedReachabilityInterface stored,
      dependencyLinkInterfaces = storedLinkInterfaces stored,
      dependencyRuntimePrimitiveNames = storedRuntimePrimitiveNames stored,
      dependencyUnits = fromMaybe [] (storedUnits stored),
      dependencyUnitMetadata = storedUnitMetadata stored,
      dependencyInitializerSymbols = [],
      dependencyArchivePaths = []
    }

toStoredExports :: ModuleExports -> StoredModuleExports
toStoredExports = StoredModuleExports . map toStoredExport . Map.toAscList
  where
    toStoredExport (key, scope) =
      ( packageName (moduleKeyPackage key),
        packageIdText (packageId (moduleKeyPackage key)),
        moduleKeyName key,
        toStoredScope scope
      )

fromStoredExports :: StoredModuleExports -> ModuleExports
fromStoredExports (StoredModuleExports exports) =
  Map.fromList
    [ (ModuleKey (Package visibleName (PackageId identity)) storedModuleName, fromStoredScope scope)
    | (visibleName, identity, storedModuleName, scope) <- exports
    ]

toStoredScope :: Scope -> StoredScope
toStoredScope scope =
  StoredScope
    { storedScopeTerms = map (fmap toStoredResolvedName) (Map.toAscList (scopeTerms scope)),
      storedScopeTypes = map (fmap toStoredResolvedName) (Map.toAscList (scopeTypes scope)),
      storedScopeConstructors = Map.toAscList (scopeConstructors scope),
      storedScopeRecordFields = Map.toAscList (scopeRecordFields scope),
      storedScopeMethods = Map.toAscList (scopeMethods scope),
      storedScopeFixities = Map.toAscList (scopeFixities scope),
      storedScopeQualifiedModules = map (fmap toStoredScope) (Map.toAscList (scopeQualifiedModules scope))
    }

fromStoredScope :: StoredScope -> Scope
fromStoredScope scope =
  Scope
    { scopeTerms = Map.fromList (map (fmap fromStoredResolvedName) (storedScopeTerms scope)),
      scopeTypes = Map.fromList (map (fmap fromStoredResolvedName) (storedScopeTypes scope)),
      scopeConstructors = Map.fromList (storedScopeConstructors scope),
      scopeRecordFields = Map.fromList (storedScopeRecordFields scope),
      scopeMethods = Map.fromList (storedScopeMethods scope),
      scopeFixities = Map.fromList (storedScopeFixities scope),
      scopeQualifiedModules = Map.fromList (map (fmap fromStoredScope) (storedScopeQualifiedModules scope))
    }

toStoredResolvedName :: ResolvedName -> StoredResolvedName
toStoredResolvedName resolved =
  case resolved of
    ResolvedTopLevel (PackageId identity) Name {nameQualifier, nameType, nameText} -> StoredTopLevel identity nameQualifier nameType nameText
    ResolvedLocal unique UnqualifiedName {unqualifiedNameType, unqualifiedNameText} -> StoredLocal unique unqualifiedNameType unqualifiedNameText
    ResolvedBuiltin name -> StoredBuiltin name
    ResolvedError err -> StoredError err

fromStoredResolvedName :: StoredResolvedName -> ResolvedName
fromStoredResolvedName stored =
  case stored of
    StoredTopLevel identity qualifier nameType name -> ResolvedTopLevel (PackageId identity) (Name qualifier nameType name [])
    StoredLocal unique nameType name -> ResolvedLocal unique (UnqualifiedName nameType name [])
    StoredBuiltin name -> ResolvedBuiltin name
    StoredError err -> ResolvedError err
