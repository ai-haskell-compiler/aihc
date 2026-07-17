{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core-library discovery and content-addressed compilation for @aihc compile@.
-- Each closure cache contains the frontend interfaces and one Core, GRIN, and
-- native object artifact per strongly connected module component.
module Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    DependencyUnit (..),
    buildDependencies,
  )
where

import Aihc.Arm64 (LinkInterface, LinkLayout, buildLinkLayoutFromInterfaces, compileModule, extractLinkInterface, targetTriple)
import Aihc.Fc (DesugarResult (..), FcProgram (..), NewtypeInterface, ReachabilityInterface, desugarModuleWithBindings, extractNewtypeInterface, extractReachabilityInterface, lowerNewtypesWithInterface)
import Aihc.Grin qualified as Grin
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
    OperatorFixity,
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    extractInterfaceWithDeps,
    resolveWithDeps,
  )
import Aihc.Tc
  ( InstanceInfo,
    TcBindingResult (..),
    TyConInfo,
    TypeScheme (..),
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleSuccess,
    typecheckModuleSccWithFullEnv,
  )
import Control.Exception (bracket, bracketOnError)
import Control.Monad (filterM, foldM)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath
  ( dropExtension,
    makeRelative,
    splitDirectories,
    takeDirectory,
    takeExtension,
    (</>),
  )
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data CompileEnvironment = CompileEnvironment
  { compileCoreLibraryRoot :: !FilePath,
    compileCacheRoot :: !FilePath
  }
  deriving (Eq, Show)

data DependencyArtifact = DependencyArtifact
  { dependencyExports :: !ModuleExports,
    dependencyTerms :: ![(Text, TypeScheme)],
    dependencyTyCons :: ![TyConInfo],
    dependencyBindings :: ![TcBindingResult],
    dependencyInstances :: ![InstanceInfo],
    dependencyNewtypeInterface :: !NewtypeInterface,
    dependencyGrinInterface :: !Grin.GrinInterface,
    dependencyReachabilityInterface :: !ReachabilityInterface,
    dependencyLinkInterfaces :: ![LinkInterface],
    dependencyUnits :: ![DependencyUnit],
    dependencyInitializerSymbols :: ![Text],
    dependencyArchivePaths :: ![FilePath]
  }

data DependencyUnit = DependencyUnit
  { dependencyUnitLibraries :: ![Text],
    dependencyUnitModules :: ![Text],
    dependencyUnitProgram :: !FcProgram,
    dependencyUnitGrin :: !Grin.GrinProgram,
    dependencyUnitCpsGrin :: !Grin.CpsGrinProgram,
    dependencyUnitNewtypeInterface :: !NewtypeInterface,
    dependencyUnitGrinInterface :: !Grin.GrinInterface,
    dependencyUnitReachabilityInterface :: !ReachabilityInterface,
    dependencyUnitLinkInterface :: !LinkInterface
  }
  deriving (Eq, Show, Read)

data StoredDependencyArtifact = StoredDependencyArtifact
  { storedSchemaVersion :: !Int,
    storedExports :: !StoredModuleExports,
    storedTerms :: ![(Text, TypeScheme)],
    storedTyCons :: ![TyConInfo],
    storedBindings :: ![TcBindingResult],
    storedInstances :: ![InstanceInfo],
    storedUnits :: ![DependencyUnit]
  }
  deriving (Show, Read)

newtype StoredModuleExports = StoredModuleExports [(Text, StoredScope)]
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
  = StoredTopLevel !(Maybe Text) !NameType !Text
  | StoredLocal !Int !NameType !Text
  | StoredBuiltin !Text
  | StoredError !String
  deriving (Show, Read)

data ModuleSource = ModuleSource
  { moduleSourceLibrary :: !Text,
    moduleSourcePath :: !FilePath
  }

data LoadedModule = LoadedModule
  { loadedLibrary :: !Text,
    loadedModule :: !Module
  }

cacheSchemaVersion :: Int
cacheSchemaVersion = 6

buildDependencies :: CompileEnvironment -> Bool -> Bool -> Module -> IO (Either String DependencyArtifact)
buildDependencies environment usesImplicitPrelude buildNative mainModule = do
  let importedRoots = map importDeclModule (moduleImports mainModule)
      defaultRoots = if usesImplicitPrelude then ["GHC.Prim", "Prelude"] else []
      initialRoots = sort (Set.toList (Set.fromList (defaultRoots <> importedRoots)))
  if null initialRoots
    then pure (Right emptyDependencyArtifact)
    else do
      indexResult <- discoverModules (compileCoreLibraryRoot environment)
      case indexResult of
        Left err -> pure (Left err)
        Right moduleIndex -> do
          let roots = addLibraryRoots moduleIndex initialRoots
          closureResult <- loadModuleClosure moduleIndex roots
          case closureResult of
            Left err -> pure (Left err)
            Right loaded -> do
              graphHash <- dependencyGraphHash (compileCoreLibraryRoot environment) loaded
              let closureHash = stableHash (map (Text.encodeUtf8 . frameText) roots)
                  cacheDirectory = compileCacheRoot environment </> graphHash
                  cachePath = cacheDirectory </> closureHash <> ".cache"
              cached <- readCache cachePath
              case cached of
                Just artifact
                  | buildNative -> finishArtifact cacheDirectory closureHash artifact
                  | otherwise -> pure (Right artifact)
                Nothing ->
                  case compileLoadedModules loaded of
                    Left err -> pure (Left err)
                    Right artifact -> do
                      createDirectoryIfMissing True cacheDirectory
                      writeCache cacheDirectory cachePath artifact
                      if buildNative
                        then finishArtifact cacheDirectory closureHash artifact
                        else pure (Right artifact)

finishArtifact :: FilePath -> FilePath -> DependencyArtifact -> IO (Either String DependencyArtifact)
finishArtifact cacheDirectory closureHash artifact = do
  native <- buildNativeArtifacts (cacheDirectory </> closureHash) (dependencyUnits artifact)
  pure $
    (\(initializers, archives) -> artifact {dependencyInitializerSymbols = initializers, dependencyArchivePaths = archives})
      <$> native

emptyDependencyArtifact :: DependencyArtifact
emptyDependencyArtifact =
  DependencyArtifact
    { dependencyExports = Map.empty,
      dependencyTerms = [],
      dependencyTyCons = [],
      dependencyBindings = [],
      dependencyInstances = [],
      dependencyNewtypeInterface = mempty,
      dependencyGrinInterface = mempty,
      dependencyReachabilityInterface = mempty,
      dependencyLinkInterfaces = [],
      dependencyUnits = [],
      dependencyInitializerSymbols = [],
      dependencyArchivePaths = []
    }

-- aihc-base is defined in terms of the compiler-owned GHC.Prim interface even
-- when a selected base module does not import it directly.
addLibraryRoots :: Map Text ModuleSource -> [Text] -> [Text]
addLibraryRoots moduleIndex roots
  | any isBaseModule roots = sort (Set.toList (Set.insert "GHC.Prim" (Set.fromList roots)))
  | otherwise = roots
  where
    isBaseModule name =
      case Map.lookup name moduleIndex of
        Just ModuleSource {moduleSourceLibrary = "aihc-base"} -> True
        _ -> False

discoverModules :: FilePath -> IO (Either String (Map Text ModuleSource))
discoverModules root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure (Left ("core library directory does not exist: " <> root))
    else do
      entries <- sort <$> listDirectory root
      libraries <- filterM (doesDirectoryExist . (root </>)) entries
      foldM addLibrary (Right Map.empty) libraries
  where
    addLibrary (Left err) _ = pure (Left err)
    addLibrary (Right index) library = do
      let sourceRoot = root </> library </> "src"
      sourceRootExists <- doesDirectoryExist sourceRoot
      if not sourceRootExists
        then pure (Right index)
        else do
          files <- listFilesRecursively sourceRoot
          pure (foldM (insertModule sourceRoot (T.pack library)) index (filter ((== ".hs") . takeExtension) files))

    insertModule sourceRoot library index path =
      let relative = dropExtension (makeRelative sourceRoot path)
          discoveredModuleName = T.intercalate "." (map T.pack (splitDirectories relative))
       in case Map.lookup discoveredModuleName index of
            Nothing -> Right (Map.insert discoveredModuleName (ModuleSource library path) index)
            Just previous
              | libraryPriority library < libraryPriority (moduleSourceLibrary previous) ->
                  Right (Map.insert discoveredModuleName (ModuleSource library path) index)
              | otherwise -> Right index

    libraryPriority "aihc-prim" = 0 :: Int
    libraryPriority "aihc-base" = 1
    libraryPriority "aihc-internal" = 3
    libraryPriority _ = 2

loadModuleClosure :: Map Text ModuleSource -> [Text] -> IO (Either String [LoadedModule])
loadModuleClosure index roots = fmap (fmap snd) (go Set.empty [] roots)
  where
    go seen loaded [] = pure (Right (seen, loaded))
    go seen loaded (name : remaining)
      | name `Set.member` seen = go seen loaded remaining
      | otherwise =
          case Map.lookup name index of
            Nothing -> pure (Left ("core module not found in ./core-libs: " <> T.unpack name))
            Just source -> do
              parsed <- parseModuleFile source
              case parsed of
                Left err -> pure (Left err)
                Right modu -> do
                  let seen' = Set.insert name seen
                      imports = map importDeclModule (moduleImports modu)
                  dependencies <- go seen' loaded imports
                  case dependencies of
                    Left err -> pure (Left err)
                    Right (seen'', loaded') ->
                      go seen'' (loaded' <> [LoadedModule (moduleSourceLibrary source) modu]) remaining

parseModuleFile :: ModuleSource -> IO (Either String Module)
parseModuleFile ModuleSource {moduleSourcePath} = do
  source <- TIO.readFile moduleSourcePath
  pure $
    case parseModule (parserConfig moduleSourcePath source) source of
      ([], modu) -> Right modu
      (errors, _) -> Left ("failed to parse core module " <> moduleSourcePath <> ": " <> show errors)

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
    initialState = CompileState Map.empty [] [] [] [] mempty mempty mempty [] []

    compileScc state members =
      case resolveWithDeps (compileStateExports state) (map loadedModule members) of
        ResolveResult {resolveErrors = errors@(_ : _)} -> Left ("core library resolve error: " <> show errors)
        resolved@ResolveResult {resolvedModules} ->
          let (checkedModules, termSchemes, tyCons) =
                typecheckModuleSccWithFullEnv
                  (compileStateTerms state)
                  (compileStateTyCons state)
                  (compileStateInstances state)
                  resolvedModules
           in if not (all tcModuleSuccess checkedModules)
                then Left ("core library typecheck error: " <> show (concatMap tcModuleDiagnostics checkedModules))
                else
                  let localBindings = concatMap tcModuleBindings checkedModules
                      bindings = compileStateBindings state <> localBindings
                      localInstances = concatMap tcModuleInstances checkedModules
                      desugared = zipWith (desugarModuleWithBindings bindings) checkedModules resolvedModules
                   in if not (all dsSuccess desugared)
                        then Left ("core library desugar error: " <> unlines (concatMap dsErrors desugared))
                        else
                          let sourceCore = FcProgram (concatMap (fcTopBinds . dsProgram) desugared)
                              core = lowerNewtypesWithInterface (compileStateNewtypes state) sourceCore
                              newtypes = extractNewtypeInterface core
                              grinInterface = Grin.extractGrinInterface core
                              reachabilityInterface = extractReachabilityInterface core
                              grin = Grin.lowerProgramWithInterface (compileStateGrin state) core
                              linkInterface = extractLinkInterface grin
                           in case Grin.toCpsGrin grin of
                                Left err -> Left ("core library CPS-GRIN error: " <> show err)
                                Right cpsGrin ->
                                  let unit =
                                        DependencyUnit
                                          { dependencyUnitLibraries = sort (Set.toList (Set.fromList (map loadedLibrary members))),
                                            dependencyUnitModules = sort (map loadedModuleName members),
                                            dependencyUnitProgram = core,
                                            dependencyUnitGrin = grin,
                                            dependencyUnitCpsGrin = cpsGrin,
                                            dependencyUnitNewtypeInterface = newtypes,
                                            dependencyUnitGrinInterface = grinInterface,
                                            dependencyUnitReachabilityInterface = reachabilityInterface,
                                            dependencyUnitLinkInterface = linkInterface
                                          }
                                   in Right
                                        CompileState
                                          { compileStateExports = compileStateExports state <> extractInterfaceWithDeps (compileStateExports state) resolved,
                                            compileStateTerms = termSchemes,
                                            compileStateTyCons = tyCons,
                                            compileStateBindings = bindings,
                                            compileStateInstances = compileStateInstances state <> localInstances,
                                            compileStateNewtypes = compileStateNewtypes state <> newtypes,
                                            compileStateGrin = compileStateGrin state <> grinInterface,
                                            compileStateReachability = compileStateReachability state <> reachabilityInterface,
                                            compileStateLinks = compileStateLinks state <> [linkInterface],
                                            compileStateUnits = compileStateUnits state <> [unit]
                                          }

    finish state =
      DependencyArtifact
        { dependencyExports = compileStateExports state,
          dependencyTerms = compileStateTerms state,
          dependencyTyCons = compileStateTyCons state,
          dependencyBindings = compileStateBindings state,
          dependencyInstances = compileStateInstances state,
          dependencyNewtypeInterface = compileStateNewtypes state,
          dependencyGrinInterface = compileStateGrin state,
          dependencyReachabilityInterface = compileStateReachability state,
          dependencyLinkInterfaces = compileStateLinks state,
          dependencyUnits = compileStateUnits state,
          dependencyInitializerSymbols = [],
          dependencyArchivePaths = []
        }

data CompileState = CompileState
  { compileStateExports :: !ModuleExports,
    compileStateTerms :: ![(Text, TypeScheme)],
    compileStateTyCons :: ![TyConInfo],
    compileStateBindings :: ![TcBindingResult],
    compileStateInstances :: ![InstanceInfo],
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

buildNativeArtifacts :: FilePath -> [DependencyUnit] -> IO (Either String ([Text], [FilePath]))
buildNativeArtifacts artifactRoot units = do
  let layout = buildLinkLayoutFromInterfaces (map dependencyUnitLinkInterface units)
      objectRoot = artifactRoot </> "objects"
      archiveRoot = artifactRoot </> "archives"
      nativeUnits = map (nativeUnit objectRoot) units
  objectResults <- mapM (buildObject layout) nativeUnits
  case sequence objectResults of
    Left err -> pure (Left err)
    Right _ -> do
      createDirectoryIfMissing True archiveRoot
      let archiveMembers =
            foldl'
              (\archives unit -> Map.insertWith (flip (<>)) (nativeUnitLibrary unit) [nativeObjectPath unit] archives)
              Map.empty
              nativeUnits
      archives <- mapM (buildArchive archiveRoot) (Map.toAscList archiveMembers)
      pure $ do
        archivePaths <- sequence archives
        Right (map nativeInitializerSymbol nativeUnits, archivePaths)

data NativeUnit = NativeUnit
  { nativeDependencyUnit :: !DependencyUnit,
    nativeProgram :: !Grin.CpsGrinProgram,
    nativeInitializerSymbol :: !Text,
    nativeObjectPath :: !FilePath
  }

nativeUnit :: FilePath -> DependencyUnit -> NativeUnit
nativeUnit objectRoot unit =
  NativeUnit
    { nativeDependencyUnit = unit,
      nativeProgram = dependencyUnitCpsGrin unit,
      nativeInitializerSymbol = initializer,
      nativeObjectPath = objectRoot </> T.unpack library </> T.unpack unitName <> ".o"
    }
  where
    library = dependencyUnitPrimaryLibrary unit
    unitName = T.intercalate "+" (dependencyUnitModules unit)
    initializer = "_aihc_init_" <> symbolHex (library <> "\0" <> T.intercalate "\0" (dependencyUnitModules unit))

nativeUnitLibrary :: NativeUnit -> Text
nativeUnitLibrary = dependencyUnitPrimaryLibrary . nativeDependencyUnit

dependencyUnitPrimaryLibrary :: DependencyUnit -> Text
dependencyUnitPrimaryLibrary unit = fromMaybe "dependencies" (listToMaybe (dependencyUnitLibraries unit))

symbolHex :: Text -> Text
symbolHex = T.concat . map renderByte . BS.unpack . Text.encodeUtf8
  where
    renderByte byte = T.pack (padLeft 2 '0' (showHex byte ""))

buildObject :: LinkLayout -> NativeUnit -> IO (Either String ())
buildObject layout unit = do
  let destination = nativeObjectPath unit
      directory = takeDirectory destination
  exists <- doesFileExist destination
  if exists
    then pure (Right ())
    else do
      case compileModule layout (nativeInitializerSymbol unit) (nativeProgram unit) of
        Left err -> pure (Left ("ARM64 dependency code generation failed for " <> dependencyUnitLabel (nativeDependencyUnit unit) <> ": " <> show err))
        Right assembly -> do
          createDirectoryIfMissing True directory
          withTemporaryDirectory directory "module-build" $ \temporary -> do
            let assemblyPath = temporary </> "module.s"
                objectPath = temporary </> "module.o"
            TIO.writeFile assemblyPath assembly
            (exitCode, _stdout, stderr) <- readProcessWithExitCode "clang" ["--target=" <> targetTriple, "-c", assemblyPath, "-o", objectPath] ""
            case exitCode of
              ExitSuccess -> renameFile objectPath destination >> pure (Right ())
              ExitFailure _ -> pure (Left ("failed to assemble dependency unit " <> dependencyUnitLabel (nativeDependencyUnit unit) <> ": " <> stderr))

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

dependencyGraphHash :: FilePath -> [LoadedModule] -> IO String
dependencyGraphHash root loaded = do
  let libraries = sort (Set.toList (Set.fromList (map (T.unpack . loadedLibrary) loaded)))
  chunks <- concat <$> mapM libraryChunks libraries
  pure (stableHash (Text.encodeUtf8 (frameText (T.pack (show cacheSchemaVersion))) : chunks))
  where
    libraryChunks library = do
      let libraryRoot = root </> library
      exists <- doesDirectoryExist libraryRoot
      if not exists
        then pure [Text.encodeUtf8 (frameText (T.pack library))]
        else do
          files <- sort <$> listFilesRecursively libraryRoot
          fmap concat . mapM (fileChunks libraryRoot) $ files

    fileChunks libraryRoot path = do
      bytes <- BS.readFile path
      pure
        [ Text.encodeUtf8 (frameText (T.pack (makeRelative root libraryRoot))),
          Text.encodeUtf8 (frameText (T.pack (makeRelative libraryRoot path))),
          Text.encodeUtf8 (frameText (T.pack (show (BS.length bytes)))),
          bytes
        ]

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively root = do
  entries <- sort <$> listDirectory root
  fmap concat . mapM visit $ entries
  where
    visit name = do
      let path = root </> name
      directory <- doesDirectoryExist path
      if directory
        then listFilesRecursively path
        else do
          file <- doesFileExist path
          pure [path | file]

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

writeCache :: FilePath -> FilePath -> DependencyArtifact -> IO ()
writeCache directory destination artifact =
  bracketOnError
    (openTempFile directory "dependency.cache.tmp")
    (\(path, handle) -> hClose handle >> removeFile path)
    ( \(path, handle) -> do
        hPutStr handle (show (toStoredArtifact artifact))
        hClose handle
        renameFile path destination
    )

toStoredArtifact :: DependencyArtifact -> StoredDependencyArtifact
toStoredArtifact artifact =
  StoredDependencyArtifact
    { storedSchemaVersion = cacheSchemaVersion,
      storedExports = toStoredExports (dependencyExports artifact),
      storedTerms = dependencyTerms artifact,
      storedTyCons = dependencyTyCons artifact,
      storedBindings = dependencyBindings artifact,
      storedInstances = dependencyInstances artifact,
      storedUnits = dependencyUnits artifact
    }

fromStoredArtifact :: StoredDependencyArtifact -> DependencyArtifact
fromStoredArtifact stored =
  DependencyArtifact
    { dependencyExports = fromStoredExports (storedExports stored),
      dependencyTerms = storedTerms stored,
      dependencyTyCons = storedTyCons stored,
      dependencyBindings = storedBindings stored,
      dependencyInstances = storedInstances stored,
      dependencyNewtypeInterface = foldMap dependencyUnitNewtypeInterface units,
      dependencyGrinInterface = foldMap dependencyUnitGrinInterface units,
      dependencyReachabilityInterface = foldMap dependencyUnitReachabilityInterface units,
      dependencyLinkInterfaces = map dependencyUnitLinkInterface units,
      dependencyUnits = storedUnits stored,
      dependencyInitializerSymbols = [],
      dependencyArchivePaths = []
    }
  where
    units = storedUnits stored

toStoredExports :: ModuleExports -> StoredModuleExports
toStoredExports = StoredModuleExports . map (fmap toStoredScope) . Map.toAscList

fromStoredExports :: StoredModuleExports -> ModuleExports
fromStoredExports (StoredModuleExports exports) = Map.fromList (map (fmap fromStoredScope) exports)

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
    ResolvedTopLevel Name {nameQualifier, nameType, nameText} -> StoredTopLevel nameQualifier nameType nameText
    ResolvedLocal unique UnqualifiedName {unqualifiedNameType, unqualifiedNameText} -> StoredLocal unique unqualifiedNameType unqualifiedNameText
    ResolvedBuiltin name -> StoredBuiltin name
    ResolvedError err -> StoredError err

fromStoredResolvedName :: StoredResolvedName -> ResolvedName
fromStoredResolvedName stored =
  case stored of
    StoredTopLevel qualifier nameType name -> ResolvedTopLevel (Name qualifier nameType name [])
    StoredLocal unique nameType name -> ResolvedLocal unique (UnqualifiedName nameType name [])
    StoredBuiltin name -> ResolvedBuiltin name
    StoredError err -> ResolvedError err
