{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core-library discovery and content-addressed compilation for @aihc compile@.
module Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    buildDependencies,
  )
where

import Aihc.Fc (DesugarResult (..), FcProgram (..), desugarModuleWithBindings)
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
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve
  ( ModuleExports,
    OperatorFixity,
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    extractInterface,
    resolveWithDeps,
  )
import Aihc.Tc
  ( InstanceInfo,
    TcBindingResult (..),
    TcType (..),
    TyVarId,
    TypeScheme (..),
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleSuccess,
    typecheckModulesWithEnv,
  )
import Control.Exception (bracketOnError)
import Control.Monad (filterM, foldM)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    removeFile,
    renameFile,
  )
import System.FilePath
  ( dropExtension,
    makeRelative,
    splitDirectories,
    takeExtension,
    (</>),
  )
import System.IO (hClose, hPutStr, openTempFile)
import Text.Read (readMaybe)

data CompileEnvironment = CompileEnvironment
  { compileCoreLibraryRoot :: !FilePath,
    compileCacheRoot :: !FilePath
  }
  deriving (Eq, Show)

data DependencyArtifact = DependencyArtifact
  { dependencyExports :: !ModuleExports,
    dependencyTerms :: ![(Text, TypeScheme)],
    dependencyBindings :: ![TcBindingResult],
    dependencyInstances :: ![InstanceInfo],
    dependencyProgram :: !FcProgram
  }

data StoredDependencyArtifact = StoredDependencyArtifact
  { storedSchemaVersion :: !Int,
    storedExports :: !StoredModuleExports,
    storedTerms :: ![(Text, TypeScheme)],
    storedBindings :: ![TcBindingResult],
    storedInstances :: ![InstanceInfo],
    storedProgram :: !FcProgram
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
cacheSchemaVersion = 1

buildDependencies :: CompileEnvironment -> Bool -> Module -> IO (Either String DependencyArtifact)
buildDependencies environment usesImplicitPrelude mainModule = do
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
                Just artifact -> pure (Right artifact)
                Nothing ->
                  case compileLoadedModules loaded of
                    Left err -> pure (Left err)
                    Right artifact -> do
                      createDirectoryIfMissing True cacheDirectory
                      writeCache cacheDirectory cachePath artifact
                      pure (Right artifact)

emptyDependencyArtifact :: DependencyArtifact
emptyDependencyArtifact =
  DependencyArtifact
    { dependencyExports = Map.empty,
      dependencyTerms = [],
      dependencyBindings = [],
      dependencyInstances = [],
      dependencyProgram = FcProgram []
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
          moduleName = T.intercalate "." (map T.pack (splitDirectories relative))
       in case Map.lookup moduleName index of
            Nothing -> Right (Map.insert moduleName (ModuleSource library path) index)
            Just previous
              | libraryPriority library < libraryPriority (moduleSourceLibrary previous) ->
                  Right (Map.insert moduleName (ModuleSource library path) index)
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
compileLoadedModules loaded =
  case resolveWithDeps Map.empty modules of
    ResolveResult {resolveErrors = errors@(_ : _)} -> Left ("core library resolve error: " <> show errors)
    resolved@ResolveResult {resolvedModules} ->
      let checkedModules = typecheckModulesWithEnv [] resolvedModules
       in if not (all tcModuleSuccess checkedModules)
            then Left ("core library typecheck error: " <> show (concatMap tcModuleDiagnostics checkedModules))
            else
              let bindings = concatMap tcModuleBindings checkedModules
                  instances = concatMap tcModuleInstances checkedModules
                  desugared = zipWith (desugarModuleWithBindings bindings) checkedModules resolvedModules
               in if not (all dsSuccess desugared)
                    then Left ("core library desugar error: " <> unlines (concatMap dsErrors desugared))
                    else
                      Right
                        DependencyArtifact
                          { dependencyExports = extractInterface resolved,
                            dependencyTerms = map bindingImportedTerm bindings,
                            dependencyBindings = bindings,
                            dependencyInstances = instances,
                            dependencyProgram = concatPrograms (map dsProgram desugared)
                          }
  where
    modules = map loadedModule loaded

bindingImportedTerm :: TcBindingResult -> (Text, TypeScheme)
bindingImportedTerm binding = (tbName binding, tcTypeScheme (tbType binding))

tcTypeScheme :: TcType -> TypeScheme
tcTypeScheme ty =
  case collectForAlls ty of
    (tvs, TcQualTy preds body) -> ForAll tvs preds body
    (tvs, body) -> ForAll tvs [] body

collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

concatPrograms :: [FcProgram] -> FcProgram
concatPrograms programs = FcProgram (concatMap fcTopBinds programs)

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
      storedBindings = dependencyBindings artifact,
      storedInstances = dependencyInstances artifact,
      storedProgram = dependencyProgram artifact
    }

fromStoredArtifact :: StoredDependencyArtifact -> DependencyArtifact
fromStoredArtifact stored =
  DependencyArtifact
    { dependencyExports = fromStoredExports (storedExports stored),
      dependencyTerms = storedTerms stored,
      dependencyBindings = storedBindings stored,
      dependencyInstances = storedInstances stored,
      dependencyProgram = storedProgram stored
    }

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
