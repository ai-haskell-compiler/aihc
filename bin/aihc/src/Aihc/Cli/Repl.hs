{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.Repl
  ( ReplError (..),
    ReplSettings (..),
    ReplSession (..),
    ReplStep (..),
    defaultReplSettings,
    evaluateExpression,
    handleReplInput,
    loadReplSession,
    runRepl,
  )
where

import Aihc.Fc (DesugarResult (..), FcProgram (..), desugarModuleWithBindings, evalProgramBinding, renderProgram, renderValue)
import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr, parseModule)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax
  ( Decl (..),
    Expr,
    ImportDecl (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    ModuleHead (..),
    NameType (..),
    Rhs (..),
    ValueDecl (..),
    mkUnqualifiedName,
    qualifyName,
    unqualifiedNameFromText,
  )
import Aihc.Resolve (ModuleExports, ResolveError (..), ResolveResult (..), ResolvedName (..), Scope (..), extractInterface, resolveWithDeps)
import Aihc.Tc
  ( Pred (..),
    TcBindingResult (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    renderTcType,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleWithEnv,
    typecheckModulesWithEnv,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import System.Console.Haskeline qualified as Haskeline
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data ReplSession = ReplSession
  { replModuleExports :: !ModuleExports,
    replImportedTerms :: ![(Text, TypeScheme)],
    replDependencyProgram :: !FcProgram,
    replSettings :: !(IORef ReplSettings)
  }

data ReplSettings = ReplSettings
  { replShowParsedPretty :: !Bool,
    replShowParsedShorthand :: !Bool,
    replShowInferredType :: !Bool,
    replShowSystemFc :: !Bool
  }
  deriving (Eq, Show)

defaultReplSettings :: ReplSettings
defaultReplSettings =
  ReplSettings
    { replShowParsedPretty = False,
      replShowParsedShorthand = False,
      replShowInferredType = False,
      replShowSystemFc = False
    }

data ReplError
  = ReplMissingBase FilePath
  | ReplInvalidInterface FilePath String
  | ReplParseError
  | ReplResolveError [ResolveError]
  | ReplTypeError [String]
  | ReplDesugarError [String]
  | ReplEvalError String
  deriving (Eq, Show)

data ReplStep
  = ReplContinue !(Maybe String)
  | ReplExit !(Maybe String)
  deriving (Eq, Show)

runRepl :: Maybe FilePath -> IO ()
runRepl maybeStoreRoot = do
  session <- loadReplSession maybeStoreRoot
  Haskeline.runInputT Haskeline.defaultSettings (loop session)
  where
    loop session = do
      minput <- Haskeline.getInputLine "aihc> "
      case minput of
        Nothing -> pure ()
        Just input -> do
          step <- liftIO (handleReplInput session input)
          case step of
            ReplContinue output -> do
              mapM_ Haskeline.outputStrLn output
              loop session
            ReplExit output ->
              mapM_ Haskeline.outputStrLn output

loadReplSession :: Maybe FilePath -> IO ReplSession
loadReplSession maybeStoreRoot = do
  baseContext <- loadAihcBaseContext
  installedInterface <- loadExplicitStoreInterface maybeStoreRoot
  settingsRef <- newIORef defaultReplSettings
  let installedExports = maybe Map.empty interfaceExports installedInterface
      installedTerms = maybe [] interfaceImportedTerms installedInterface
  pure
    ReplSession
      { replModuleExports = replBaseExports baseContext `Map.union` ensurePreludeMvpScope installedExports,
        replImportedTerms = mergeImportedTerms (replBaseImportedTerms baseContext) (ensurePreludeMvpTerms installedTerms),
        replDependencyProgram = replBaseProgram baseContext,
        replSettings = settingsRef
      }

handleReplInput :: ReplSession -> String -> IO ReplStep
handleReplInput session input =
  let trimmedInput = trim input
   in case trimmedInput of
        "" -> pure (ReplContinue Nothing)
        ":quit" -> pure (ReplExit Nothing)
        ":q" -> pure (ReplExit Nothing)
        ":help" -> pure (ReplContinue (Just helpText))
        ":?" -> pure (ReplContinue (Just helpText))
        ":set" -> ReplContinue . Just . renderReplSettings <$> readIORef (replSettings session)
        _ | Just command <- stripPrefix ":set " trimmedInput -> handleSetCommand session command
        command@(':' : _) -> pure (ReplContinue (Just ("unknown command: " <> command)))
        _ -> do
          result <- evaluateExpression session (T.pack trimmedInput)
          pure (ReplContinue (Just (either renderReplError T.unpack result)))

evaluateExpression :: ReplSession -> Text -> IO (Either ReplError Text)
evaluateExpression session input = do
  settings <- readIORef (replSettings session)
  pure $ do
    expr <-
      case parseExpr defaultConfig {parserSourceName = "<repl>"} input of
        ParseOk parsed -> Right parsed
        ParseErr _ -> Left ReplParseError
    let parsedModule = replModule expr
        resolved = resolveWithDeps (replModuleExports session) [parsedModule]
    case resolveErrors resolved of
      [] -> pure ()
      errors -> Left (ReplResolveError errors)
    resolvedModule <-
      case resolvedModules resolved of
        [modu] -> Right modu
        _ -> Left (ReplResolveError [ResolveNotImplemented "REPL resolver returned no module"])
    let tcResult = typecheckModuleWithEnv (replImportedTerms session) resolvedModule
    if tcModuleSuccess tcResult
      then pure ()
      else Left (ReplTypeError (map show (tcModuleDiagnostics tcResult)))
    inferredType <-
      case find ((== replBindingName) . tbName) (tcModuleBindings tcResult) of
        Just binding -> Right (tbType binding)
        Nothing -> Left (ReplTypeError ["missing inferred type for " <> T.unpack replBindingName])
    let allBindings = importedTermBindings (replImportedTerms session) <> tcModuleBindings tcResult
        dsResult = desugarModuleWithBindings allBindings tcResult resolvedModule
    if dsSuccess dsResult
      then pure ()
      else Left (ReplDesugarError (dsErrors dsResult))
    value <- mapLeft (ReplEvalError . show) (evalProgramBinding replBindingName (concatPrograms [replDependencyProgram session, dsProgram dsResult]))
    renderedValue <- mapLeft (ReplEvalError . show) (renderValue value)
    Right (renderEvaluation settings expr inferredType dsResult renderedValue)

handleSetCommand :: ReplSession -> String -> IO ReplStep
handleSetCommand session rawCommand =
  case words rawCommand of
    [command] ->
      case parseSettingCommand command of
        Just updateSettings -> do
          let settingsRef = replSettings session
          settings <- readIORef settingsRef
          let settings' = updateSettings settings
          writeIORef settingsRef settings'
          pure (ReplContinue (Just (renderReplSettings settings')))
        Nothing ->
          pure (ReplContinue (Just ("unknown setting: " <> command)))
    _ ->
      pure (ReplContinue (Just "usage: :set [+|-]parsed-pretty|parsed-shorthand|type|system-fc"))

parseSettingCommand :: String -> Maybe (ReplSettings -> ReplSettings)
parseSettingCommand command =
  case command of
    "+parsed-pretty" -> Just (\settings -> settings {replShowParsedPretty = True})
    "-parsed-pretty" -> Just (\settings -> settings {replShowParsedPretty = False})
    "+pretty" -> Just (\settings -> settings {replShowParsedPretty = True})
    "-pretty" -> Just (\settings -> settings {replShowParsedPretty = False})
    "+parsed-shorthand" -> Just (\settings -> settings {replShowParsedShorthand = True})
    "-parsed-shorthand" -> Just (\settings -> settings {replShowParsedShorthand = False})
    "+shorthand" -> Just (\settings -> settings {replShowParsedShorthand = True})
    "-shorthand" -> Just (\settings -> settings {replShowParsedShorthand = False})
    "+type" -> Just (\settings -> settings {replShowInferredType = True})
    "-type" -> Just (\settings -> settings {replShowInferredType = False})
    "+system-fc" -> Just (\settings -> settings {replShowSystemFc = True})
    "-system-fc" -> Just (\settings -> settings {replShowSystemFc = False})
    "+fc" -> Just (\settings -> settings {replShowSystemFc = True})
    "-fc" -> Just (\settings -> settings {replShowSystemFc = False})
    _ -> Nothing

renderReplSettings :: ReplSettings -> String
renderReplSettings settings =
  unwords
    [ "settings:",
      flag replShowParsedPretty "parsed-pretty",
      flag replShowParsedShorthand "parsed-shorthand",
      flag replShowInferredType "type",
      flag replShowSystemFc "system-fc"
    ]
  where
    flag selector name =
      (if selector settings then "+" else "-") <> name

renderEvaluation :: ReplSettings -> Expr -> TcType -> DesugarResult -> Text -> Text
renderEvaluation settings expr inferredType dsResult renderedValue =
  T.intercalate "\n" (debugSections <> [renderedValue])
  where
    debugSections =
      concat
        [ section "parsed" (T.pack (renderPrettyExpr expr)) (replShowParsedPretty settings),
          section "shorthand" (T.pack (show (shorthand expr))) (replShowParsedShorthand settings),
          section "type" (T.pack (renderTcType inferredType)) (replShowInferredType settings),
          section "system-fc" (T.pack (renderProgram (dsProgram dsResult))) (replShowSystemFc settings)
        ]
    section label body enabled =
      [label <> ":\n" <> body | enabled]

renderPrettyExpr :: Expr -> String
renderPrettyExpr =
  renderString . layoutPretty defaultLayoutOptions . pretty

replModule :: Expr -> Module
replModule expr =
  Module
    { moduleAnns = [],
      moduleHead =
        Just
          ModuleHead
            { moduleHeadAnns = [],
              moduleHeadName = "Aihc.Repl",
              moduleHeadWarningPragma = Nothing,
              moduleHeadExports = Nothing
            },
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls =
        [ DeclValue $
            FunctionBind
              (mkUnqualifiedName NameVarId replBindingName)
              [ Match
                  { matchAnns = [],
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [],
                    matchRhs = UnguardedRhs [] expr Nothing
                  }
              ]
        ]
    }

replBindingName :: Text
replBindingName = "__aihc_repl_it"

renderReplError :: ReplError -> String
renderReplError err =
  case err of
    ReplMissingBase storeRoot ->
      "repl error: installed base package not found in " <> storeRoot <> "; run `aihc install base` first"
    ReplInvalidInterface path message ->
      "repl error: could not load " <> path <> ": " <> message
    ReplParseError ->
      "parse error"
    ReplResolveError errors ->
      "resolve error: " <> unwords (map show errors)
    ReplTypeError errors ->
      "type error: " <> unwords errors
    ReplDesugarError errors ->
      "desugar error: " <> unwords errors
    ReplEvalError message ->
      "eval error: " <> message

helpText :: String
helpText =
  unlines
    [ "Commands:",
      "  :quit, :q  Exit the REPL",
      "  :help, :?  Show this help"
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

data Interface = Interface
  { interfaceExports :: ModuleExports,
    interfaceImportedTerms :: [(Text, TypeScheme)]
  }

data ReplBaseContext = ReplBaseContext
  { replBaseExports :: !ModuleExports,
    replBaseImportedTerms :: ![(Text, TypeScheme)],
    replBaseProgram :: !FcProgram
  }

loadAihcBaseContext :: IO ReplBaseContext
loadAihcBaseContext = do
  root <- defaultAihcBaseRoot
  modulesResult <- loadTransitiveModules [("aihc-base", root)] (Set.singleton "Prelude")
  case modulesResult of
    Left err -> ioError (userError ("repl error: could not load bundled aihc-base Prelude: " <> err))
    Right modules -> buildBaseContext modules

buildBaseContext :: [Module] -> IO ReplBaseContext
buildBaseContext modules =
  case resolveWithDeps Map.empty modules of
    resolved@ResolveResult {resolveErrors = [], resolvedModules} -> do
      let tcResults = typecheckModulesWithEnv [] resolvedModules
      if all tcModuleSuccess tcResults
        then do
          let allBindings = concatMap tcModuleBindings tcResults
              dsResults = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
          if all dsSuccess dsResults
            then
              pure
                ReplBaseContext
                  { replBaseExports = extractInterface resolved,
                    replBaseImportedTerms = map bindingImportedTerm allBindings,
                    replBaseProgram = concatPrograms (map dsProgram dsResults)
                  }
            else ioError (userError ("repl error: could not desugar bundled aihc-base Prelude: " <> unwords (concatMap dsErrors dsResults)))
        else ioError (userError ("repl error: could not type-check bundled aihc-base Prelude: " <> unwords (concatMap (map show . tcModuleDiagnostics) tcResults)))
    ResolveResult {resolveErrors} ->
      ioError (userError ("repl error: could not resolve bundled aihc-base Prelude: " <> show resolveErrors))

bindingImportedTerm :: TcBindingResult -> (Text, TypeScheme)
bindingImportedTerm binding =
  (tbName binding, tcTypeScheme (tbType binding))

mergeImportedTerms :: [(Text, TypeScheme)] -> [(Text, TypeScheme)] -> [(Text, TypeScheme)]
mergeImportedTerms preferred fallback =
  Map.toList (Map.fromList fallback <> Map.fromList preferred)

importedTermBindings :: [(Text, TypeScheme)] -> [TcBindingResult]
importedTermBindings terms =
  [ TcBindingResult
      { tbName = name,
        tbDisplayName = name,
        tbType = instantiateSchemeBody scheme
      }
  | (name, scheme) <- terms
  ]

instantiateSchemeBody :: TypeScheme -> TcType
instantiateSchemeBody (ForAll tvs preds body) =
  foldr TcForAllTy (qualify preds body) tvs
  where
    qualify [] ty = ty
    qualify constraints ty = TcQualTy constraints ty

concatPrograms :: [FcProgram] -> FcProgram
concatPrograms programs =
  FcProgram (concatMap fcTopBinds programs)

loadExplicitStoreInterface :: Maybe FilePath -> IO (Maybe Interface)
loadExplicitStoreInterface Nothing = pure Nothing
loadExplicitStoreInterface (Just storeRoot) = do
  interfacePath <- findInstalledBaseInterface storeRoot
  Just <$> loadInterface interfacePath

defaultAihcBaseRoot :: IO FilePath
defaultAihcBaseRoot = do
  baseOverride <- lookupEnv "AIHC_BASE_SRC"
  case baseOverride of
    Just root -> pure root
    Nothing -> do
      coreLibsOverride <- lookupEnv "AIHC_CORE_LIBS_ROOT"
      case coreLibsOverride of
        Just root -> pure (root </> "core-libs" </> "aihc-base")
        Nothing -> do
          cwd <- getCurrentDirectory
          findUp cwd
  where
    findUp dir = do
      let candidate = dir </> "core-libs" </> "aihc-base"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = FilePath.takeDirectory dir
          if parent == dir
            then pure candidate
            else findUp parent

loadTransitiveModules :: [(Text, FilePath)] -> Set.Set Text -> IO (Either String [Module])
loadTransitiveModules packageRoots initialModules =
  go Set.empty [] (Set.toAscList initialModules)
  where
    go _ loaded [] =
      pure (Right loaded)
    go seen loaded (moduleName : pending)
      | moduleName `Set.member` seen =
          go seen loaded pending
      | otherwise = do
          maybePath <- findModulePathInDependencies packageRoots moduleName
          case maybePath of
            Nothing -> do
              let dependencyNames = T.intercalate ", " (map fst packageRoots)
              pure (Left ("dependency module " <> T.unpack moduleName <> " not found in dependencies: " <> T.unpack dependencyNames))
            Just path -> do
              source <- TIO.readFile path
              case parseOneModule path source of
                Left errMsg -> pure (Left ("dependency module " <> T.unpack moduleName <> " parse error: " <> errMsg))
                Right modu -> do
                  let seen' = Set.insert moduleName seen
                      newImports = Set.toAscList (importedModuleNames [modu] `Set.difference` seen')
                  go seen' (modu : loaded) (pending <> newImports)

findModulePathInDependencies :: [(Text, FilePath)] -> Text -> IO (Maybe FilePath)
findModulePathInDependencies [] _ = pure Nothing
findModulePathInDependencies ((_dependency, root) : rest) moduleName = do
  let path = root </> "src" </> moduleNamePath moduleName
  exists <- doesFileExist path
  if exists
    then pure (Just path)
    else findModulePathInDependencies rest moduleName

moduleNamePath :: Text -> FilePath
moduleNamePath moduleName =
  FilePath.joinPath (map T.unpack (T.splitOn "." moduleName)) <> ".hs"

parseOneModule :: FilePath -> Text -> Either String Module
parseOneModule sourceName input =
  let cfg =
        defaultConfig
          { parserSourceName = sourceName
          }
      (errs, ast) = parseModule cfg input
   in if null errs
        then Right ast
        else Left ("parse module error: " <> show errs)

importedModuleNames :: [Module] -> Set.Set Text
importedModuleNames modules =
  Set.fromList [importDeclModule importDecl | modu <- modules, importDecl <- moduleImports modu]

data InterfaceModule = InterfaceModule
  { interfaceModuleName :: !Text,
    interfaceModuleTerms :: ![Text],
    interfaceModuleTypes :: ![Text],
    interfaceModuleConstructors :: !(Map.Map Text [Text]),
    interfaceModuleRecordFields :: !(Map.Map Text [Text]),
    interfaceModuleMethods :: !(Map.Map Text [Text])
  }
  deriving (Show)

instance Aeson.FromJSON InterfaceModule where
  parseJSON =
    Aeson.withObject "interface module" $ \obj ->
      InterfaceModule
        <$> obj .: "module"
        <*> obj .: "terms"
        <*> obj .: "types"
        <*> obj .: "constructors"
        <*> obj .: "recordFields"
        <*> obj .: "methods"

loadInterface :: FilePath -> IO Interface
loadInterface path = do
  bytes <- BL.readFile path
  case Aeson.eitherDecode bytes of
    Left err -> ioError (userError (renderReplError (ReplInvalidInterface path err)))
    Right value ->
      case AesonTypes.parseEither parseInterface value of
        Left err -> ioError (userError (renderReplError (ReplInvalidInterface path err)))
        Right interface -> pure interface

parseInterface :: Aeson.Value -> AesonTypes.Parser Interface
parseInterface =
  Aeson.withObject "package interface" $ \obj -> do
    modules <- obj .: "modules"
    tcModules <- obj .:? "typecheck" .!= []
    pure
      Interface
        { interfaceExports = Map.fromList [(interfaceModuleName modu, interfaceModuleScope modu) | modu <- modules],
          interfaceImportedTerms = concatMap interfaceTcModuleTerms tcModules
        }

newtype InterfaceTcModule = InterfaceTcModule
  { interfaceTcModuleBindings :: [InterfaceTcBinding]
  }
  deriving (Show)

data InterfaceTcBinding = InterfaceTcBinding
  { interfaceTcBindingName :: !Text,
    interfaceTcBindingType :: !(Maybe TcType)
  }
  deriving (Show)

instance Aeson.FromJSON InterfaceTcModule where
  parseJSON =
    Aeson.withObject "typecheck module" $ \obj ->
      InterfaceTcModule <$> obj .: "bindings"

instance Aeson.FromJSON InterfaceTcBinding where
  parseJSON =
    Aeson.withObject "typecheck binding" $ \obj ->
      InterfaceTcBinding
        <$> obj .: "name"
        <*> (obj .:? "typeJson" >>= traverse parseTcTypeJson)

interfaceTcModuleTerms :: InterfaceTcModule -> [(Text, TypeScheme)]
interfaceTcModuleTerms modu =
  [ (normalizeImportedBindingName name, tcTypeScheme ty)
  | InterfaceTcBinding name (Just ty) <- interfaceTcModuleBindings modu
  ]

interfaceModuleScope :: InterfaceModule -> Scope
interfaceModuleScope modu =
  Scope
    { scopeTerms =
        Map.fromList
          [ (name, resolvedTopLevel (interfaceModuleName modu) name)
          | name <- interfaceModuleTerms modu
          ],
      scopeTypes =
        Map.fromList
          [ (name, resolvedTopLevel (interfaceModuleName modu) name)
          | name <- interfaceModuleTypes modu
          ],
      scopeConstructors = interfaceModuleConstructors modu,
      scopeRecordFields = interfaceModuleRecordFields modu,
      scopeMethods = interfaceModuleMethods modu,
      scopeFixities = Map.empty,
      scopeQualifiedModules = Map.empty
    }

resolvedTopLevel :: Text -> Text -> ResolvedName
resolvedTopLevel moduleName name =
  ResolvedTopLevel (qualifyName (Just moduleName) (unqualifiedNameFromText name))

findInstalledBaseInterface :: FilePath -> IO FilePath
findInstalledBaseInterface storeRoot = do
  storeExists <- doesDirectoryExist storeRoot
  if not storeExists
    then ioError (userError (renderReplError (ReplMissingBase storeRoot)))
    else do
      entries <- listDirectory storeRoot
      candidates <- mapM (readCandidate . (storeRoot </>)) entries
      case find isBaseManifest candidates of
        Just candidate -> pure (candidateInterfacePath candidate)
        Nothing -> ioError (userError (renderReplError (ReplMissingBase storeRoot)))
  where
    readCandidate path = do
      let manifestPath = path </> "manifest.json"
      exists <- doesFileExist manifestPath
      if exists
        then do
          bytes <- BL.readFile manifestPath
          pure $
            case Aeson.eitherDecode bytes of
              Right candidate -> candidate
              Left _ -> StoreCandidate "" "" (path </> "interfaces" </> "package-interface.json")
        else pure (StoreCandidate "" "" (path </> "interfaces" </> "package-interface.json"))

data StoreCandidate = StoreCandidate
  { candidatePackageName :: !String,
    candidatePackageVersion :: !String,
    candidateInterfacePath :: !FilePath
  }
  deriving (Show)

instance Aeson.FromJSON StoreCandidate where
  parseJSON =
    Aeson.withObject "manifest" $ \obj -> do
      pkg <- obj .: "package"
      StoreCandidate
        <$> pkg .: "name"
        <*> pkg .: "version"
        <*> obj .: "interfacePath"

isBaseManifest :: StoreCandidate -> Bool
isBaseManifest candidate =
  candidatePackageName candidate `elem` ["aihc-base", "base"]

ensurePreludeMvpScope :: ModuleExports -> ModuleExports
ensurePreludeMvpScope exports =
  Map.insert "Prelude" preludeScope exports
  where
    existing = Map.findWithDefault emptyScope "Prelude" exports
    preludeScope =
      existing
        { scopeTerms =
            Map.fromList
              [ ("++", resolvedTopLevel "Prelude" "++")
              ]
              <> scopeTerms existing,
          scopeTypes =
            Map.fromList
              [ ("Char", resolvedTopLevel "Prelude" "Char"),
                ("String", resolvedTopLevel "Prelude" "String")
              ]
              <> scopeTypes existing
        }

ensurePreludeMvpTerms :: [(Text, TypeScheme)] -> [(Text, TypeScheme)]
ensurePreludeMvpTerms terms =
  Map.toList (Map.fromList [("++", appendScheme)] <> Map.fromList terms)

appendScheme :: TypeScheme
appendScheme =
  ForAll [aVar] [] (TcFunTy listA (TcFunTy listA listA))
  where
    aVar = TyVarId "a" (Unique (-100))
    listA = TcTyCon (TyCon "[]" 1) [TcTyVar aVar]

normalizeImportedBindingName :: Text -> Text
normalizeImportedBindingName name =
  case T.uncons name of
    Just ('(', rest)
      | Just inner <- T.stripSuffix ")" rest ->
          inner
    _ -> name

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

parseTcTypeJson :: Aeson.Value -> AesonTypes.Parser TcType
parseTcTypeJson =
  Aeson.withObject "type" $ \obj -> do
    tag <- obj .: "tag" :: AesonTypes.Parser Text
    case tag of
      "var" -> TcTyVar <$> parseTyVarObject obj
      "meta" -> TcMetaTv . Unique <$> obj .: "unique"
      "con" ->
        TcTyCon
          <$> (TyCon <$> obj .: "name" <*> obj .: "arity")
          <*> (obj .: "args" >>= traverse parseTcTypeJson)
      "fun" ->
        TcFunTy
          <$> (obj .: "arg" >>= parseTcTypeJson)
          <*> (obj .: "result" >>= parseTcTypeJson)
      "forall" ->
        TcForAllTy
          <$> (obj .: "binder" >>= parseTyVarValue)
          <*> (obj .: "body" >>= parseTcTypeJson)
      "qual" ->
        TcQualTy
          <$> (obj .: "predicates" >>= traverse parsePredJson)
          <*> (obj .: "body" >>= parseTcTypeJson)
      "app" ->
        TcAppTy
          <$> (obj .: "fun" >>= parseTcTypeJson)
          <*> (obj .: "arg" >>= parseTcTypeJson)
      other -> fail ("unknown type tag: " <> T.unpack other)

parseTyVarObject :: AesonTypes.Object -> AesonTypes.Parser TyVarId
parseTyVarObject obj =
  TyVarId <$> obj .: "name" <*> (Unique <$> obj .: "unique")

parseTyVarValue :: Aeson.Value -> AesonTypes.Parser TyVarId
parseTyVarValue =
  Aeson.withObject "type variable" parseTyVarObject

parsePredJson :: Aeson.Value -> AesonTypes.Parser Pred
parsePredJson =
  Aeson.withObject "predicate" $ \obj -> do
    tag <- obj .: "tag" :: AesonTypes.Parser Text
    case tag of
      "class" ->
        ClassPred
          <$> obj .: "class"
          <*> (obj .: "args" >>= traverse parseTcTypeJson)
      "eq" ->
        EqPred
          <$> (obj .: "left" >>= parseTcTypeJson)
          <*> (obj .: "right" >>= parseTcTypeJson)
      other -> fail ("unknown predicate tag: " <> T.unpack other)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f value =
  case value of
    Left err -> Left (f err)
    Right ok -> Right ok
