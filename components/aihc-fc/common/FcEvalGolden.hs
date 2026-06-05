{-# LANGUAGE OverloadedStrings #-}

-- | Golden test infrastructure for System FC evaluation fixtures.
module FcEvalGolden
  ( Outcome (..),
    FcEvalCase (..),
    evalFixtureRoot,
    loadFcEvalCases,
    evaluateFcEvalCase,
  )
where

import Aihc.Fc (DesugarResult (..), FcProgram (..), desugarModuleWithBindings, evalProgramBinding, renderRawValue)
import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    parseExpr,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Decl (..),
    Expr,
    Extension,
    ImportDecl (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NameType (..),
    Rhs (..),
    ValueDecl (..),
    mkUnqualifiedName,
    parseExtensionName,
  )
import Aihc.Resolve (ResolveResult (..), resolveWithDeps)
import Aihc.Tc (TcBindingResult, TcModuleResult (..), tcModuleDiagnostics, tcmBindings, typecheckModulesWithEnv)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (joinPath, takeDirectory, takeExtension, (</>))

data ExpectedStatus
  = StatusPass
  | StatusFail
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data FcEvalCase = FcEvalCase
  { evalCaseId :: !String,
    evalCaseCategory :: !String,
    evalCasePath :: !FilePath,
    evalCaseExtensions :: ![Extension],
    evalCaseDependencies :: ![Text],
    evalCaseModules :: ![Text],
    evalCaseExpression :: !Text,
    evalCaseOutput :: !String,
    evalCaseStatus :: !ExpectedStatus,
    evalCaseReason :: !String
  }
  deriving (Eq, Show)

evalFixtureRoot :: FilePath
evalFixtureRoot = "test/Test/Fixtures/eval"

evalBindingName :: Text
evalBindingName = "__aihc_eval__"

loadFcEvalCases :: IO [FcEvalCase]
loadFcEvalCases = do
  exists <- doesDirectoryExist evalFixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles evalFixtureRoot
      mapM loadFcEvalCase paths

loadFcEvalCase :: FilePath -> IO FcEvalCase
loadFcEvalCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML eval fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFcEvalFixture path value of
      Left e -> fail e
      Right c -> pure c

parseFcEvalFixture :: FilePath -> Y.Value -> Either String FcEvalCase
parseFcEvalFixture path value = do
  (extNames, dependencies, modules, expression, output, statusText, reasonText) <-
    parseEither
      ( withObject "fc eval fixture" $ \obj -> do
          exts <- obj .: "extensions"
          deps <- obj .:? "dependencies" .!= []
          mods <- obj .: "modules" >>= parseModules
          expr <- obj .: "expression"
          expected <- obj .: "output"
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, deps, mods, expr, expected, status, reason)
      )
      value
  if null modules
    then Left ("Eval fixture must define at least one module in " <> path)
    else do
      exts <- validateExtensions path extNames
      status <- parseStatus path statusText
      let relPath = dropRootPrefix path
          category = categoryFromPath relPath
      pure
        FcEvalCase
          { evalCaseId = relPath,
            evalCaseCategory = category,
            evalCasePath = relPath,
            evalCaseExtensions = exts,
            evalCaseDependencies = dependencies,
            evalCaseModules = modules,
            evalCaseExpression = expression,
            evalCaseOutput = trim (T.unpack output),
            evalCaseStatus = status,
            evalCaseReason = trim (T.unpack reasonText)
          }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

evaluateFcEvalCase :: FcEvalCase -> IO (Outcome, String)
evaluateFcEvalCase tc =
  case parseInputs tc of
    Left errMsg -> pure (classifyFailure tc errMsg)
    Right (modules, expr) -> do
      let evalModules = combineModules modules expr
      dependencyModules <- loadDependencyModules tc evalModules
      pure $
        case dependencyModules of
          Left errMsg -> classifyFailure tc errMsg
          Right deps ->
            let resolved = resolveWithDeps mempty (deps <> evalModules)
             in case resolved of
                  ResolveResult {resolvedModules, resolveErrors = []} ->
                    let tcResults = typecheckModulesWithEnv [] resolvedModules
                     in if all tcmSuccess tcResults
                          then
                            let allBindings = moduleGroupBindings tcResults
                                results = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
                             in if all dsSuccess results
                                  then case evalProgramBinding evalBindingName (concatPrograms (map dsProgram results)) >>= renderRawValue of
                                    Right actual -> classifySuccess tc (T.unpack actual)
                                    Left err -> classifyFailure tc ("eval error: " <> show err)
                                  else classifyFailure tc ("desugar error: " <> unlines (concatMap dsErrors results))
                          else classifyFailure tc ("typecheck error: " <> renderTcErrors tcResults)
                  ResolveResult {resolveErrors} ->
                    classifyFailure tc ("resolve error: " <> show resolveErrors)

parseInputs :: FcEvalCase -> Either String ([Module], Expr)
parseInputs tc = do
  modules <- mapM (parseOneModuleWithExtensions (evalCaseExtensions tc)) (evalCaseModules tc)
  expr <- parseOneExpr (evalCaseExpression tc)
  pure (modules, expr)
  where
    parseOneExpr input =
      case parseExpr (config (evalCasePath tc <> ":expression")) input of
        ParseOk expr -> Right expr
        ParseErr err -> Left ("parse expression error: " <> show err)
    config source =
      defaultConfig
        { parserSourceName = source,
          parserExtensions = evalCaseExtensions tc
        }

parseOneModuleWithExtensions :: [Extension] -> Text -> Either String Module
parseOneModuleWithExtensions extensions input =
  parseOneModule (T.unpack (T.takeWhile (/= '\n') input)) extensions input

parseOneModule :: FilePath -> [Extension] -> Text -> Either String Module
parseOneModule sourceName extensions input =
  let cfg =
        defaultConfig
          { parserSourceName = sourceName,
            parserExtensions = extensions
          }
      (errs, ast) = parseModule cfg input
   in if null errs
        then Right ast
        else Left ("parse module error: " <> show errs)

combineModules :: [Module] -> Expr -> [Module]
combineModules modules expr =
  case modules of
    [] -> [emptyEvalModule expr]
    _ ->
      let depModules = init modules
          evalModule = last modules
       in depModules <> [evalModule {moduleDecls = moduleDecls evalModule <> [evalDecl expr]}]

emptyEvalModule :: Expr -> Module
emptyEvalModule expr =
  Module
    { moduleAnns = [],
      moduleHead = Nothing,
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls = [evalDecl expr]
    }

evalDecl :: Expr -> Decl
evalDecl expr =
  DeclValue $
    FunctionBind
      (mkUnqualifiedName NameVarId evalBindingName)
      [ Match
          { matchAnns = [],
            matchHeadForm = MatchHeadPrefix,
            matchPats = [],
            matchRhs = UnguardedRhs [] expr Nothing
          }
      ]

renderTcErrors :: [TcModuleResult] -> String
renderTcErrors results =
  let rendered = unlines [show diagnostic | result <- results, diagnostic <- tcModuleDiagnostics (tcmModule result)]
   in if null (trim rendered)
        then "type checker failed without diagnostics"
        else rendered

moduleGroupBindings :: [TcModuleResult] -> [TcBindingResult]
moduleGroupBindings =
  concatMap tcmBindings

concatPrograms :: [FcProgram] -> FcProgram
concatPrograms programs =
  FcProgram (concatMap fcTopBinds programs)

loadDependencyModules :: FcEvalCase -> [Module] -> IO (Either String [Module])
loadDependencyModules tc evalModules =
  case evalCaseDependencies tc of
    [] -> pure (Right [])
    dependencies -> do
      roots <- traverse resolveDependencyRoot dependencies
      case sequence roots of
        Left errMsg -> pure (Left errMsg)
        Right packageRoots ->
          loadTransitiveModules packageRoots (initialDependencyModules evalModules)

resolveDependencyRoot :: Text -> IO (Either String (Text, FilePath))
resolveDependencyRoot dependency =
  case dependency of
    "aihc-base" -> do
      envRoot <- lookupEnv "AIHC_BASE_SRC"
      root <- maybe defaultAihcBaseRoot pure envRoot
      pure (Right (dependency, root))
    _ ->
      pure (Left ("unknown eval fixture dependency: " <> T.unpack dependency))

defaultAihcBaseRoot :: IO FilePath
defaultAihcBaseRoot = do
  cwd <- getCurrentDirectory
  findUp cwd
  where
    findUp dir = do
      let candidate = dir </> "core-libs" </> "aihc-base"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure candidate
            else findUp parent

initialDependencyModules :: [Module] -> Set.Set Text
initialDependencyModules modules =
  Set.insert "Prelude" (importedModuleNames modules)

importedModuleNames :: [Module] -> Set.Set Text
importedModuleNames modules =
  Set.fromList [importDeclModule importDecl | modu <- modules, importDecl <- moduleImports modu]

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
              case parseOneModule path [] source of
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
  joinPath (map T.unpack (T.splitOn "." moduleName)) <> ".hs"

classifySuccess :: FcEvalCase -> String -> (Outcome, String)
classifySuccess tc actual =
  case evalCaseStatus tc of
    StatusPass
      | trim actual == trim (evalCaseOutput tc) -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected:\n" <> evalCaseOutput tc <> "\nactual:\n" <> trim actual
          )
    StatusFail ->
      (OutcomeFail, "expected failure but evaluation succeeded")
    StatusXFail
      | trim actual == trim (evalCaseOutput tc) -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | trim actual == trim (evalCaseOutput tc) -> (OutcomeXPass, "known bug still passes")
      | otherwise ->
          (OutcomeFail, "expected xpass output match but got: " <> trim actual)

classifyFailure :: FcEvalCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case evalCaseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

validateExtensions :: FilePath -> [Text] -> Either String [Extension]
validateExtensions path = traverse parseOne
  where
    parseOne raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (evalFixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "eval"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
