{-# LANGUAGE OverloadedStrings #-}

-- | Inline annotated golden test infrastructure for the type checker.
--
-- This is intentionally separate from the existing TC golden fixtures. The
-- existing fixtures assert the compact top-level signature summary; these
-- fixtures assert a human-readable source overlay for type-checker output.
module TcAnnotatedGolden
  ( ExpectedStatus (..),
    Outcome (..),
    TcAnnotatedCase (..),
    fixtureRoot,
    loadTcAnnotatedCases,
    evaluateTcAnnotatedCase,
    renderAnnotatedTcResults,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Extension,
    Module,
    importDeclModule,
    moduleImports,
    moduleName,
    parseExtensionName,
  )
import Aihc.Resolve (Package (..), PackageId (..), ResolveResult (..), extractInterface, resolveWithDeps, unnamedPackage)
import Aihc.Tc
  ( ClassInfo (ciName),
    DataFamilyInstanceInfo (dfiiAxiomName),
    DataTypeInfo (dtiName),
    InstanceInfo (iiDictName),
    TcInterface (..),
    TyConInfo (tciTyCon),
    emptyTcInterface,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (dropWhileEnd, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import TcAnnotatedRender (renderAnnotatedTcResults)

data ExpectedStatus
  = StatusPass
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data TcAnnotatedCase = TcAnnotatedCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseAnnotated :: ![String],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/annotated"

loadTcAnnotatedCases :: IO [TcAnnotatedCase]
loadTcAnnotatedCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadTcAnnotatedCase paths

loadTcAnnotatedCase :: FilePath -> IO TcAnnotatedCase
loadTcAnnotatedCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseTcAnnotatedFixture path value of
      Left e -> fail e
      Right c -> pure c

parseTcAnnotatedFixture :: FilePath -> Y.Value -> Either String TcAnnotatedCase
parseTcAnnotatedFixture path value = do
  (extNames, modules, annotatedTexts, statusText, reasonText) <-
    parseEither
      ( withObject "tc annotated fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          annotated <- obj .: "annotated" >>= parseAnnotatedList
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, annotated, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
      reason = trim (T.unpack reasonText)
      annotated = map (trim . T.unpack) annotatedTexts
  pure
    TcAnnotatedCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseAnnotated = annotated,
        caseStatus = status,
        caseReason = reason
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseAnnotatedList :: Y.Value -> Y.Parser [Text]
parseAnnotatedList = withArray "annotated" $ \arr ->
  mapM parseAnnotatedEntry (foldr (:) [] arr)
  where
    parseAnnotatedEntry (Y.String t) = pure t
    parseAnnotatedEntry _ = fail "each annotated entry must be a string"

evaluateTcAnnotatedCase :: TcAnnotatedCase -> IO (Outcome, String)
evaluateTcAnnotatedCase tc = do
  result <- try (evaluate (forceEvaluation (evaluateTcAnnotatedCasePure tc))) :: IO (Either ErrorCall (Outcome, String))
  pure $
    case result of
      Left exception -> classifyFailure tc ("exception: " <> displayException exception)
      Right outcome -> outcome

forceEvaluation :: (Outcome, String) -> (Outcome, String)
forceEvaluation result@(outcome, details) = outcome `seq` length details `seq` result

evaluateTcAnnotatedCasePure :: TcAnnotatedCase -> (Outcome, String)
evaluateTcAnnotatedCasePure tc =
  let parsedModules = map parseOne (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> classifyFailure tc ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps coreExports (map modulePackage modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              case typecheckModuleGraph coreInterface (map snd resolvedModules) of
                Left errMsg -> classifyFailure tc errMsg
                Right results ->
                  let actual = renderAnnotatedTcResults (caseModules tc) results
                   in classifySuccess tc actual
            ResolveResult {resolveErrors} ->
              classifyFailure tc ("resolve error: " <> show resolveErrors)
  where
    corePackage = Package "aihc-prim" (PackageId "aihc-prim")
    coreSource = "module GHC.Types (List(..)) where\ndata List a = [] | a : [a]\ninfixr 5 :\n"
    coreModule = parseCoreModule coreSource
    coreResolved = resolveWithDeps mempty [(corePackage, coreModule)]
    coreExports = extractInterface coreResolved
    coreInterface =
      case coreResolved of
        ResolveResult {resolvedModules = [(_, resolvedCore)], resolveErrors = []} ->
          snd (typecheckModulesWithInterface emptyTcInterface [resolvedCore])
        _ -> emptyTcInterface
    modulePackage modu
      | moduleName modu `elem` [Just "GHC.Classes", Just "GHC.Prim", Just "GHC.Tuple", Just "GHC.Types"] =
          (Package "aihc-prim" (PackageId "aihc-prim"), modu)
      | otherwise = (unnamedPackage, modu)
    parseOne input =
      let config =
            defaultConfig
              { parserSourceName = T.unpack (T.takeWhile (/= '\n') input),
                parserExtensions = caseExtensions tc
              }
          (errs, ast) = parseModule config input
       in if null errs
            then Right ast
            else Left (show errs)
    parseCoreModule input =
      let config = defaultConfig {parserSourceName = "GHC.Types"}
          (errs, ast) = parseModule config input
       in if null errs then ast else error (show errs)

data ModuleNode = ModuleNode
  { nodeIndex :: !Int,
    nodeModule :: !Module,
    nodeDependencies :: ![Int]
  }

typecheckModuleGraph :: TcInterface -> [Module] -> Either String [Module]
typecheckModuleGraph baseInterface modules = do
  (checkedModules, _) <- foldl' checkComponent (Right (Map.empty, Map.empty)) components
  traverse (lookupCheckedModule checkedModules) [0 .. length modules - 1]
  where
    moduleIndices =
      Map.fromList
        [ (name, index)
        | (index, modu) <- zip [0 ..] modules,
          Just name <- [moduleName modu]
        ]
    nodes =
      [ let dependencies = mapMaybe ((`Map.lookup` moduleIndices) . importDeclModule) (moduleImports modu)
         in (ModuleNode index modu dependencies, index, dependencies)
      | (index, modu) <- zip [0 ..] modules
      ]
    components = stronglyConnComp nodes
    checkComponent stateResult component = do
      (checkedByIndex, interfacesByIndex) <- stateResult
      let componentNodes = flattenComponent component
          componentIndices = Set.fromList (map nodeIndex componentNodes)
          dependencyIndices =
            Set.toList
              ( Set.fromList (concatMap nodeDependencies componentNodes)
                  `Set.difference` componentIndices
              )
      dependencyInterfaces <- traverse (lookupDependencyInterface interfacesByIndex) dependencyIndices
      let importedInterface = mconcat (baseInterface : dependencyInterfaces)
          (checked, checkedInterface) =
            typecheckModuleSccWithInterface importedInterface (map nodeModule componentNodes)
          localInterface = subtractInterface importedInterface checkedInterface
          checkedByIndex' = foldl' (\acc (node, modu) -> Map.insert (nodeIndex node) modu acc) checkedByIndex (zip componentNodes checked)
          interfacesByIndex' = foldl' (\acc node -> Map.insert (nodeIndex node) localInterface acc) interfacesByIndex componentNodes
      pure (checkedByIndex', interfacesByIndex')

flattenComponent :: SCC ModuleNode -> [ModuleNode]
flattenComponent component =
  case component of
    AcyclicSCC node -> [node]
    CyclicSCC nodes -> nodes

lookupDependencyInterface :: Map Int TcInterface -> Int -> Either String TcInterface
lookupDependencyInterface interfaces index =
  maybe (Left ("module graph dependency was not checked: " <> show index)) Right (Map.lookup index interfaces)

lookupCheckedModule :: Map Int Module -> Int -> Either String Module
lookupCheckedModule checked index =
  maybe (Left ("module graph result is missing: " <> show index)) Right (Map.lookup index checked)

subtractInterface :: TcInterface -> TcInterface -> TcInterface
subtractInterface imported complete =
  TcInterface
    { tcInterfaceTerms = withoutImported fst (tcInterfaceTerms imported) (tcInterfaceTerms complete),
      tcInterfaceTyCons = withoutImported tciTyCon (tcInterfaceTyCons imported) (tcInterfaceTyCons complete),
      tcInterfaceDataTypes = withoutImported dtiName (tcInterfaceDataTypes imported) (tcInterfaceDataTypes complete),
      tcInterfaceClasses = withoutImported ciName (tcInterfaceClasses imported) (tcInterfaceClasses complete),
      tcInterfaceInstances = withoutImported iiDictName (tcInterfaceInstances imported) (tcInterfaceInstances complete),
      tcInterfaceDataFamilyInstances = withoutImported dfiiAxiomName (tcInterfaceDataFamilyInstances imported) (tcInterfaceDataFamilyInstances complete)
    }

withoutImported :: (Ord key) => (value -> key) -> [value] -> [value] -> [value]
withoutImported key imported =
  let importedKeys = Set.fromList (map key imported)
   in filter ((`Set.notMember` importedKeys) . key)

classifySuccess :: TcAnnotatedCase -> [String] -> (Outcome, String)
classifySuccess tc actual =
  let expected = caseAnnotated tc
      outputMatches = map trim actual == map trim expected
   in case caseStatus tc of
        StatusPass
          | outputMatches -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "annotated output mismatch\nexpected:\n"
                  <> unlines expected
                  <> "\nactual:\n"
                  <> unlines actual
              )
        StatusXFail
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeXFail, "")
        StatusXPass
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass output match")

classifyFailure :: TcAnnotatedCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
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
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "annotated"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
