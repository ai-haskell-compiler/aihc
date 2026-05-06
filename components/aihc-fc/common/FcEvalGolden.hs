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

import Aihc.Fc (DesugarResult (..), desugarModuleWithTcResult, evalProgramBinding, renderRawValue)
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
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    UnqualifiedName (..),
    ValueDecl (..),
    mkUnqualifiedName,
    parseExtensionName,
  )
import Aihc.Tc (TcBindingResult (..), TcModuleResult (..), TcType (..), TyCon (..))
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

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
  (extNames, modules, expression, output, statusText, reasonText) <-
    parseEither
      ( withObject "fc eval fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          expr <- obj .: "expression"
          expected <- obj .: "output"
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expr, expected, status, reason)
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

evaluateFcEvalCase :: FcEvalCase -> (Outcome, String)
evaluateFcEvalCase tc =
  case parseInputs tc of
    Left errMsg -> classifyFailure tc errMsg
    Right (modules, expr) ->
      let evalModule = combineModules modules expr
          result = desugarModuleWithTcResult (syntheticTcResult evalModule) evalModule
       in if dsSuccess result
            then case evalProgramBinding evalBindingName (dsProgram result) >>= renderRawValue of
              Right actual -> classifySuccess tc (T.unpack actual)
              Left err -> classifyFailure tc ("eval error: " <> show err)
            else classifyFailure tc ("desugar error: " <> unlines (dsErrors result))

parseInputs :: FcEvalCase -> Either String ([Module], Expr)
parseInputs tc = do
  modules <- mapM parseOneModule (evalCaseModules tc)
  expr <- parseOneExpr (evalCaseExpression tc)
  pure (modules, expr)
  where
    config source =
      defaultConfig
        { parserSourceName = source,
          parserExtensions = evalCaseExtensions tc
        }
    parseOneModule input =
      let cfg = config (T.unpack (T.takeWhile (/= '\n') input))
          (errs, ast) = parseModule cfg input
       in if null errs
            then Right ast
            else Left ("parse module error: " <> show errs)
    parseOneExpr input =
      case parseExpr (config (evalCasePath tc <> ":expression")) input of
        ParseOk expr -> Right expr
        ParseErr err -> Left ("parse expression error: " <> show err)

combineModules :: [Module] -> Expr -> Module
combineModules modules expr =
  case modules of
    [] -> emptyEvalModule expr
    firstModule : _ ->
      firstModule
        { moduleLanguagePragmas = concatMap moduleLanguagePragmas modules,
          moduleImports = concatMap moduleImports modules,
          moduleDecls = concatMap moduleDecls modules <> [evalDecl expr]
        }

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

syntheticTcResult :: Module -> TcModuleResult
syntheticTcResult modu =
  TcModuleResult
    { tcmBindings = concatMap bindingTypes (moduleDecls modu),
      tcmDiagnostics = [],
      tcmSuccess = True
    }

bindingTypes :: Decl -> [TcBindingResult]
bindingTypes decl =
  case decl of
    DeclAnn _ inner -> bindingTypes inner
    DeclValue (FunctionBind name matches) ->
      [TcBindingResult (unqualifiedNameText name) (functionType (matchArity matches))]
    DeclValue (PatternBind _ pat _) ->
      [TcBindingResult name unknownTy | Just name <- [barePatternName pat]]
    _ -> []

matchArity :: [Match] -> Int
matchArity [] = 0
matchArity (match : _) = length (matchPats match)

functionType :: Int -> TcType
functionType arity =
  foldr TcFunTy unknownTy (replicate arity unknownTy)

unknownTy :: TcType
unknownTy = TcTyCon (TyCon "?" 0) []

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

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
