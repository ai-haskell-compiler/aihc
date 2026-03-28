{-# LANGUAGE OverloadedStrings #-}

module ParserGolden
  ( CaseKind (..),
    ExpectedStatus (..),
    Outcome (..),
    ParserCase (..),
    fixtureRoot,
    exprFixtureRoot,
    moduleFixtureRoot,
    loadExprCases,
    loadModuleCases,
    parseParserCaseText,
    evaluateExprCase,
    evaluateModuleCase,
    progressSummary,
  )
where

import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    errorBundlePretty,
    parseExpr,
    parseModule,
  )
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

data CaseKind = CaseExpr | CaseModule deriving (Eq, Show)

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

data ParserCase = ParserCase
  { caseKind :: !CaseKind,
    caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseInput :: !Text,
    caseAst :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

exprFixtureRoot :: FilePath
exprFixtureRoot = fixtureRoot </> "expr"

moduleFixtureRoot :: FilePath
moduleFixtureRoot = fixtureRoot </> "module"

loadExprCases :: IO [ParserCase]
loadExprCases = loadCases CaseExpr exprFixtureRoot

loadModuleCases :: IO [ParserCase]
loadModuleCases = loadCases CaseModule moduleFixtureRoot

loadCases :: CaseKind -> FilePath -> IO [ParserCase]
loadCases kind root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles root
      mapM (loadParserCase kind) paths

loadParserCase :: CaseKind -> FilePath -> IO ParserCase
loadParserCase kind path = do
  source <- TIO.readFile path
  case parseParserCaseText kind path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseParserCaseText :: CaseKind -> FilePath -> Text -> Either String ParserCase
parseParserCaseText kind path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, astText, statusText, reasonText) <- parseYamlFixture path value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  ast <- validateAst path status (T.unpack astText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    ParserCase
      { caseKind = kind,
        caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseInput = inputText,
        caseAst = ast,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], Text, Text, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "parser fixture" $ \obj -> do
        exts <- obj .: "extensions"
        inputText <- obj .: "input"
        astText <- obj .:? "ast" .!= ""
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, inputText, astText, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid parser fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateExprCase :: ParserCase -> (Outcome, String)
evaluateExprCase meta =
  case parseExpr parserConfig (caseInput meta) of
    ParseOk ast -> classifySuccess meta (show (shorthand ast))
    ParseErr err -> classifyFailure meta (errorBundlePretty (Just (caseInput meta)) err)
  where
    parserConfig =
      defaultConfig
        { parserSourceName = casePath meta,
          parserExtensions = caseExtensions meta
        }

evaluateModuleCase :: ParserCase -> (Outcome, String)
evaluateModuleCase meta =
  case parseModule parserConfig (caseInput meta) of
    ParseOk ast -> classifySuccess meta (show (shorthand ast))
    ParseErr err -> classifyFailure meta (errorBundlePretty (Just (caseInput meta)) err)
  where
    parserConfig =
      defaultConfig
        { parserSourceName = casePath meta,
          parserExtensions = caseExtensions meta
        }

classifySuccess :: ParserCase -> String -> (Outcome, String)
classifySuccess meta actualAst =
  case caseStatus meta of
    StatusPass
      | actualAst == caseAst meta -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "AST mismatch (expected=" <> show (caseAst meta) <> ", actual=" <> show actualAst <> ")"
          )
    StatusFail ->
      ( OutcomeFail,
        "expected parse failure but parser succeeded with AST=" <> actualAst
      )
    StatusXFail ->
      (OutcomeFail, "expected xfail (known failing bug), but parser succeeded")
    StatusXPass
      | actualAst == caseAst meta -> (OutcomeXPass, "known bug still passes unexpectedly")
      | otherwise ->
          ( OutcomeFail,
            "expected xpass AST match but got AST=" <> actualAst
          )

classifyFailure :: ParserCase -> String -> (Outcome, String)
classifyFailure meta errDetails =
  case caseStatus meta of
    StatusPass ->
      ( OutcomeFail,
        "expected parse success, got parse error: " <> errDetails
      )
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass ->
      ( OutcomeFail,
        "expected xpass (known passing bug), got parse error: " <> errDetails
      )

progressSummary :: [(ParserCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

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
        Nothing -> Left ("Unknown parser extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

validateAst :: FilePath -> ExpectedStatus -> String -> Either String String
validateAst path status ast =
  let trimmed = trim ast
   in case status of
        StatusPass | null trimmed -> Left ("[ast] is required for pass status in " <> path)
        StatusXPass | null trimmed -> Left ("[ast] is required for xpass status in " <> path)
        _ -> Right trimmed

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "golden"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
