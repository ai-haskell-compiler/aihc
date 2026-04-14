{-# LANGUAGE OverloadedStrings #-}

module ParserGolden
  ( CaseKind (..),
    ExpectedStatus (..),
    Outcome (..),
    ParserCase (..),
    fixtureRoot,
    exprFixtureRoot,
    moduleFixtureRoot,
    patternFixtureRoot,
    loadExprCases,
    loadModuleCases,
    loadPatternCases,
    parseParserCaseText,
    evaluateExprCase,
    evaluateModuleCase,
    evaluatePatternCase,
    progressSummary,
  )
where

import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseExpr,
    parseModule,
    parsePattern,
  )
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax
  ( ExtensionSetting,
    LanguageEdition (Haskell2010Edition),
    editionFromExtensionSettings,
    effectiveExtensions,
    parseExtensionSettingName,
  )
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Megaparsec.Error qualified as MPE

data CaseKind = CaseExpr | CaseModule | CasePattern deriving (Eq, Show)

data ExpectedStatus
  = StatusPass
  | StatusFail
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
    caseExtensions :: ![ExtensionSetting],
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

patternFixtureRoot :: FilePath
patternFixtureRoot = fixtureRoot </> "pattern"

loadExprCases :: IO [ParserCase]
loadExprCases = loadCases CaseExpr exprFixtureRoot

loadModuleCases :: IO [ParserCase]
loadModuleCases = loadCases CaseModule moduleFixtureRoot

loadPatternCases :: IO [ParserCase]
loadPatternCases = loadCases CasePattern patternFixtureRoot

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
    ParseErr err -> classifyFailure meta (MPE.errorBundlePretty err)
  where
    edition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (caseExtensions meta))
    parserConfig =
      ( defaultConfig
          { parserSourceName = casePath meta,
            parserExtensions = effectiveExtensions edition (caseExtensions meta)
          }
      )

evaluateModuleCase :: ParserCase -> (Outcome, String)
evaluateModuleCase meta =
  let (errs, ast) = parseModule parserConfig (caseInput meta)
   in if null errs
        then classifySuccess meta (show (shorthand ast))
        else classifyFailure meta (formatParseErrors (casePath meta) (Just (caseInput meta)) errs)
  where
    edition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (caseExtensions meta))
    parserConfig =
      ( defaultConfig
          { parserSourceName = casePath meta,
            parserExtensions = effectiveExtensions edition (caseExtensions meta)
          }
      )

evaluatePatternCase :: ParserCase -> (Outcome, String)
evaluatePatternCase meta =
  case parsePattern parserConfig (caseInput meta) of
    ParseOk ast -> classifySuccess meta (show (shorthand ast))
    ParseErr err -> classifyFailure meta (MPE.errorBundlePretty err)
  where
    edition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (caseExtensions meta))
    parserConfig =
      ( defaultConfig
          { parserSourceName = casePath meta,
            parserExtensions = effectiveExtensions edition (caseExtensions meta)
          }
      )

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
    StatusXFail
      | null (caseAst meta) ->
          ( OutcomeXPass,
            "expected xfail (known failing bug), but parser succeeded"
          )
      | actualAst == caseAst meta -> (OutcomeXPass, "expected xfail (known failing bug), but parser now produces correct AST")
      | otherwise ->
          ( OutcomeXFail,
            "known bug still present: AST mismatch (expected=" <> show (caseAst meta) <> ", actual=" <> show actualAst <> ")"
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

validateExtensions :: FilePath -> [Text] -> Either String [ExtensionSetting]
validateExtensions path = traverse parseOne
  where
    parseOne raw =
      case parseExtensionSettingName raw of
        Just setting -> Right setting
        Nothing -> Left ("Unknown parser extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xfail" -> Right StatusXFail
    "xpass" -> Left ("xpass is not allowed in " <> path <> ": use xfail instead")
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        _ -> Right trimmed

validateAst :: FilePath -> ExpectedStatus -> String -> Either String String
validateAst path status ast =
  let trimmed = trim ast
   in case status of
        StatusPass | null trimmed -> Left ("[ast] is required for pass status in " <> path)
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
