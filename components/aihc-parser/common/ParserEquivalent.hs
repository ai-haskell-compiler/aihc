{-# LANGUAGE OverloadedStrings #-}

module ParserEquivalent
  ( CaseKind (..),
    ExpectedStatus (..),
    Outcome (..),
    EquivalentCase (..),
    fixtureRoot,
    exprFixtureRoot,
    moduleFixtureRoot,
    declFixtureRoot,
    patternFixtureRoot,
    loadExprCases,
    loadModuleCases,
    loadDeclCases,
    loadPatternCases,
    parseEquivalentCaseText,
    evaluateExprCase,
    evaluateModuleCase,
    evaluateDeclCase,
    evaluatePatternCase,
    progressSummary,
  )
where

import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseDecl,
    parseExpr,
    parseModule,
    parsePattern,
  )
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax
  ( Decl,
    Expr (..),
    ExtensionSetting,
    LanguageEdition (Haskell2010Edition),
    Module,
    Pattern (..),
    editionFromExtensionSettings,
    effectiveExtensions,
    parseExtensionSettingName,
    stripAnnotations,
  )
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.Data (Data (..))
import Data.List (dropWhileEnd, sort, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import ParserValidation (stripParens)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (addTrailingPathSeparator, normalise, takeDirectory, takeExtension, (</>))

data CaseKind = CaseExpr | CaseModule | CaseDecl | CasePattern deriving (Eq, Show)

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

data EquivalentCase = EquivalentCase
  { caseKind :: !CaseKind,
    caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![ExtensionSetting],
    caseInputs :: ![Text],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/equivalent"

exprFixtureRoot :: FilePath
exprFixtureRoot = fixtureRoot </> "expr"

moduleFixtureRoot :: FilePath
moduleFixtureRoot = fixtureRoot </> "module"

declFixtureRoot :: FilePath
declFixtureRoot = fixtureRoot </> "decl"

patternFixtureRoot :: FilePath
patternFixtureRoot = fixtureRoot </> "pattern"

loadExprCases :: IO [EquivalentCase]
loadExprCases = loadCases CaseExpr exprFixtureRoot

loadModuleCases :: IO [EquivalentCase]
loadModuleCases = loadCases CaseModule moduleFixtureRoot

loadDeclCases :: IO [EquivalentCase]
loadDeclCases = loadCases CaseDecl declFixtureRoot

loadPatternCases :: IO [EquivalentCase]
loadPatternCases = loadCases CasePattern patternFixtureRoot

loadCases :: CaseKind -> FilePath -> IO [EquivalentCase]
loadCases kind root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles root
      mapM (loadEquivalentCase kind) paths

loadEquivalentCase :: CaseKind -> FilePath -> IO EquivalentCase
loadEquivalentCase kind path = do
  source <- TIO.readFile path
  case parseEquivalentCaseText kind path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseEquivalentCaseText :: CaseKind -> FilePath -> Text -> Either String EquivalentCase
parseEquivalentCaseText kind path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML equivalence fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputTexts, statusText, reasonText) <- parseYamlFixture path value
  exts <- validateExtensions path extNames
  inputs <- validateInputs path inputTexts
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    EquivalentCase
      { caseKind = kind,
        caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseInputs = inputs,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], [Text], Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "parser equivalence fixture" $ \obj -> do
        exts <- obj .: "extensions"
        inputs <- obj .: "equivalent"
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, inputs, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid parser equivalence fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateExprCase :: EquivalentCase -> (Outcome, String)
evaluateExprCase meta = evaluateEquivalentCase meta parseExprValue

evaluateModuleCase :: EquivalentCase -> (Outcome, String)
evaluateModuleCase meta = evaluateEquivalentCase meta parseModuleValue

evaluateDeclCase :: EquivalentCase -> (Outcome, String)
evaluateDeclCase meta = evaluateEquivalentCase meta parseDeclValue

evaluatePatternCase :: EquivalentCase -> (Outcome, String)
evaluatePatternCase meta = evaluateEquivalentCase meta parsePatternValue

evaluateEquivalentCase ::
  (Eq a, Data a, Shorthand a) =>
  EquivalentCase ->
  (ParserConfig -> Text -> Either String a) ->
  (Outcome, String)
evaluateEquivalentCase meta parseValue =
  case traverse parseOne (zip [(1 :: Int) ..] (caseInputs meta)) of
    Left err -> classifyFailure meta err
    Right parsed -> classifyEquivalence meta parsed
  where
    config = parserConfig meta
    parseOne (ix, inputText) =
      case parseValue config inputText of
        Left err -> Left ("input #" <> show ix <> " failed to parse: " <> err)
        Right ast -> Right (ix, normalize ast)

parseExprValue :: ParserConfig -> Text -> Either String Expr
parseExprValue config source =
  case parseExpr config source of
    ParseOk ast -> Right ast
    ParseErr err -> Left (formatParseErrors (parserSourceName config) (Just source) err)

parseModuleValue :: ParserConfig -> Text -> Either String Module
parseModuleValue config source =
  case parseModule config source of
    ([], ast) -> Right ast
    (errs, _) -> Left (formatParseErrors (parserSourceName config) (Just source) errs)

parseDeclValue :: ParserConfig -> Text -> Either String Decl
parseDeclValue config source =
  case parseDecl config source of
    ParseOk ast -> Right ast
    ParseErr err -> Left (formatParseErrors (parserSourceName config) (Just source) err)

parsePatternValue :: ParserConfig -> Text -> Either String Pattern
parsePatternValue config source =
  case parsePattern config source of
    ParseOk ast -> Right ast
    ParseErr err -> Left (formatParseErrors (parserSourceName config) (Just source) err)

classifyEquivalence :: (Eq a, Shorthand a) => EquivalentCase -> [(Int, a)] -> (Outcome, String)
classifyEquivalence meta parsed =
  case findMismatch parsed of
    Nothing -> classifyEquivalent meta
    Just (expected, actual) -> classifyDifferent meta expected actual

findMismatch :: (Eq a) => [(Int, a)] -> Maybe ((Int, a), (Int, a))
findMismatch [] = Nothing
findMismatch [_] = Nothing
findMismatch (expected : rest) =
  case filter ((/= snd expected) . snd) rest of
    [] -> Nothing
    actual : _ -> Just (expected, actual)

classifyEquivalent :: EquivalentCase -> (Outcome, String)
classifyEquivalent meta =
  case caseStatus meta of
    StatusPass -> (OutcomePass, "")
    StatusFail ->
      ( OutcomeFail,
        "expected non-equivalent inputs but normalized ASTs matched"
      )
    StatusXFail ->
      ( OutcomeXPass,
        "expected xfail (known failing bug), but inputs now normalize to equivalent ASTs"
      )

classifyDifferent :: (Shorthand a) => EquivalentCase -> (Int, a) -> (Int, a) -> (Outcome, String)
classifyDifferent meta expected actual =
  case caseStatus meta of
    StatusPass ->
      ( OutcomeFail,
        equivalenceMismatchDetails expected actual
      )
    StatusFail -> (OutcomePass, "")
    StatusXFail ->
      ( OutcomeXFail,
        "known bug still present: " <> equivalenceMismatchDetails expected actual
      )

classifyFailure :: EquivalentCase -> String -> (Outcome, String)
classifyFailure meta errDetails =
  case caseStatus meta of
    StatusPass ->
      ( OutcomeFail,
        "expected parse success, got parse error: " <> errDetails
      )
    -- StatusFail means the inputs should parse successfully but normalize to
    -- different ASTs; parse errors indicate a malformed equivalence fixture.
    StatusFail ->
      ( OutcomeFail,
        "expected parse success for non-equivalent inputs, got parse error: " <> errDetails
      )
    StatusXFail ->
      ( OutcomeXFail,
        "known bug still present: " <> errDetails
      )

equivalenceMismatchDetails :: (Shorthand a) => (Int, a) -> (Int, a) -> String
equivalenceMismatchDetails (expectedIx, expected) (actualIx, actual) =
  "normalized AST mismatch between input #"
    <> show expectedIx
    <> " and input #"
    <> show actualIx
    <> " (expected shorthand="
    <> show (shorthand expected)
    <> ", actual shorthand="
    <> show (shorthand actual)
    <> ")"

normalize :: (Data a) => a -> a
normalize = stripParens . stripAnnotations

parserConfig :: EquivalentCase -> ParserConfig
parserConfig meta =
  defaultConfig
    { parserSourceName = casePath meta,
      parserExtensions = effectiveExtensions edition (caseExtensions meta)
    }
  where
    edition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (caseExtensions meta))

progressSummary :: [(EquivalentCase, Outcome, String)] -> (Int, Int, Int, Int)
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

validateInputs :: FilePath -> [Text] -> Either String [Text]
validateInputs path inputs =
  case inputs of
    [] -> Left ("[equivalent] must contain at least two inputs in " <> path)
    [_] -> Left ("[equivalent] must contain at least two inputs in " <> path)
    _ -> Right inputs

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

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  fromMaybe path (stripPrefix prefix normalizedPath)
  where
    prefix = addTrailingPathSeparator (normalise fixtureRoot)
    normalizedPath = normalise path

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "equivalent"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
