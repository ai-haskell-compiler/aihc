{-# LANGUAGE OverloadedStrings #-}

module ParserErrorGolden
  ( ExpectedStatus (..),
    Outcome (..),
    ErrorMessageCase (..),
    fixtureRoot,
    loadErrorMessageCases,
    parseErrorMessageCaseText,
    evaluateErrorMessageCase,
    progressSummary,
  )
where

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import GhcOracle (oracleModuleParseErrorWithNamesAt)
import Parser (defaultConfig, errorBundlePretty, parseModuleAt)
import Parser.Types (ParseResult (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

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

data ErrorMessageCase = ErrorMessageCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseSource :: !Text,
    caseExpectedGhc :: !Text,
    caseExpectedAihc :: !Text,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/error-messages"

loadErrorMessageCases :: IO [ErrorMessageCase]
loadErrorMessageCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadErrorMessageCase paths

loadErrorMessageCase :: FilePath -> IO ErrorMessageCase
loadErrorMessageCase path = do
  source <- TIO.readFile path
  case parseErrorMessageCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseErrorMessageCaseText :: FilePath -> Text -> Either String ErrorMessageCase
parseErrorMessageCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (inputText, ghcText, aihcText, statusText, reasonText) <- parseYamlFixture path value
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    ErrorMessageCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseSource = inputText,
        caseExpectedGhc = normalizeText ghcText,
        caseExpectedAihc = normalizeText aihcText,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String (Text, Text, Text, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "parser error fixture" $ \obj -> do
        inputText <- obj .: "src"
        ghcText <- obj .: "ghc"
        aihcText <- obj .: "aihc"
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (inputText, ghcText, aihcText, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid parser error fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateErrorMessageCase :: ErrorMessageCase -> (Outcome, String)
evaluateErrorMessageCase meta =
  case ghcMismatch meta of
    Just details -> (OutcomeFail, details)
    Nothing -> classifyAihcResult meta (renderAihcMessage meta)

ghcMismatch :: ErrorMessageCase -> Maybe String
ghcMismatch meta =
  case oracleModuleParseErrorWithNamesAt sourceName [] Nothing (caseSource meta) of
    Left details ->
      Just ("expected GHC parse failure, but parser accepted the input: " <> T.unpack details)
    Right actual
      | normalizeText actual == caseExpectedGhc meta -> Nothing
      | otherwise ->
          Just
            ( "GHC error mismatch. expected="
                <> show (T.unpack (caseExpectedGhc meta))
                <> " actual="
                <> show (T.unpack (normalizeText actual))
            )

renderAihcMessage :: ErrorMessageCase -> Either String Text
renderAihcMessage meta =
  case parseModuleAt sourceName defaultConfig (caseSource meta) of
    ParseErr bundle -> Right (normalizeText (T.pack (errorBundlePretty bundle)))
    ParseOk _ -> Left "aihc parser accepted the input"

classifyAihcResult :: ErrorMessageCase -> Either String Text -> (Outcome, String)
classifyAihcResult meta actualResult =
  let matchesExpected =
        case actualResult of
          Right actual -> actual == caseExpectedAihc meta
          Left _ -> False
      renderDetails =
        case actualResult of
          Left msg -> msg
          Right actual ->
            "aihc error mismatch. expected="
              <> show (T.unpack (caseExpectedAihc meta))
              <> " actual="
              <> show (T.unpack actual)
   in case caseStatus meta of
        StatusPass
          | matchesExpected -> (OutcomePass, "")
          | otherwise -> (OutcomeFail, renderDetails)
        StatusFail
          | matchesExpected -> (OutcomeFail, "expected fixture mismatch, but aihc matched expected output")
          | otherwise -> (OutcomePass, "")
        StatusXFail
          | matchesExpected -> (OutcomeFail, "expected xfail (known error-message mismatch), but aihc matched expected output")
          | otherwise -> (OutcomeXFail, "")
        StatusXPass
          | matchesExpected -> (OutcomeXPass, "known error-message bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, renderDetails)

progressSummary :: [(ErrorMessageCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

sourceName :: FilePath
sourceName = "test.hs"

normalizeText :: Text -> Text
normalizeText = normalizeLines . dropTrailingNewlines . normalizeLineEndings

normalizeLineEndings :: Text -> Text
normalizeLineEndings = T.replace "\r\n" "\n"

dropTrailingNewlines :: Text -> Text
dropTrailingNewlines = T.dropWhileEnd (`elem` ['\n', '\r'])

normalizeLines :: Text -> Text
normalizeLines = T.intercalate "\n" . map T.stripEnd . T.lines

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path rawStatus =
  case map toLower (T.unpack (T.strip rawStatus)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xfail" -> Right StatusXFail
    "xpass" -> Right StatusXPass
    other -> Left ("Unknown [status] " <> show other <> " in " <> path)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status rawReason =
  let trimmed = trim rawReason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles root = do
  entries <- listDirectory root
  paths <- mapM visit (sort entries)
  pure (concat paths)
  where
    visit name
      | isHidden name = pure []
      | otherwise = do
          let path = root </> name
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else pure [path | isYamlFile path]

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix = makeRelative fixtureRoot

categoryFromPath :: FilePath -> String
categoryFromPath relPath =
  case takeDirectory relPath of
    "." -> "error-messages"
    dir -> dir

isYamlFile :: FilePath -> Bool
isYamlFile path =
  let ext = map toLower (takeExtension path)
   in ext == ".yaml" || ext == ".yml"

isHidden :: FilePath -> Bool
isHidden [] = False
isHidden (c : _) = c == '.'

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
