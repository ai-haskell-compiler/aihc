{-# LANGUAGE OverloadedStrings #-}

-- | Golden test infrastructure for CLI binaries (aihc-lexer, aihc-parser).
--
-- This module provides in-process testing that directly calls the CLI core
-- functions and captures their output, avoiding the need to spawn external
-- processes or have executables installed in PATH.
module CLIGolden
  ( ExpectedStatus (..),
    Outcome (..),
    CLICase (..),
    CLITool (..),
    fixtureRoot,
    loadLexerCLICases,
    loadParserCLICases,
    parseCLICaseText,
    evaluateCLICase,
    progressSummary,
  )
where

import qualified Aihc.Parser.Run.Lexer as LexerRun
import qualified Aihc.Parser.Run.Parser as ParserRun
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
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))

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

-- | Which CLI tool to run
data CLITool = ToolLexer | ToolParser
  deriving (Eq, Show)

data CLICase = CLICase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseArgs :: ![String],
    caseInput :: !Text,
    caseExpectedOutput :: !Text,
    caseExpectedExitCode :: !Int,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/cli"

lexerFixtureRoot :: FilePath
lexerFixtureRoot = fixtureRoot </> "lexer"

parserFixtureRoot :: FilePath
parserFixtureRoot = fixtureRoot </> "parser"

loadLexerCLICases :: IO [CLICase]
loadLexerCLICases = loadCases lexerFixtureRoot

loadParserCLICases :: IO [CLICase]
loadParserCLICases = loadCases parserFixtureRoot

loadCases :: FilePath -> IO [CLICase]
loadCases root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles root
      mapM (loadCLICase root) paths

loadCLICase :: FilePath -> FilePath -> IO CLICase
loadCLICase root path = do
  source <- TIO.readFile path
  case parseCLICaseText root path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseCLICaseText :: FilePath -> FilePath -> Text -> Either String CLICase
parseCLICaseText root path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (args, inputText, expectedOutput, exitCode, statusText, reasonText) <-
    parseYamlFixture path value
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix root path
      category = categoryFromPath relPath
  pure
    CLICase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseArgs = map T.unpack args,
        caseInput = inputText,
        caseExpectedOutput = expectedOutput,
        caseExpectedExitCode = exitCode,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture ::
  FilePath ->
  Y.Value ->
  Either String ([Text], Text, Text, Int, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "cli fixture" $ \obj -> do
        args <- obj .:? "args" .!= []
        inputText <- obj .: "input"
        expectedOutput <- obj .: "output"
        exitCode <- obj .:? "exit_code" .!= 0
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (args, inputText, expectedOutput, exitCode, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid CLI fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

-- | Run a CLI case in-process and evaluate the outcome.
-- Returns (Outcome, detail message).
evaluateCLICase :: CLITool -> CLICase -> IO (Outcome, String)
evaluateCLICase tool meta = do
  let result = runCLIInProcess tool (caseArgs meta) (caseInput meta)
      -- Combine stdout and stderr for output comparison
      actualOutput = T.stripEnd (cliStdout result <> cliStderr result)
      expectedOutput = T.stripEnd (caseExpectedOutput meta)
      expectedExit = caseExpectedExitCode meta
      actualExit = exitCodeToInt (cliExitCode result)
      outputMatch = actualOutput == expectedOutput
      exitMatch = actualExit == expectedExit
      success = outputMatch && exitMatch
  pure $ case caseStatus meta of
    StatusPass
      | success -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "expected pass but got mismatch"
              <> detailsSuffix actualOutput expectedOutput actualExit expectedExit
          )
    StatusXFail
      | success -> (OutcomeFail, "expected xfail (known failing bug), but test now passes")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | success -> (OutcomeXPass, "known bug still passes unexpectedly")
      | otherwise -> (OutcomeFail, "expected xpass (known passing bug), but test now fails")

-- | CLI result with exit code, stdout, and stderr.
data CLIResult = CLIResult
  { cliExitCode :: !ExitCode,
    cliStdout :: !Text,
    cliStderr :: !Text
  }

-- | Run a CLI tool in-process with full argument parsing.
-- This calls the pure CLI functions directly without IO.
runCLIInProcess :: CLITool -> [String] -> Text -> CLIResult
runCLIInProcess tool args input =
  case tool of
    ToolLexer ->
      let r = LexerRun.runLexer args input
       in CLIResult (LexerRun.cliExitCode r) (LexerRun.cliStdout r) (LexerRun.cliStderr r)
    ToolParser ->
      let r = ParserRun.runParser args input
       in CLIResult (ParserRun.cliExitCode r) (ParserRun.cliStdout r) (ParserRun.cliStderr r)

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

progressSummary :: [(CLICase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

detailsSuffix :: Text -> Text -> Int -> Int -> String
detailsSuffix actualOut expectedOut actualExit expectedExit =
  "\n  expected output:\n"
    <> T.unpack expectedOut
    <> "\n  actual output:\n"
    <> T.unpack actualOut
    <> "\n  expected exit: "
    <> show expectedExit
    <> "\n  actual exit: "
    <> show actualExit

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
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

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
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

dropRootPrefix :: FilePath -> FilePath -> FilePath
dropRootPrefix root path =
  maybe path T.unpack (T.stripPrefix (T.pack (root <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "cli"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
