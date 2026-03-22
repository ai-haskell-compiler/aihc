{-# LANGUAGE OverloadedStrings #-}

-- | Golden test infrastructure for CLI binaries (aihc-lexer, aihc-parser).
--
-- This module provides process-based testing that actually spawns the
-- executables and verifies command-line argument parsing, stdin/file input,
-- and output format.
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

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Parser.Ast (Extension, parseExtensionName)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )

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

data CLITool
  = ToolLexer
  | ToolParser
  deriving (Eq, Show)

data CLICase = CLICase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseTool :: !CLITool,
    caseExtensions :: ![Extension],
    caseArgs :: ![String],
    caseInput :: !Text,
    caseExpectedOutput :: ![Text],
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
loadLexerCLICases = loadCases ToolLexer lexerFixtureRoot

loadParserCLICases :: IO [CLICase]
loadParserCLICases = loadCases ToolParser parserFixtureRoot

loadCases :: CLITool -> FilePath -> IO [CLICase]
loadCases tool root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles root
      mapM (loadCLICase tool root) paths

loadCLICase :: CLITool -> FilePath -> FilePath -> IO CLICase
loadCLICase tool root path = do
  source <- TIO.readFile path
  case parseCLICaseText tool root path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseCLICaseText :: CLITool -> FilePath -> FilePath -> Text -> Either String CLICase
parseCLICaseText tool root path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, extraArgs, inputText, expectedLines, exitCode, statusText, reasonText) <-
    parseYamlFixture path value
  exts <- mapM (parseFixtureExtensionName path) extNames
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix root path
      category = categoryFromPath relPath
  pure
    CLICase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseTool = tool,
        caseExtensions = exts,
        caseArgs = map T.unpack extraArgs,
        caseInput = inputText,
        caseExpectedOutput = expectedLines,
        caseExpectedExitCode = exitCode,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture ::
  FilePath ->
  Y.Value ->
  Either String ([Text], [Text], Text, [Text], Int, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "cli fixture" $ \obj -> do
        exts <- obj .:? "extensions" .!= []
        extraArgs <- obj .:? "args" .!= []
        inputText <- obj .: "input"
        expectedLines <- obj .: "output"
        exitCode <- obj .:? "exit_code" .!= 0
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, extraArgs, inputText, expectedLines, exitCode, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid CLI fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

-- | Run a CLI case and evaluate the outcome.
-- Returns (Outcome, detail message).
evaluateCLICase :: FilePath -> CLICase -> IO (Outcome, String)
evaluateCLICase exePath meta = do
  let extArgs = concatMap (\ext -> ["-X" <> show ext]) (caseExtensions meta)
      allArgs = extArgs <> caseArgs meta
  (exitCode, stdout, stderr) <- runProcess exePath allArgs (caseInput meta)
  let actualLines = filter (not . T.null) $ map T.strip $ T.lines stdout
      expectedLines = caseExpectedOutput meta
      expectedExit = caseExpectedExitCode meta
      actualExit = exitCodeToInt exitCode
      outputMatch = actualLines == expectedLines
      exitMatch = actualExit == expectedExit
      success = outputMatch && exitMatch
  pure $ case caseStatus meta of
    StatusPass
      | success -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "expected pass but got mismatch"
              <> detailsSuffix actualLines expectedLines actualExit expectedExit stderr
          )
    StatusXFail
      | success -> (OutcomeFail, "expected xfail (known failing bug), but test now passes")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | success -> (OutcomeXPass, "known bug still passes unexpectedly")
      | otherwise -> (OutcomeFail, "expected xpass (known passing bug), but test now fails")

runProcess :: FilePath -> [String] -> Text -> IO (ExitCode, Text, Text)
runProcess exe args input = do
  let cp =
        (proc exe args)
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }
  (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
  -- Write input
  TIO.hPutStr hIn input
  hFlush hIn
  hClose hIn
  -- Read output
  outBytes <- BS.hGetContents hOut
  errBytes <- BS.hGetContents hErr
  exitCode <- waitForProcess ph
  let stdout = T.pack (BS8.unpack outBytes)
      stderr = T.pack (BS8.unpack errBytes)
  pure (exitCode, stdout, stderr)

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

detailsSuffix :: [Text] -> [Text] -> Int -> Int -> Text -> String
detailsSuffix actualOut expectedOut actualExit expectedExit stderrOut =
  "\n  expected output: "
    <> show expectedOut
    <> "\n  actual output:   "
    <> show actualOut
    <> "\n  expected exit:   "
    <> show expectedExit
    <> "\n  actual exit:     "
    <> show actualExit
    <> (if T.null stderrOut then "" else "\n  stderr: " <> T.unpack stderrOut)

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

parseFixtureExtensionName :: FilePath -> Text -> Either String Extension
parseFixtureExtensionName path name =
  case parseExtensionName name of
    Just ext -> Right ext
    Nothing -> Left ("Unknown extension in " <> path <> ": " <> T.unpack name)

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
