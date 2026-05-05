{-# LANGUAGE OverloadedStrings #-}

module Test.Fmt.Golden
  ( goldenTests,
  )
where

import Aihc.Fmt
import Aihc.Parser.Syntax (ExtensionSetting, parseExtensionSettingName)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (forM)
import Data.Aeson (FromJSON, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.Foldable (for_)
import Data.List (dropWhileEnd, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, findExecutable, getTemporaryDirectory, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO (Handle, hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

data ExpectedStatus
  = StatusPass
  | StatusXFail
  deriving (Eq, Show)

data FormatterExpectation = FormatterExpectation
  { expectationStatus :: !ExpectedStatus,
    expectationOutput :: !Text,
    expectationReason :: !String
  }
  deriving (Eq, Show)

data FormatterCase = FormatterCase
  { caseId :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![ExtensionSetting],
    caseInput :: !Text,
    caseFormatters :: !(Map Text FormatterExpectation)
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/fmt"

goldenTests :: IO TestTree
goldenTests = do
  cases <- loadCases
  aihcTests <- mapM mkAihcTest cases
  externalTests <- mapM mkExternalTests cases
  pure $
    testGroup
      "fmt-golden"
      [ testGroup "aihc-fmt" aihcTests,
        testGroup "external" (concat externalTests)
      ]

mkAihcTest :: FormatterCase -> IO TestTree
mkAihcTest meta = pure $
  case expectationFor "aihc-fmt" meta of
    Nothing -> testCase (caseId meta) (assertFailure "missing aihc-fmt expectation")
    Just expected ->
      case expectationStatus expected of
        StatusPass -> testCase (caseId meta) (assertAihcCase meta expected)
        StatusXFail -> testCaseInfo (caseId meta) (xfailDetails (evaluateAihc meta expected))

mkExternalTests :: FormatterCase -> IO [TestTree]
mkExternalTests meta =
  forM ["ormolu", "hindent", "stylish-haskell"] $ \name ->
    case expectationFor name meta of
      Nothing -> pure $ testCase (caseId meta <> "/" <> T.unpack name) (pure ())
      Just expected ->
        mkExternalTest name meta expected

mkExternalTest :: Text -> FormatterCase -> FormatterExpectation -> IO TestTree
mkExternalTest name meta expected = do
  mExe <- findExecutable (T.unpack name)
  case mExe of
    Nothing ->
      pure $
        testCaseInfo
          (caseId meta <> "/" <> T.unpack name)
          (pure ("skipped: " <> T.unpack name <> " is not on PATH"))
    Just exe ->
      pure $
        testCase (caseId meta <> "/" <> T.unpack name) $
          assertExternalCase exe name meta expected

assertAihcCase :: FormatterCase -> FormatterExpectation -> Assertion
assertAihcCase meta expected =
  for_ (evaluateAihc meta expected) assertFailure

xfailDetails :: Maybe String -> IO String
xfailDetails result =
  case result of
    Nothing -> assertFailure "expected xfail case to keep failing"
    Just details -> pure details

evaluateAihc :: FormatterCase -> FormatterExpectation -> Maybe String
evaluateAihc meta expected =
  case formatText opts (casePath meta) (caseInput meta) of
    Left err ->
      Just ("aihc-fmt failed: " <> T.unpack (formatErrorMessage err))
    Right actual
      | actual == expectationOutput expected -> Nothing
      | expectationStatus expected == StatusXFail ->
          Just ("known formatter gap: " <> expectationReason expected)
      | otherwise ->
          Just (mismatchDetails (expectationOutput expected) actual)
  where
    opts = defaultFormatOptions {formatExtensions = caseExtensions meta}

assertExternalCase :: FilePath -> Text -> FormatterCase -> FormatterExpectation -> Assertion
assertExternalCase exe name meta expected = do
  (exitCode, stdout, stderr, formattedFile) <-
    bracket
      acquireTempFile
      releaseTempFile
      ( \(tmpPath, handle) -> do
          TIO.hPutStr handle (caseInput meta)
          hClose handle
          (exitCode, stdout, stderr) <- readProcessWithExitCode exe [tmpPath] ""
          formattedFile <- TIO.readFile tmpPath
          pure (exitCode, stdout, stderr, formattedFile)
      )
  let actual =
        if null stdout
          then formattedFile
          else T.pack stdout
  case (exitCode, expectationStatus expected) of
    (ExitSuccess, StatusPass)
      | actual == expectationOutput expected -> pure ()
      | otherwise -> assertFailure (mismatchDetails (expectationOutput expected) actual)
    (ExitSuccess, StatusXFail)
      | actual == expectationOutput expected ->
          assertFailure ("expected " <> T.unpack name <> " xfail to keep failing")
      | otherwise -> pure ()
    (ExitFailure _, StatusXFail) -> pure ()
    (ExitFailure _, StatusPass) ->
      assertFailure (T.unpack name <> " failed:\n" <> stderr)

acquireTempFile :: IO (FilePath, Handle)
acquireTempFile = do
  tmpDir <- getTemporaryDirectory
  openTempFile tmpDir "aihc-fmt-fixture.hs"

releaseTempFile :: (FilePath, Handle) -> IO ()
releaseTempFile (tmpPath, handle) = do
  hClose handle `catch` ignoreIOException
  removeFile tmpPath `catch` ignoreIOException

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()

loadCases :: IO [FormatterCase]
loadCases = do
  paths <- listFixtureFiles fixtureRoot
  mapM loadCase paths

loadCase :: FilePath -> IO FormatterCase
loadCase path = do
  source <- TIO.readFile path
  case parseCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseCaseText :: FilePath -> Text -> Either String FormatterCase
parseCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, formatters) <-
    case parseEither parseYaml value of
      Left err -> Left ("Invalid formatter fixture schema in " <> path <> ": " <> err)
      Right parsed -> Right parsed
  exts <- traverse (parseExtension path) extNames
  pure
    FormatterCase
      { caseId = dropRootPrefix path,
        casePath = path,
        caseExtensions = exts,
        caseInput = inputText,
        caseFormatters = formatters
      }

parseYaml :: Y.Value -> Parser ([Text], Text, Map Text FormatterExpectation)
parseYaml =
  withObject "formatter fixture" $ \obj ->
    (,,)
      <$> obj .: "extensions"
      <*> obj .: "input"
      <*> obj .: "formatters"

instance FromJSON FormatterExpectation where
  parseJSON =
    withObject "formatter expectation" $ \obj -> do
      rawStatus <- obj .: "status"
      FormatterExpectation
        <$> parseStatus rawStatus
        <*> obj .: "output"
        <*> obj .:? "reason" .!= ""

parseStatus :: Text -> Y.Parser ExpectedStatus
parseStatus raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> pure StatusPass
    "xfail" -> pure StatusXFail
    _ -> fail ("invalid status: " <> T.unpack raw)

parseExtension :: FilePath -> Text -> Either String ExtensionSetting
parseExtension path raw =
  case parseExtensionSettingName raw of
    Just ext -> Right ext
    Nothing -> Left ("Invalid extension in " <> path <> ": " <> T.unpack raw)

expectationFor :: Text -> FormatterCase -> Maybe FormatterExpectation
expectationFor name meta = M.lookup name (caseFormatters meta)

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

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

mismatchDetails :: Text -> Text -> String
mismatchDetails expected actual =
  "output mismatch\nexpected:\n"
    <> T.unpack expected
    <> "\nactual:\n"
    <> T.unpack actual

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
