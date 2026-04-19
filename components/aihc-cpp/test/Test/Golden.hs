{-# LANGUAGE OverloadedStrings #-}

module Test.Golden
  ( cppGoldenTests,
  )
where

import Aihc.Cpp
  ( Config (..),
    Diagnostic (..),
    Result (..),
    Severity (..),
    defaultConfig,
  )
import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Paths_aihc_cpp (getDataFileName)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeBaseName, takeDirectory, takeExtension, (</>))
import Test.Runner (runPreprocessFromFile, runPreprocessFromText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

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

data DiagnosticExpectation = DiagnosticExpectation
  { expectedSeverity :: !Severity,
    expectedMessage :: !Text,
    expectedFile :: !FilePath,
    expectedLine :: !Int
  }
  deriving (Eq, Show)

data GoldenCase = GoldenCase
  { caseId :: !String,
    caseCategory :: !String,
    caseActualInput :: !(Either FilePath Text),
    caseConfig :: !Config,
    caseExpectedOutput :: !(Either FilePath Text),
    caseExpectedDiagnostics :: ![DiagnosticExpectation],
    caseExpectError :: !Bool,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }

data GoldenSpec = GoldenSpec
  { specInput :: !(Either FilePath Text),
    specInputFile :: !(Maybe FilePath),
    specOutput :: !(Either FilePath Text),
    specDiagnostics :: !(Maybe FilePath),
    specExpectError :: !Bool,
    specStatus :: !ExpectedStatus,
    specReason :: !String,
    specMacros :: !(M.Map Text Text)
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

cppGoldenTests :: IO TestTree
cppGoldenTests = do
  cases <- loadGoldenCases
  let checks = map mkGoldenTest cases
  pure
    ( testGroup
        "cpp-golden"
        ([fixtureValidationTests] <> checks <> [testCase "summary" (assertNoRegressions cases)])
    )

mkGoldenTest :: GoldenCase -> TestTree
mkGoldenTest gcase =
  testCase (caseId gcase) $ do
    (_, outcome, details) <- evaluateCase gcase
    case outcome of
      OutcomePass -> pure ()
      OutcomeXFail -> pure ()
      OutcomeFail ->
        assertFailure
          ( "CPP golden regression in "
              <> caseId gcase
              <> " ("
              <> caseCategory gcase
              <> ") expected "
              <> show (caseStatus gcase)
              <> " reason="
              <> caseReason gcase
              <> " details="
              <> details
          )
      OutcomeXPass ->
        assertFailure
          ( "Unexpected pass in xfail cpp golden case "
              <> caseId gcase
              <> " reason="
              <> caseReason gcase
              <> " details="
              <> details
          )

evaluateCase :: GoldenCase -> IO (GoldenCase, Outcome, String)
evaluateCase gcase = do
  result <-
    case caseActualInput gcase of
      Left path -> runPreprocessFromFile (caseConfig gcase) path
      Right text -> runPreprocessFromText (caseConfig gcase) text
  let (outcome, details) = classify gcase result
  pure (gcase, outcome, details)

classify :: GoldenCase -> Result -> (Outcome, String)
classify gcase result =
  case caseStatus gcase of
    StatusPass
      | null mismatches -> (OutcomePass, "")
      | otherwise -> (OutcomeFail, renderMismatches mismatches)
    StatusFail
      | not hasError -> (OutcomeFail, "expected failure but preprocessing succeeded")
      | null mismatches -> (OutcomePass, "")
      | otherwise -> (OutcomeFail, renderMismatches mismatches)
    StatusXFail
      | null mismatches -> (OutcomeXPass, "known failing golden case now matches expected output")
      | otherwise -> (OutcomeXFail, renderMismatches mismatches)
  where
    mismatches = compareResult gcase result
    hasError = any ((== Error) . diagSeverity) (resultDiagnostics result)

compareResult :: GoldenCase -> Result -> [String]
compareResult gcase result =
  outputMismatch <> diagnosticsMismatch <> errorMismatch
  where
    outputMismatch =
      case caseExpectedOutput gcase of
        Left _path -> []
        Right expectedOutput
          | T.stripEnd (resultOutput result) == T.stripEnd expectedOutput -> []
          | otherwise ->
              [ "output mismatch\nexpected: "
                  <> show (T.unpack expectedOutput)
                  <> "\nactual:   "
                  <> show (T.unpack (resultOutput result))
              ]
    diagnosticsMismatch
      | actualDiagnostics == caseExpectedDiagnostics gcase = []
      | otherwise =
          [ "diagnostics mismatch\nexpected: "
              <> show (caseExpectedDiagnostics gcase)
              <> "\nactual:   "
              <> show actualDiagnostics
          ]
    errorMismatch
      | caseExpectError gcase && not hasError = ["expected at least one error diagnostic"]
      | otherwise = []
    actualDiagnostics = map toExpectation (resultDiagnostics result)
    hasError = any ((== Error) . diagSeverity) (resultDiagnostics result)

toExpectation :: Diagnostic -> DiagnosticExpectation
toExpectation diag =
  DiagnosticExpectation
    { expectedSeverity = diagSeverity diag,
      expectedMessage = diagMessage diag,
      expectedFile = diagFile diag,
      expectedLine = diagLine diag
    }

renderMismatches :: [String] -> String
renderMismatches = unlines

assertNoRegressions :: [GoldenCase] -> Assertion
assertNoRegressions cases = do
  outcomes <- mapM evaluateCase cases
  let (passN, xfailN, xpassN, failN) = progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when
    (failN > 0 || xpassN > 0)
    ( assertFailure
        ( "cpp golden regressions found. "
            <> "pass="
            <> show passN
            <> " xfail="
            <> show xfailN
            <> " xpass="
            <> show xpassN
            <> " fail="
            <> show failN
            <> " completion="
            <> show completion
            <> "%"
        )
    )

progressSummary :: [(GoldenCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, outcome, _) <- outcomes, outcome == wanted]

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

loadGoldenCases :: IO [GoldenCase]
loadGoldenCases = do
  fixtureRootAbs <- getDataFileName fixtureRoot
  exists <- doesDirectoryExist fixtureRootAbs
  if not exists
    then pure []
    else do
      paths <- listCaseFiles fixtureRootAbs
      mapM (loadGoldenCase fixtureRootAbs) paths

loadGoldenCase :: FilePath -> FilePath -> IO GoldenCase
loadGoldenCase fixtureRootAbs caseFile = do
  raw <- Y.decodeFileEither caseFile
  value <-
    either
      (\err -> fail ("Invalid YAML fixture " <> caseFile <> ": " <> Y.prettyPrintParseException err))
      pure
      raw
  spec <- either fail pure (parseGoldenSpec caseFile value)
  let caseDirectory = takeDirectory caseFile
      relDir = makeRelative fixtureRootAbs caseDirectory
      actualInput = specInput spec
      displayInputPath = fromMaybe "input.hs" (specInputFile spec)
      config =
        defaultConfig
          { configInputFile = displayInputPath,
            configMacros = M.union (specMacros spec) (configMacros defaultConfig)
          }
  either
    ( \path -> do
        exists <- doesFileExist (caseDirectory </> path)
        unless exists (fail ("Golden fixture input is missing: " <> path))
    )
    (\_ -> pure ())
    actualInput
  expectedOutput <-
    case specOutput spec of
      Left path -> do
        exists <- doesFileExist (caseDirectory </> path)
        unless exists (fail ("Golden fixture output is missing: " <> path))
        content <- TIO.readFile (caseDirectory </> path)
        pure (Right content)
      Right inlineContent -> pure (Right inlineContent)
  expectedDiagnostics <- maybe (pure []) (loadExpectedDiagnostics caseDirectory) (specDiagnostics spec)
  validateGoldenCase caseFile spec expectedOutput expectedDiagnostics
  pure
    GoldenCase
      { caseId = relDir,
        caseCategory = categoryFromPath relDir,
        caseActualInput = actualInput,
        caseConfig = config,
        caseExpectedOutput = expectedOutput,
        caseExpectedDiagnostics = expectedDiagnostics,
        caseExpectError = specExpectError spec,
        caseStatus = specStatus spec,
        caseReason = specReason spec
      }

parseGoldenSpec :: FilePath -> Y.Value -> Either String GoldenSpec
parseGoldenSpec path value = do
  (inputText, maybeInputFilePath, outputText, diagnosticsPath, expectError, statusText, reasonText, macros) <-
    parseEither
      ( withObject "cpp golden fixture" $ \obj -> do
          inputText <- obj .: "input"
          outputText <- obj .:? "output"
          diagnosticsPath <- obj .:? "diagnostics"
          expectError <- obj .:? "expect-error" .!= False
          statusText <- obj .: "status"
          reasonText <- obj .:? "reason" .!= ""
          configValues <- obj .:? "config" .!= Y.Object mempty
          (maybeInputFilePath, macros) <- parseConfigValues configValues
          pure (inputText, maybeInputFilePath, outputText, diagnosticsPath, expectError, statusText, reasonText, macros)
      )
      value
  status <- parseStatus path statusText
  let inputSpec = parseInlineOrFileText inputText
      outputSpec = maybe (Right "") parseInlineOrFileText outputText
  pure
    GoldenSpec
      { specInput = inputSpec,
        specInputFile = maybeInputFilePath,
        specOutput = outputSpec,
        specDiagnostics = diagnosticsPath,
        specExpectError = expectError,
        specStatus = status,
        specReason = trim (T.unpack reasonText),
        specMacros = macros
      }

parseInlineOrFileText :: Text -> Either FilePath Text
parseInlineOrFileText = Right

parseConfigValues :: Y.Value -> Y.Parser (Maybe FilePath, M.Map Text Text)
parseConfigValues = withObject "cpp golden config" parser
  where
    parser cfg = do
      inputFilePath <- cfg .:? "input-file"
      macros <- cfg .:? "macros" .!= M.empty
      pure (inputFilePath, macros)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xfail" -> Right StatusXFail
    "xpass" -> Left ("xpass is not allowed in " <> path <> ": use xfail instead")
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

loadExpectedDiagnostics :: FilePath -> FilePath -> IO [DiagnosticExpectation]
loadExpectedDiagnostics root relPath = do
  let fullPath = root </> relPath
  raw <- Y.decodeFileEither fullPath
  value <-
    either
      (\err -> fail ("Invalid diagnostics fixture " <> fullPath <> ": " <> Y.prettyPrintParseException err))
      pure
      raw
  either fail pure (parseDiagnostics fullPath value)

parseDiagnostics :: FilePath -> Y.Value -> Either String [DiagnosticExpectation]
parseDiagnostics path value =
  case parseEither parser value of
    Left err -> Left ("Invalid diagnostics schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed
  where
    parser = Y.withArray "cpp diagnostics fixture" $ \arr -> mapM parseDiagnostic (foldr (:) [] arr)
    parseDiagnostic = withObject "cpp diagnostic" $ \obj -> do
      severityText <- obj .: "severity"
      message <- obj .: "message"
      filePath <- obj .: "file"
      lineNo <- obj .: "line"
      severity <-
        case map toLower (trim (T.unpack severityText)) of
          "warning" -> pure Warning
          "error" -> pure Error
          _ -> fail ("invalid severity: " <> T.unpack severityText)
      pure
        DiagnosticExpectation
          { expectedSeverity = severity,
            expectedMessage = message,
            expectedFile = filePath,
            expectedLine = lineNo
          }

validateGoldenCase :: FilePath -> GoldenSpec -> Either FilePath Text -> [DiagnosticExpectation] -> IO ()
validateGoldenCase path spec expectedOutput expectedDiagnostics = do
  let hasOutput =
        case expectedOutput of
          Left _ -> True
          Right txt -> not (T.null (T.strip txt))
      hasDiagnostics = not (null expectedDiagnostics)
      expectsError = any ((== Error) . expectedSeverity) expectedDiagnostics
  case specStatus spec of
    StatusPass -> do
      if hasOutput
        then pure ()
        else fail ("[output] is required for pass status in " <> path)
      when (specExpectError spec) (fail ("pass status cannot use expect-error in " <> path))
    StatusFail ->
      if specExpectError spec || expectsError
        then pure ()
        else fail ("fail status requires expect-error or an error diagnostic in " <> path)
    StatusXFail -> do
      when (null (specReason spec)) (fail ("[reason] is required for xfail status in " <> path))
      if hasOutput || hasDiagnostics || specExpectError spec
        then pure ()
        else fail ("xfail status requires output, diagnostics, or expect-error in " <> path)

listCaseFiles :: FilePath -> IO [FilePath]
listCaseFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listCaseFiles path
            else
              if takeBaseName path == "case" && takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "golden"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

fixtureValidationTests :: TestTree
fixtureValidationTests =
  testGroup
    "fixture-parse"
    [ testCase "rejects xfail without reason" $
        case parseGoldenSpec "case.yaml" invalidXFailMissingReason of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec -> expectValidationFailure "missing xfail reason" (validateGoldenCase "case.yaml" spec (Right "ok") []),
      testCase "rejects pass without output" $
        case parseGoldenSpec "case.yaml" invalidPassMissingOutput of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec -> expectValidationFailure "missing pass output" (validateGoldenCase "case.yaml" spec (Right "") []),
      testCase "rejects fail without error expectation" $
        case parseGoldenSpec "case.yaml" invalidFailMissingError of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec -> expectValidationFailure "missing fail error expectation" (validateGoldenCase "case.yaml" spec (Right "") []),
      testCase "accepts explicit input-file override" $
        case parseGoldenSpec "case.yaml" validInputFileOverride of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec ->
            if specInputFile spec == Just "Test.hs"
              then pure ()
              else assertFailure "expected config.input-file override to be preserved",
      testCase "parses macros from config" $
        case parseGoldenSpec "case.yaml" validInputFileOverride of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec ->
            if M.lookup "__DATE__" (specMacros spec) == Just "\"Mar 15 2026\""
              then pure ()
              else assertFailure "expected config.macros to be preserved",
      testCase "preserves inline input whitespace" $
        case parseGoldenSpec "case.yaml" validInlineWhitespace of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right spec ->
            if specInput spec == Right "  foo\n"
              then pure ()
              else assertFailure "expected inline input text to be preserved exactly"
    ]

expectValidationFailure :: String -> IO () -> Assertion
expectValidationFailure label action = do
  result <- try action :: IO (Either IOError ())
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure ("expected validation failure for " <> label)

invalidXFailMissingReason :: Y.Value
invalidXFailMissingReason =
  Y.object
    [ ("input", Y.String "#define FOO 1"),
      ("output", Y.String ""),
      ("status", Y.String "xfail")
    ]

invalidPassMissingOutput :: Y.Value
invalidPassMissingOutput =
  Y.object
    [ ("input", Y.String "#define FOO 1"),
      ("output", Y.String ""),
      ("status", Y.String "pass")
    ]

invalidFailMissingError :: Y.Value
invalidFailMissingError =
  Y.object
    [ ("input", Y.String "#define FOO 1"),
      ("status", Y.String "fail")
    ]

validInputFileOverride :: Y.Value
validInputFileOverride =
  Y.Object
    ( KeyMap.fromList
        [ ("input", Y.String "#define FOO 1"),
          ("output", Y.String ""),
          ("status", Y.String "pass"),
          ( "config",
            Y.Object
              ( KeyMap.fromList
                  [ ("input-file", Y.String "Test.hs"),
                    ( "macros",
                      Y.Object
                        (KeyMap.fromList [("__DATE__", Y.String "\"Mar 15 2026\"")])
                    )
                  ]
              )
          )
        ]
    )

validInlineWhitespace :: Y.Value
validInlineWhitespace =
  Y.object
    [ ("input", Y.String "  foo\n"),
      ("output", Y.String "bar"),
      ("status", Y.String "pass")
    ]
