{-# LANGUAGE OverloadedStrings #-}

-- | CLI binary tests using golden test fixtures.
--
-- These tests spawn the actual aihc-lexer and aihc-parser executables
-- and verify their output against expected golden outputs.
--
-- The executables must be available via environment variables:
-- - AIHC_LEXER_EXE: path to the aihc-lexer executable
-- - AIHC_PARSER_EXE: path to the aihc-parser executable
--
-- If these are not set, the tests will look for the executables in PATH.
-- If not found anywhere, the tests will fail (not skip).
module Test.CLI.Suite
  ( cliTests,
  )
where

import qualified CLIGolden as CG
import Control.Monad (forM, unless)
import Data.List (intercalate)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

cliTests :: IO TestTree
cliTests = do
  -- Load golden test fixtures
  lexerCases <- CG.loadLexerCLICases
  parserCases <- CG.loadParserCLICases

  -- Build test trees
  lexerTests <- mkCLIGoldenTests "aihc-lexer" "AIHC_LEXER_EXE" lexerCases
  parserTests <- mkCLIGoldenTests "aihc-parser" "AIHC_PARSER_EXE" parserCases

  pure $
    testGroup
      "CLI"
      [ testGroup "aihc-lexer" lexerTests,
        testGroup "aihc-parser" parserTests
      ]

-- | Build golden tests for a CLI tool.
-- Looks for the executable first in the given environment variable,
-- then in PATH. If not found, the test fails.
mkCLIGoldenTests :: String -> String -> [CG.CLICase] -> IO [TestTree]
mkCLIGoldenTests toolName envVar cases = do
  if null cases
    then pure [testCase "no fixtures found" (assertFailure "no CLI test fixtures found")]
    else do
      -- First check environment variable, then PATH
      mEnvPath <- lookupEnv envVar
      mPathExe <- findExecutable toolName
      case mEnvPath of
        Just envPath -> mkTests envPath cases
        Nothing ->
          case mPathExe of
            Just pathExe -> mkTests pathExe cases
            Nothing ->
              pure
                [ testCase
                    "executable not found"
                    (assertFailure $ "Could not find " <> toolName <> " executable. Set " <> envVar <> " or ensure " <> toolName <> " is in PATH.")
                ]
  where
    mkTests exePath fixtures = do
      -- Create a test case for each fixture
      tests <- forM fixtures $ \cliCase -> do
        let testName = CG.caseId cliCase
        pure $
          testCase testName $
            runCLIGoldenTest exePath cliCase
      -- Add a summary test
      let summaryTest = testCase "summary" (runSummaryTest exePath fixtures)
      pure (tests <> [summaryTest])

-- | Run a single CLI golden test.
runCLIGoldenTest :: FilePath -> CG.CLICase -> IO ()
runCLIGoldenTest exePath cliCase = do
  (outcome, detail) <- CG.evaluateCLICase exePath cliCase
  case outcome of
    CG.OutcomePass -> pure ()
    CG.OutcomeXFail -> pure () -- Expected failure
    CG.OutcomeXPass -> pure () -- Known bug that passes (acceptable)
    CG.OutcomeFail ->
      assertFailure $
        "CLI test failed for " <> CG.caseId cliCase <> ": " <> detail

-- | Run all tests and produce a summary.
runSummaryTest :: FilePath -> [CG.CLICase] -> IO ()
runSummaryTest exePath cases = do
  results <- forM cases $ \cliCase -> do
    (outcome, detail) <- CG.evaluateCLICase exePath cliCase
    pure (cliCase, outcome, detail)
  let (pass, xfail, xpass, _failures) = CG.progressSummary results
      total = length cases
      failList = [(c, d) | (c, CG.OutcomeFail, d) <- results]
  unless (null failList) $ do
    let failMessages =
          ["  - " <> CG.caseId c <> ": " <> d | (c, d) <- failList]
    assertFailure $
      "CLI golden test summary: "
        <> show pass
        <> " pass, "
        <> show xfail
        <> " xfail, "
        <> show xpass
        <> " xpass, "
        <> show (length failList)
        <> " fail out of "
        <> show total
        <> "\n\nFailures:\n"
        <> intercalate "\n" failMessages
