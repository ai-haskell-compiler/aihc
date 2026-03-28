{-# LANGUAGE OverloadedStrings #-}

-- | CLI binary tests using golden test fixtures.
--
-- These tests run the aihc-lexer and aihc-parser main functions in-process
-- using the 'silently' library to capture output. No external executables
-- are required.
module Test.CLI.Suite
  ( cliTests,
  )
where

import qualified CLIGolden as CG
import Control.Monad (forM, unless)
import Data.List (intercalate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

cliTests :: IO TestTree
cliTests = do
  -- Load golden test fixtures
  lexerCases <- CG.loadLexerCLICases
  parserCases <- CG.loadParserCLICases

  -- Build test trees
  let lexerTests = mkCLIGoldenTests CG.ToolLexer lexerCases
      parserTests = mkCLIGoldenTests CG.ToolParser parserCases

  pure $
    testGroup
      "CLI"
      [ testGroup "aihc-lexer" lexerTests,
        testGroup "aihc-parser" parserTests
      ]

-- | Build golden tests for a CLI tool.
-- Runs tests in-process, no external executables required.
mkCLIGoldenTests :: CG.CLITool -> [CG.CLICase] -> [TestTree]
mkCLIGoldenTests tool cases =
  if null cases
    then [testCase "no fixtures found" (assertFailure "no CLI test fixtures found")]
    else
      let tests = map (mkTest tool) cases
          summaryTest = testCase "summary" (runSummaryTest tool cases)
       in tests <> [summaryTest]
  where
    mkTest cliTool cliCase =
      testCase (CG.caseId cliCase) $
        runCLIGoldenTest cliTool cliCase

-- | Run a single CLI golden test.
runCLIGoldenTest :: CG.CLITool -> CG.CLICase -> IO ()
runCLIGoldenTest tool cliCase = do
  (outcome, detail) <- CG.evaluateCLICase tool cliCase
  case outcome of
    CG.OutcomePass -> pure ()
    CG.OutcomeXFail -> pure () -- Expected failure
    CG.OutcomeXPass -> pure () -- Known bug that passes (acceptable)
    CG.OutcomeFail ->
      assertFailure $
        "CLI test failed for " <> CG.caseId cliCase <> ": " <> detail

-- | Run all tests and produce a summary.
runSummaryTest :: CG.CLITool -> [CG.CLICase] -> IO ()
runSummaryTest tool cases = do
  results <- forM cases $ \cliCase -> do
    (outcome, detail) <- CG.evaluateCLICase tool cliCase
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
