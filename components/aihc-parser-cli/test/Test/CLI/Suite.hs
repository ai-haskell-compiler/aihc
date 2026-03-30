{-# LANGUAGE OverloadedStrings #-}

-- | CLI binary tests using golden test fixtures.
--
-- These tests run the unified aihc-parser CLI in-process. Lexer tests use the
-- @--lex@ flag to switch to lexer mode. No external executables are required.
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

  -- Build test trees (both use the unified aihc-parser CLI)
  let lexerTests = mkCLIGoldenTests lexerCases
      parserTests = mkCLIGoldenTests parserCases

  pure $
    testGroup
      "CLI"
      [ testGroup "lexer (--lex)" lexerTests,
        testGroup "parser" parserTests
      ]

-- | Build golden tests for the CLI.
-- Runs tests in-process, no external executables required.
mkCLIGoldenTests :: [CG.CLICase] -> [TestTree]
mkCLIGoldenTests cases =
  if null cases
    then [testCase "no fixtures found" (assertFailure "no CLI test fixtures found")]
    else
      let tests = map mkTest cases
          summaryTest = testCase "summary" (runSummaryTest cases)
       in tests <> [summaryTest]
  where
    mkTest cliCase =
      testCase (CG.caseId cliCase) $
        runCLIGoldenTest cliCase

-- | Run a single CLI golden test.
runCLIGoldenTest :: CG.CLICase -> IO ()
runCLIGoldenTest cliCase = do
  (outcome, detail) <- CG.evaluateCLICase cliCase
  case outcome of
    CG.OutcomePass -> pure ()
    CG.OutcomeXFail -> pure () -- Expected failure
    CG.OutcomeXPass -> pure () -- Known bug that passes (acceptable)
    CG.OutcomeFail ->
      assertFailure $
        "CLI test failed for " <> CG.caseId cliCase <> ": " <> detail

-- | Run all tests and produce a summary.
runSummaryTest :: [CG.CLICase] -> IO ()
runSummaryTest cases = do
  results <- forM cases $ \cliCase -> do
    (outcome, detail) <- CG.evaluateCLICase cliCase
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
