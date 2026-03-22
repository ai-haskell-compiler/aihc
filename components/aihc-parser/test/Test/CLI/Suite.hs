{-# LANGUAGE OverloadedStrings #-}

-- | CLI binary tests using golden test fixtures.
--
-- These tests spawn the actual aihc-lexer and aihc-parser executables
-- and verify their output against expected golden outputs.
module Test.CLI.Suite
  ( cliTests,
  )
where

import qualified CLIGolden as CG
import Control.Monad (forM, unless)
import Data.List (intercalate)
import System.Directory (findExecutable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

cliTests :: IO TestTree
cliTests = do
  -- Load golden test fixtures
  lexerCases <- CG.loadLexerCLICases
  parserCases <- CG.loadParserCLICases

  -- Build test trees
  lexerTests <- mkCLIGoldenTests "aihc-lexer" lexerCases
  parserTests <- mkCLIGoldenTests "aihc-parser" parserCases

  pure $
    testGroup
      "CLI"
      [ testGroup "aihc-lexer" lexerTests,
        testGroup "aihc-parser" parserTests
      ]

-- | Build golden tests for a CLI tool.
mkCLIGoldenTests :: String -> [CG.CLICase] -> IO [TestTree]
mkCLIGoldenTests toolName cases = do
  if null cases
    then pure [testCase "no fixtures found" (pure ())]
    else do
      -- Try to find the executable
      mExePath <- findExecutable toolName
      case mExePath of
        Nothing ->
          -- If not found in PATH, skip tests gracefully.
          -- This is expected when running via nix flake check since the
          -- executables aren't installed to PATH during the build.
          pure
            [ testCase
                "skipped (executable not in PATH)"
                (pure ())
            ]
        Just exePath -> do
          -- Create a test case for each fixture
          tests <- forM cases $ \cliCase -> do
            let testName = CG.caseId cliCase
            pure $
              testCase testName $
                runCLIGoldenTest exePath cliCase
          -- Add a summary test
          let summaryTest = testCase "summary" (runSummaryTest exePath cases)
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
