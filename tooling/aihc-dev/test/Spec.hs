{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Dev.Snippet
  ( ParseComparison (..),
    SnippetReport (..),
    buildSnippetReport,
    parseExtensionSettingArg,
    renderSnippetReport,
    renderSnippetReportColored,
  )
import Aihc.Parser.Syntax (Extension (TypeApplications), ExtensionSetting (..))
import Test.ExtractHiCompare (extractHiCompareTests)
import Test.GoldenUpdate (goldenUpdateTests)
import Test.ParserCLI.Suite (cliTests)
import Test.ResolvePackage (resolvePackageTests)
import Test.ResolveStackageProgress.PathsModule (resolveStackagePathsModuleTests)
import Test.StackageProgress.FileChecker (stackageProgressFileCheckerTests)
import Test.StackageProgress.FileCheckerTiming (stackageProgressFileCheckerTimingTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  cliTestsTree <- cliTests
  defaultMain . testGroup "aihc-dev" $
    [ testCase "reports both parsers rejecting a snippet" $ do
        let report = buildSnippetReport BothReject Nothing Nothing Nothing
        assertEqual
          "message"
          "GHC Parses: Parse failure\nAIHC Parses: Parse failure (Expected)\nGHC AST Match: Skipped\nMinimal Parentheses: Skipped\n"
          (renderSnippetReport report)
        assertBool "failure" (reportHasFailure report),
      testCase "reports parser mismatch when aihc rejects valid GHC snippet" $ do
        let report = buildSnippetReport GhcAcceptsAihcRejects Nothing Nothing Nothing
        assertEqual
          "message"
          "GHC Parses: OK\nAIHC Parses: Parse failure (Bug: GHC/AIHC mismatch)\nGHC AST Match: Skipped\nMinimal Parentheses: Skipped\n"
          (renderSnippetReport report)
        assertBool "failure" (reportHasFailure report),
      testCase "includes GHC AST conversion mismatch" $ do
        let report = buildSnippetReport BothAccept (Just "Bug found:\nDeclaration 1 differs") Nothing Nothing
        assertEqual
          "message"
          "GHC Parses: OK\nAIHC Parses: OK\nGHC AST Match: Bug found:\nDeclaration 1 differs\nMinimal Parentheses: OK\n"
          (renderSnippetReport report)
        assertBool "failure" (reportHasFailure report),
      testCase "includes roundtrip failures" $ do
        let report = buildSnippetReport BothAccept Nothing (Just "Roundtrip mismatch") Nothing
        assertEqual
          "message"
          "GHC Parses: OK\nAIHC Parses: OK\nGHC AST Match: OK\nRoundtrip: Bug found:\nRoundtrip mismatch\nMinimal Parentheses: OK\n"
          (renderSnippetReport report)
        assertBool "failure" (reportHasFailure report),
      testCase "includes parens diff" $ do
        let report = buildSnippetReport BothAccept Nothing Nothing (Just "Bug found:\nChanged section:\n@@ line 1 @@\n- a\n+ (a)\n")
        assertEqual
          "message"
          "GHC Parses: OK\nAIHC Parses: OK\nGHC AST Match: OK\nMinimal Parentheses: Bug found:\nChanged section:\n@@ line 1 @@\n- a\n+ (a)\n"
          (renderSnippetReport report)
        assertBool "failure" (reportHasFailure report),
      testCase "colors status lines when requested" $ do
        let report = buildSnippetReport GhcAcceptsAihcRejects Nothing Nothing Nothing
        assertEqual
          "message"
          "\ESC[32mGHC Parses: OK\ESC[0m\n\ESC[31mAIHC Parses: Parse failure (Bug: GHC/AIHC mismatch)\ESC[0m\n\ESC[32mGHC AST Match: Skipped\ESC[0m\n\ESC[32mMinimal Parentheses: Skipped\ESC[0m\n"
          (renderSnippetReportColored True report),
      testCase "parses -X extension arguments" $ do
        assertEqual "extension" (Right (EnableExtension TypeApplications)) (parseExtensionSettingArg "TypeApplications"),
      QC.testProperty "dummy quickcheck property" prop_dummy,
      extractHiCompareTests,
      resolvePackageTests,
      goldenUpdateTests,
      resolveStackagePathsModuleTests,
      stackageProgressFileCheckerTests,
      stackageProgressFileCheckerTimingTests,
      cliTestsTree
    ]

prop_dummy :: Bool
prop_dummy = True
