{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Dev.Snippet
  ( ParseComparison (..),
    SnippetReport (..),
    buildSnippetReport,
    parseExtensionSettingArg,
    renderSnippetReport,
  )
import Aihc.Parser.Syntax (Extension (TypeApplications), ExtensionSetting (..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc-dev" $
    [ testCase "reports both parsers rejecting a snippet" $ do
        let report = buildSnippetReport BothReject Nothing False
        assertEqual "message" "Snippet fails to parse with both GHC and aihc-parser.\n" (renderSnippetReport report)
        assertBool "not a bug" (not (reportHasBug report)),
      testCase "reports parser mismatch when aihc rejects valid GHC snippet" $ do
        let report = buildSnippetReport GhcAcceptsAihcRejects Nothing False
        assertEqual "message" "Bug found: code rejected by aihc-parser but parsed by GHC.\n" (renderSnippetReport report)
        assertBool "bug" (reportHasBug report),
      testCase "includes roundtrip failures" $ do
        let report = buildSnippetReport BothAccept (Just "Roundtrip mismatch") False
        assertEqual "message" "Roundtrip mismatch\n" (renderSnippetReport report)
        assertBool "bug" (reportHasBug report),
      testCase "includes parens bug notice" $ do
        let report = buildSnippetReport BothAccept Nothing True
        assertEqual "message" "Bug found: Parens.addModuleParens adds parentheses to the parsed snippet.\n" (renderSnippetReport report)
        assertBool "bug" (reportHasBug report),
      testCase "parses -X extension arguments" $ do
        assertEqual "extension" (Right (EnableExtension TypeApplications)) (parseExtensionSettingArg "TypeApplications"),
      QC.testProperty "dummy quickcheck property" prop_dummy
    ]

prop_dummy :: Bool
prop_dummy = True
