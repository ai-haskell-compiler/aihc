{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Cpp (Config (..), Result (..), Step (..), defaultConfig, preprocess)
import qualified Data.Text as T
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  defaultMain (testGroup "cpp-oracle" (checks <> [linePragmaTest, dateTimeTest, spliceModeTest]))

dateTimeTest :: TestTree
dateTimeTest =
  testGroup
    "__DATE__ and __TIME__"
    [ testCase "expands to provided values" $ do
        let cfg = defaultConfig {configDateTime = ("Mar 15 2026", "12:00:00")}
            input = "__DATE__ __TIME__"
        case preprocess cfg input of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\"Mar 15 2026\" \"12:00:00\"\n"
          _ -> assertFailure "expected Done",
      testCase "defaults to unix epoch" $ do
        let cfg = defaultConfig
            input = "__DATE__ __TIME__"
        case preprocess cfg input of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\"Jan  1 1970\" \"00:00:00\"\n"
          _ -> assertFailure "expected Done"
    ]

(@?=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

mkCase :: CaseMeta -> IO TestTree
mkCase meta =
  pure $ testCase (caseId meta) (assertCase meta)

assertCase :: CaseMeta -> Assertion
assertCase meta = do
  (_, outcome, details) <- evaluateCase meta
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "cpp regression in "
            <> caseId meta
            <> " ["
            <> caseCategory meta
            <> "]: "
            <> details
        )
    _ -> pure ()

linePragmaTest :: TestTree
linePragmaTest =
  testCase "include emits line pragmas" $
    case preprocess defaultConfig {configInputFile = "root.hs"} "before\n#include \"nested.inc\"\nafter" of
      NeedInclude _ k ->
        case k (Just "inside") of
          Done result -> do
            let out = T.lines (resultOutput result)
                hasIncludePragma = any (T.isSuffixOf "nested.inc\"") out
            if hasIncludePragma && "#line 3 \"root.hs\"" `elem` out
              then pure ()
              else assertFailure "expected include line pragmas in output"
          NeedInclude {} -> assertFailure "unexpected nested include in line pragma test"
      Done _ -> assertFailure "expected include continuation step"

spliceModeTest :: TestTree
spliceModeTest =
  testGroup
    "string-gap splice mode"
    [ testCase "disabled by default" $ do
        let input =
              T.unlines
                [ "message =",
                  "  \"one\\n\\\\",
                  "  \\two\\n\\\\",
                  "  \\\""
                ]
        case preprocess defaultConfig input of
          Done result ->
            if "  \\two\\n\\\\" `T.isInfixOf` resultOutput result
              then pure ()
              else assertFailure "expected continuation line to remain when splice mode is disabled"
          _ -> assertFailure "expected Done",
      testCase "enabled for parser compatibility" $ do
        let cfg = defaultConfig {configSpliceStringGapContinuations = True}
            input =
              T.unlines
                [ "message =",
                  "  \"one\\n\\\\",
                  "  \\two\\n\\\\",
                  "  \\\""
                ]
        case preprocess cfg input of
          Done result ->
            when
              ("  \\two\\n\\\\" `T.isInfixOf` resultOutput result)
              (assertFailure "expected continuation line to be spliced when mode is enabled")
          _ -> assertFailure "expected Done"
    ]
