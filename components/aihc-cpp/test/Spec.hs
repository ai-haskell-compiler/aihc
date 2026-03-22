{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Config (..), Result (..), Step (..), defaultConfig, preprocess)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  defaultMain
    ( testGroup
        "cpp-oracle"
        (checks <> [linePragmaTest, dateTimeTest, functionMacroArgumentTest, functionMacroUnclosedCallTest, definedConditionSpacingTest])
    )

dateTimeTest :: TestTree
dateTimeTest =
  testGroup
    "__DATE__ and __TIME__"
    [ testCase "expands to provided values" $ do
        let cfg =
              defaultConfig
                { configMacros =
                    M.fromList
                      [ ("__DATE__", "\"Mar 15 2026\""),
                        ("__TIME__", "\"12:00:00\"")
                      ]
                }
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

functionMacroArgumentTest :: TestTree
functionMacroArgumentTest =
  testCase "function-like macro keeps nested argument text" $
    case preprocess defaultConfig "#define PAIR(x,y) x + y\nPAIR((1 + 2), 3)" of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\n(1 + 2) + 3\n"
      _ -> assertFailure "expected Done"

functionMacroUnclosedCallTest :: TestTree
functionMacroUnclosedCallTest =
  testCase "unterminated function-like call does not expand macro" $
    case preprocess defaultConfig "#define ID() replaced\nID(" of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\nID(\n"
      _ -> assertFailure "expected Done"

definedConditionSpacingTest :: TestTree
definedConditionSpacingTest =
  testCase "defined handles whitespace around parenthesized name" $
    case preprocess defaultConfig "#define FLAG 1\n#if defined   ( FLAG )\nok\n#else\nbad\n#endif" of
      Done result ->
        if "ok\n" `T.isInfixOf` resultOutput result && not ("bad\n" `T.isInfixOf` resultOutput result)
          then pure ()
          else assertFailure ("expected ok branch to be active, output was: " <> show (resultOutput result))
      _ -> assertFailure "expected Done"
