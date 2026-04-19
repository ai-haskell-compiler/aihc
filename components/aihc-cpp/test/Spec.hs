{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Config (..), Result (..), Step (..), defaultConfig, preprocess)
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  defaultMain
    ( testGroup
        "cpp-oracle"
        ( checks
            <> [linePragmaTest, dateTimeTest, functionMacroArgumentTest, functionMacroUnclosedCallTest, definedConditionSpacingTest, ccallMacroConcatTest]
            <> [QC.testProperty "dummy quickcheck property" prop_dummy]
        )
    )

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

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
            input = TE.encodeUtf8 "__DATE__ __TIME__"
        case preprocess cfg input of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\"Mar 15 2026\" \"12:00:00\"\n"
          _ -> assertFailure "expected Done",
      testCase "defaults to unix epoch" $ do
        let cfg = defaultConfig
            input = TE.encodeUtf8 "__DATE__ __TIME__"
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
    case preprocess defaultConfig {configInputFile = "root.hs"} (TE.encodeUtf8 "before\n#include \"nested.inc\"\nafter") of
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
    case preprocess defaultConfig (TE.encodeUtf8 "#define PAIR(x,y) x + y\nPAIR((1 + 2), 3)") of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\n(1 + 2) + 3\n"
      _ -> assertFailure "expected Done"

functionMacroUnclosedCallTest :: TestTree
functionMacroUnclosedCallTest =
  testCase "unterminated function-like call does not expand macro" $
    case preprocess defaultConfig (TE.encodeUtf8 "#define ID() replaced\nID(") of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\nID(\n"
      _ -> assertFailure "expected Done"

definedConditionSpacingTest :: TestTree
definedConditionSpacingTest =
  testCase "defined handles whitespace around parenthesized name" $
    case preprocess defaultConfig (TE.encodeUtf8 "#define FLAG 1\n#if defined   ( FLAG )\nok\n#else\nbad\n#endif") of
      Done result ->
        if "ok\n" `T.isInfixOf` resultOutput result && not ("bad\n" `T.isInfixOf` resultOutput result)
          then pure ()
          else assertFailure ("expected ok branch to be active, output was: " <> show (resultOutput result))
      _ -> assertFailure "expected Done"

ccallMacroConcatTest :: TestTree
ccallMacroConcatTest =
  knownFailureTest "CCALL macro expands stringizing and token pasting" "token pasting with ## is not supported yet" $
    case preprocess defaultConfig (TE.encodeUtf8 ccallMacroInput) of
      Done result ->
        resultOutput result @?= ccallMacroExpectedOutput
      _ -> assertFailure "expected Done"

ccallMacroInput :: T.Text
ccallMacroInput =
  T.unlines
    [ "#define CCALL(name,signature) \\",
      "foreign import ccall unsafe #name \\",
      "    c_##name :: signature",
      "",
      "CCALL(foo, Int -> IO Int)"
    ]

ccallMacroExpectedOutput :: T.Text
ccallMacroExpectedOutput =
  T.unlines
    [ "#line 1 \"<input>\"",
      "",
      "foreign import ccall unsafe \"foo\"",
      "    c_foo :: Int -> IO Int"
    ]

knownFailureTest :: String -> String -> Assertion -> TestTree
knownFailureTest name reason assertion = singleTest name (KnownFailureTest reason assertion)

data KnownFailureTest = KnownFailureTest String Assertion

instance IsTest KnownFailureTest where
  run _ (KnownFailureTest reason assertion) _ = do
    result <- E.try assertion :: IO (Either E.SomeException ())
    case result of
      Left err -> pure (testPassed ("known failure: " <> reason <> "\n" <> E.displayException err))
      Right () -> pure (testFailed ("unexpected pass for known failure: " <> reason))
  testOptions = pure []
