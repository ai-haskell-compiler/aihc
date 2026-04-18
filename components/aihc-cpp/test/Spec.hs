{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Config (..), Result (..), Step (..), defaultConfig, preprocess)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Golden (cppGoldenTests)
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  golden <- cppGoldenTests
  defaultMain
    ( testGroup
        "cpp"
        [ testGroup
            "cpp-oracle"
            ( checks
                <> [linePragmaTest]
                <> [QC.testProperty "dummy quickcheck property" prop_dummy]
            ),
          golden
        ]
    )

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

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
