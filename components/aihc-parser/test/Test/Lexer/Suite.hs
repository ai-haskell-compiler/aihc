{-# LANGUAGE OverloadedStrings #-}

module Test.Lexer.Suite
  ( lexerTests,
  )
where

import Control.Monad (unless, when)
import Data.Text qualified as T
import LexerGolden qualified as LG
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

lexerTests :: IO TestTree
lexerTests = do
  cases <- LG.loadLexerCases
  checks <- mapM mkCaseTest cases
  summary <- mkSummaryTest cases
  pure (testGroup "lexer-golden" ([fixtureValidationTests] <> checks <> [summary]))

mkCaseTest :: LG.LexerCase -> IO TestTree
mkCaseTest meta = pure $ case LG.caseStatus meta of
  LG.StatusXFail -> testCaseInfo (LG.caseId meta) (xfailDetails (LG.evaluateLexerCase meta) <* assertCase meta)
  _ -> testCase (LG.caseId meta) (assertCase meta)

xfailDetails :: (LG.Outcome, String) -> IO String
xfailDetails (outcome, details) = do
  case outcome of
    LG.OutcomeXFail -> pure ()
    _ -> assertFailure ("expected xfail outcome, got: " <> show outcome)
  pure details

mkSummaryTest :: [LG.LexerCase] -> IO TestTree
mkSummaryTest cases = do
  let outcomes = map evaluate cases
  pure $
    testCase "summary" $ do
      let (passN, xfailN, xpassN, failN) = LG.progressSummary outcomes
          totalN = passN + xfailN + xpassN + failN
          completion = pct passN totalN
      when (failN > 0 || xpassN > 0) $
        assertFailure
          ( "lexer golden regressions found. "
              <> "pass="
              <> show passN
              <> " xfail="
              <> show xfailN
              <> " xpass="
              <> show xpassN
              <> " fail="
              <> show failN
              <> " completion="
              <> show completion
              <> "%"
          )

assertCase :: LG.LexerCase -> Assertion
assertCase meta =
  case LG.evaluateLexerCase meta of
    (LG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in lexer case "
            <> LG.caseId meta
            <> " ("
            <> LG.caseCategory meta
            <> ") expected "
            <> show (LG.caseStatus meta)
            <> " reason="
            <> LG.caseReason meta
            <> " details="
            <> details
        )
    (LG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xpass lexer case "
            <> LG.caseId meta
            <> " reason="
            <> LG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

evaluate :: LG.LexerCase -> (LG.LexerCase, LG.Outcome, String)
evaluate meta =
  let (outcome, details) = LG.evaluateLexerCase meta
   in (meta, outcome, details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

fixtureValidationTests :: TestTree
fixtureValidationTests =
  testGroup
    "fixture-parse"
    [ testCase "rejects missing required keys" $
        case LG.parseLexerCaseText "missing.yaml" "extensions: []\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for missing required YAML keys",
      testCase "requires reason for xfail" $
        case LG.parseLexerCaseText "xfail.yaml" validXFailMissingReason of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when xfail reason is missing",
      testCase "accepts xpass with reason" $
        case LG.parseLexerCaseText "xpass.yaml" validXPassFixture of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            if LG.caseStatus parsed == LG.StatusXPass
              then pure ()
              else assertFailure "expected xpass status",
      testCase "xfail lexer mismatches retain details" $
        case LG.parseLexerCaseText "xfail-details.yaml" validXFailFixture of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            case LG.evaluateLexerCase parsed of
              (LG.OutcomeXFail, details)
                | null details -> assertFailure "expected xfail details to be non-empty"
                | otherwise -> pure ()
              other -> assertFailure ("expected xfail outcome with details, got: " <> show other),
      testCase "only YAML fixtures are loaded" $ do
        cases <- LG.loadLexerCases
        mapM_
          ( \meta ->
              unless (takeExtension (LG.casePath meta) `elem` [".yaml", ".yml"]) $
                assertFailure ("unexpected non-lexer fixture loaded: " <> LG.casePath meta)
          )
          cases
    ]

validXFailMissingReason :: T.Text
validXFailMissingReason =
  T.unlines
    [ "extensions: []",
      "input: bad",
      "tokens:",
      "  - 'TkVarId \"bad\"'",
      "status: xfail"
    ]

validXFailFixture :: T.Text
validXFailFixture =
  T.unlines
    [ "extensions: []",
      "input: \"-10\"",
      "tokens:",
      "  - 'TkVarSym \"-\"'",
      "  - 'TkInteger 10 TInteger'",
      "status: xfail",
      "reason: known bug"
    ]

validXPassFixture :: T.Text
validXPassFixture =
  T.unlines
    [ "extensions: []",
      "input: \"-10\"",
      "tokens:",
      "  - 'TkVarSym \"-\"'",
      "  - 'TkInteger 10 TInteger'",
      "status: xpass",
      "reason: known bug"
    ]
