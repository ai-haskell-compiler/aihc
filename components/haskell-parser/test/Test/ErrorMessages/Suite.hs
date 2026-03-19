{-# LANGUAGE OverloadedStrings #-}

module Test.ErrorMessages.Suite
  ( errorMessageTests,
  )
where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified ParserErrorGolden as PEG
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

errorMessageTests :: IO TestTree
errorMessageTests = do
  cases <- PEG.loadErrorMessageCases
  checks <- mapM mkCaseTest cases
  summary <- mkSummaryTest cases
  pure (testGroup "error-messages" ([fixtureValidationTests] <> checks <> [summary]))

mkCaseTest :: PEG.ErrorMessageCase -> IO TestTree
mkCaseTest meta = pure $ testCase (PEG.caseId meta) (assertCase meta)

mkSummaryTest :: [PEG.ErrorMessageCase] -> IO TestTree
mkSummaryTest cases = do
  let outcomes = map evaluate cases
  pure $ testCase "summary" (assertNoRegressions outcomes)

assertCase :: PEG.ErrorMessageCase -> Assertion
assertCase meta =
  case PEG.evaluateErrorMessageCase meta of
    (PEG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in parser error case "
            <> PEG.caseId meta
            <> " ("
            <> PEG.caseCategory meta
            <> ") expected "
            <> show (PEG.caseStatus meta)
            <> " reason="
            <> PEG.caseReason meta
            <> " details="
            <> details
        )
    (PEG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xpass parser error case "
            <> PEG.caseId meta
            <> " reason="
            <> PEG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertNoRegressions :: [(PEG.ErrorMessageCase, PEG.Outcome, String)] -> Assertion
assertNoRegressions outcomes = do
  let (passN, xfailN, xpassN, failN) = PEG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( "parser error regressions found. "
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

evaluate :: PEG.ErrorMessageCase -> (PEG.ErrorMessageCase, PEG.Outcome, String)
evaluate meta =
  let (outcome, details) = PEG.evaluateErrorMessageCase meta
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
        case PEG.parseErrorMessageCaseText "missing.yaml" "status: pass\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for missing required YAML keys",
      testCase "requires reason for xfail" $
        case PEG.parseErrorMessageCaseText "xfail.yaml" validXFailMissingReason of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when xfail reason is missing",
      testCase "accepts pass fixtures" $
        case PEG.parseErrorMessageCaseText "pass.yaml" validPassFixture of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            if PEG.caseStatus parsed == PEG.StatusPass
              then pure ()
              else assertFailure "expected pass status",
      testCase "only YAML fixtures are loaded" $ do
        cases <- PEG.loadErrorMessageCases
        mapM_
          ( \meta ->
              unless (takeExtension (PEG.casePath meta) `elem` [".yaml", ".yml"]) $
                assertFailure ("unexpected non-error-message fixture loaded: " <> PEG.casePath meta)
          )
          cases
    ]

validXFailMissingReason :: T.Text
validXFailMissingReason =
  T.unlines
    [ "src: bad",
      "ghc: bad",
      "aihc: bad",
      "status: xfail"
    ]

validPassFixture :: T.Text
validPassFixture =
  T.unlines
    [ "src: bad",
      "ghc: bad",
      "aihc: bad",
      "status: pass"
    ]
