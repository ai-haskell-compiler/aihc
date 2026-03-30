{-# LANGUAGE OverloadedStrings #-}

module Test.H2010.Suite
  ( h2010Tests,
  )
where

import Aihc.Cpp (resultOutput)
import Control.Monad (when)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ExtensionSupport
  ( CaseMeta (..),
    Expected (..),
    ExtensionSpec (..),
    Outcome (..),
    fixtureDirFor,
    loadManifest,
  )
import GhcOracle (oracleDetailedParsesModuleWithNamesAt)
import ParserValidation (validateParser)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

fixtureRoot :: FilePath
fixtureRoot = fixtureDirFor h2010Spec

h2010Spec :: ExtensionSpec
h2010Spec =
  ExtensionSpec
    { extName = "Haskell2010",
      extFixtureDir = "haskell2010",
      extNotes = ""
    }

h2010Tests :: IO TestTree
h2010Tests = do
  cases <- loadManifest h2010Spec
  checks <- mapM mkCaseTest cases
  framework <- frameworkTests
  summary <- mkSummaryTest cases
  pure (testGroup "haskell2010-oracle" (checks <> [framework, summary]))

mkCaseTest :: CaseMeta -> IO TestTree
mkCaseTest meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  pure $ testCase (caseId meta) (assertCase meta source)

mkSummaryTest :: [CaseMeta] -> IO TestTree
mkSummaryTest cases = do
  outcomes <- mapM evaluateCase cases
  pure $
    testCase "summary" $ do
      let (passN, xfailN, xpassN, failN) = foldr countOutcome (0, 0, 0, 0) outcomes
          totalN = passN + xfailN + xpassN + failN
          completion = pct (passN + xpassN) totalN
      when (failN > 0 || xpassN > 0) $
        assertFailure
          ( "Haskell2010 regressions found. "
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

countOutcome :: Outcome -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
countOutcome outcome (passN, xfailN, xpassN, failN) =
  case outcome of
    OutcomePass -> (passN + 1, xfailN, xpassN, failN)
    OutcomeXFail -> (passN, xfailN + 1, xpassN, failN)
    OutcomeXPass -> (passN, xfailN, xpassN + 1, failN)
    OutcomeFail -> (passN, xfailN, xpassN, failN + 1)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

assertCase :: CaseMeta -> Text -> Assertion
assertCase meta source = do
  (outcome, details) <- evaluateCaseText meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "Regression in case "
            <> caseId meta
            <> " ("
            <> caseCategory meta
            <> ") expected "
            <> show (caseExpected meta)
            <> " reason="
            <> caseReason meta
            <> " details="
            <> details
        )
    OutcomeXPass ->
      assertFailure
        ( "Unexpected pass in xfail case "
            <> caseId meta
            <> " ("
            <> caseCategory meta
            <> ") reason="
            <> caseReason meta
        )
    _ -> pure ()

evaluateCase :: CaseMeta -> IO Outcome
evaluateCase meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  fst <$> evaluateCaseText meta source

evaluateCaseText :: CaseMeta -> Text -> IO (Outcome, String)
evaluateCaseText meta source = do
  let source' = resultOutput (preprocessForParserWithoutIncludes (casePath meta) source)
      validationOk = isNothing (validateParser source')
      oracleResult = oracleDetailedParsesModuleWithNamesAt (casePath meta) [] Nothing source'
  pure (classify (caseExpected meta) oracleResult validationOk)

classify :: Expected -> Either Text () -> Bool -> (Outcome, String)
classify expected oracleResult validationOk =
  case oracleResult of
    Left oracleErr ->
      ( OutcomeFail,
        "oracle rejected fixture: " <> T.unpack oracleErr
      )
    Right () ->
      classifyByValidation expected validationOk

classifyByValidation :: Expected -> Bool -> (Outcome, String)
classifyByValidation expected validationOk =
  case expected of
    ExpectPass
      | validationOk -> (OutcomePass, "")
      | otherwise -> (OutcomeFail, "roundtrip mismatch against oracle AST")
    ExpectXFail
      | validationOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
      | otherwise -> (OutcomeXFail, "")

frameworkTests :: IO TestTree
frameworkTests =
  pure $
    testGroup
      "framework"
      [ testCase "oracle parse failure fails xfail case" $
          let meta =
                CaseMeta
                  { caseId = "framework-invalid-xfail",
                    caseCategory = "framework",
                    casePath = "framework-invalid-xfail.hs",
                    caseExpected = ExpectXFail,
                    caseReason = "regression coverage",
                    caseExtensions = []
                  }
           in do
                (outcome, _) <- evaluateCaseText meta "module M where\nx = { y = 1, }\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome),
        testCase "oracle rejects top-level block-argument lambda" $
          let meta =
                CaseMeta
                  { caseId = "framework-block-argument-lambda",
                    caseCategory = "framework",
                    casePath = "framework-block-argument-lambda.hs",
                    caseExpected = ExpectPass,
                    caseReason = "",
                    caseExtensions = []
                  }
           in do
                (outcome, _) <- evaluateCaseText meta "module M where\nf \\x -> x\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome),
        testCase "oracle parse failure fails pass case" $
          let meta =
                CaseMeta
                  { caseId = "framework-invalid-pass",
                    caseCategory = "framework",
                    casePath = "framework-invalid-pass.hs",
                    caseExpected = ExpectPass,
                    caseReason = "",
                    caseExtensions = []
                  }
           in do
                (outcome, _) <- evaluateCaseText meta "module M where\nx = { y = 1, }\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome)
      ]
