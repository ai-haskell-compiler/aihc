{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle.Suite
  ( oracleTests,
  )
where

import Aihc.Cpp (resultOutput)
import Control.Monad (when)
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import ExtensionSupport
  ( CaseMeta (..),
    Expected (..),
    Outcome (..),
    caseSourcePath,
    finalizeOutcome,
    loadOracleCases,
  )
import GhcOracle (extensionNamesToGhcExtensions)
import ParserValidation (validateParserWithExtensions)
import Test.Oracle (oracleParsesModuleWithExtensions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

oracleTests :: IO TestTree
oracleTests = do
  cases <- loadOracleCases
  checks <- mapM mkCaseTest cases
  framework <- frameworkTests
  summary <- mkSummaryTest cases
  pure (testGroup "oracle" (checks <> [framework, summary]))

mkCaseTest :: CaseMeta -> IO TestTree
mkCaseTest meta = do
  source <- TIO.readFile (caseSourcePath meta)
  pure $ testCase (caseId meta) (assertCase meta source)

mkSummaryTest :: [CaseMeta] -> IO TestTree
mkSummaryTest cases = do
  outcomes <- mapM evaluateCaseFromFile cases
  pure $
    testCase "summary" $ do
      let (passN, xfailN, xpassN, failN) = foldr (countOutcome . snd3) (0, 0, 0, 0) outcomes
          totalN = passN + xfailN + xpassN + failN
          completion = pct (passN + xpassN) totalN
      when (failN > 0 || xpassN > 0) $
        assertFailure
          ( "Oracle regressions found. "
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
  where
    snd3 (_, b, _) = b

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
  (_, outcome, details) <- evaluateCaseText meta source
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

evaluateCaseFromFile :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCaseFromFile meta = do
  source <- TIO.readFile (caseSourcePath meta)
  evaluateCaseText meta source

evaluateCaseText :: CaseMeta -> Text -> IO (CaseMeta, Outcome, String)
evaluateCaseText meta source = do
  -- Use Haskell2010 as the base language for oracle tests, as these fixtures
  -- are meant to be valid Haskell2010 code (possibly with extensions)
  let exts = extensionNamesToGhcExtensions (caseExtensions meta) (Just "Haskell2010")
      source' =
        resultOutput
          ( preprocessForParserWithoutIncludesIfEnabled
              (caseExtensions meta)
              []
              (casePath meta)
              source
          )
      oracleOk = oracleParsesModuleWithExtensions exts source'
      validationOk = isNothing (validateParserWithExtensions exts source')
      roundtripOk = oracleOk && validationOk
  pure (finalizeOutcome meta oracleOk roundtripOk)

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
                (_, outcome, _) <- evaluateCaseText meta "module M where\nx = { y = 1, }\n"
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
                (_, outcome, _) <- evaluateCaseText meta "module M where\nf \\x -> x\n"
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
                (_, outcome, _) <- evaluateCaseText meta "module M where\nx = { y = 1, }\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome)
      ]
