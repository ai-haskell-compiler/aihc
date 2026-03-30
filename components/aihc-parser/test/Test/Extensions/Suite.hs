{-# LANGUAGE OverloadedStrings #-}

module Test.Extensions.Suite
  ( extensionTests,
  )
where

import Aihc.Cpp (resultOutput)
import Control.Monad (when)
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import ExtensionSupport
import GhcOracle (extensionNamesToGhcExtensions)
import ParserValidation (validateParserWithExtensions)
import Test.Oracle (oracleParsesModuleWithExtensions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

extensionTests :: IO TestTree
extensionTests = do
  allCases <- loadOracleCases
  let cases = filter (not . isH2010Case) allCases
  checks <- mapM mkCaseTest cases
  summary <- summaryTest cases
  pure (testGroup "extensions-oracle" (checks <> [summary]))

mkCaseTest :: CaseMeta -> IO TestTree
mkCaseTest meta = do
  source <- TIO.readFile (caseSourcePath meta)
  pure $ testCase (caseId meta) (assertCase meta source)

assertCase :: CaseMeta -> Text -> Assertion
assertCase meta source = do
  (_, outcome, details) <- evaluateCase meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "Regression in extension case "
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
    _ -> pure ()

summaryTest :: [CaseMeta] -> IO TestTree
summaryTest cases = do
  outcomes <- mapM evaluateCaseFromFile cases
  pure $ testCase "summary" (assertNoRegressions outcomes)

assertNoRegressions :: [(CaseMeta, Outcome, String)] -> Assertion
assertNoRegressions outcomes = do
  let failN = length [() | (_, OutcomeFail, _) <- outcomes]
  when (failN > 0) $
    assertFailure ("extension suite contains " <> show failN <> " regression(s)")

evaluateCaseFromFile :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCaseFromFile meta = do
  source <- TIO.readFile (caseSourcePath meta)
  evaluateCase meta source

evaluateCase :: CaseMeta -> Text -> IO (CaseMeta, Outcome, String)
evaluateCase meta source = do
  let exts = extensionNamesToGhcExtensions (caseExtensions meta) Nothing
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

isH2010Case :: CaseMeta -> Bool
isH2010Case meta = "haskell2010/" `takePrefix` casePath meta
  where
    takePrefix pref str = take (length pref) str == pref
