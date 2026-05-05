{-# LANGUAGE OverloadedStrings #-}

module Test.Resolver.Suite
  ( resolverGoldenTests,
  )
where

import Control.Monad (when)
import qualified ResolverGolden as RG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

resolverGoldenTests :: IO TestTree
resolverGoldenTests = do
  cases <- RG.loadResolverCases
  checks <- mapM mkResolverCaseTest cases
  summary <- mkSummaryTest cases
  pure
    ( testGroup
        "resolver-golden"
        (checks <> [summary])
    )

mkResolverCaseTest :: RG.ResolverCase -> IO TestTree
mkResolverCaseTest meta = pure $ case RG.caseStatus meta of
  RG.StatusXFail -> testCaseInfo (RG.caseId meta) (assertXFailResolverCase meta >> pure "Known failure - to be fixed")
  RG.StatusXPass -> testCaseInfo (RG.caseId meta) (assertResolverCase meta >> pure "Known bug - to be fixed")
  _ -> testCase (RG.caseId meta) (assertResolverCase meta)

assertXFailResolverCase :: RG.ResolverCase -> Assertion
assertXFailResolverCase meta =
  case RG.evaluateResolverCase meta of
    (RG.OutcomeXFail, _details) -> pure ()
    (RG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xfail resolver case "
            <> RG.caseId meta
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertResolverCase :: RG.ResolverCase -> Assertion
assertResolverCase meta =
  case RG.evaluateResolverCase meta of
    (RG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in resolver case "
            <> RG.caseId meta
            <> " ("
            <> RG.caseCategory meta
            <> ") expected "
            <> show (RG.caseStatus meta)
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    (RG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xpass resolver case "
            <> RG.caseId meta
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

mkSummaryTest :: [RG.ResolverCase] -> IO TestTree
mkSummaryTest cases = do
  let outcomes = map evaluate cases
  pure $ testCase "summary" (assertNoRegressions outcomes)

evaluate :: RG.ResolverCase -> (RG.ResolverCase, RG.Outcome, String)
evaluate meta =
  let (outcome, details) = RG.evaluateResolverCase meta
   in (meta, outcome, details)

assertNoRegressions :: [(RG.ResolverCase, RG.Outcome, String)] -> Assertion
assertNoRegressions outcomes = do
  let (passN, xfailN, xpassN, failN) = RG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( "resolver golden regressions found. "
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

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
