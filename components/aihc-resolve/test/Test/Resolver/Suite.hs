{-# LANGUAGE OverloadedStrings #-}

module Test.Resolver.Suite
  ( resolverGoldenTests,
    resolverUnitTests,
  )
where

import Aihc.Parser (defaultConfig, parseModule)
import Aihc.Resolve (ResolveResult (..), extractInterface, resolve, resolveWithDeps)
import Control.Monad (when)
import Data.Text (Text)
import qualified ResolverGolden as RG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

resolverUnitTests :: TestTree
resolverUnitTests =
  testGroup
    "resolver-unit"
    [ testCase "dependency-backed GHC.Num supplies built-in fromInteger" testDependencyBackedGhcNum
    ]

testDependencyBackedGhcNum :: Assertion
testDependencyBackedGhcNum =
  case (parse "GHC.Num" numSource, parse "Prelude" preludeSource) of
    (Right numModule, Right preludeModule) -> do
      let dependencyResult = resolve [numModule]
          result = resolveWithDeps (extractInterface dependencyResult) [preludeModule]
      case resolveErrors dependencyResult of
        [] ->
          case resolveErrors result of
            [] -> pure ()
            errors -> assertFailure ("failed to resolve built-in syntax through dependency exports: " <> show errors)
        errors -> assertFailure ("failed to resolve dependency module: " <> show errors)
    (Left errors, _) -> assertFailure errors
    (_, Left errors) -> assertFailure errors
  where
    parse sourceName source =
      case parseModule defaultConfig source of
        ([], modu) -> Right modu
        (errors, _) -> Left (sourceName <> " parse failure: " <> show errors)
    numSource :: Text
    numSource =
      "module GHC.Num (Num (..)) where\n\
      \data Integer = Integer\n\
      \class Num a where\n\
      \  fromInteger :: Integer -> a\n"
    preludeSource :: Text
    preludeSource =
      "module Prelude (Num (..)) where\n\
      \import GHC.Num (Num (..))\n\
      \one = 1\n"

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
