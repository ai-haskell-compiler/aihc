{-# LANGUAGE OverloadedStrings #-}

module Test.Resolver.Suite
  ( resolverGoldenTests,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Resolve
  ( ResolutionNamespace (..),
    ResolveError (..),
    ResolveResult (..),
    resolve,
  )
import Control.Monad (when)
import qualified ResolverGolden as RG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, testCaseInfo)

resolverGoldenTests :: IO TestTree
resolverGoldenTests = do
  cases <- RG.loadResolverCases
  checks <- mapM mkResolverCaseTest cases
  summary <- mkSummaryTest cases
  pure
    ( testGroup
        "resolver-golden"
        (checks <> [summary, resolveErrorsTests])
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

resolveErrorsTests :: TestTree
resolveErrorsTests =
  testGroup
    "resolve-errors"
    [ testCase "collects unresolved names into ResolveResult" $ do
        let source = "module Main where\nx = unknownVar\n"
            config = defaultConfig {parserSourceName = "<test>"}
            (errs, parsed) = parseModule config source
            result = resolve [parsed]
        assertEqual "parser errors" [] errs
        case resolveErrors result of
          [ResolveResolutionError span' name namespace msg] -> do
            assertEqual "error name" "unknownVar" name
            assertEqual "error namespace" ResolutionNamespaceTerm namespace
            assertEqual "error message" "unbound" msg
            case span' of
              SourceSpan _ 2 5 2 15 _ _ -> pure ()
              _ ->
                assertFailure
                  ("unexpected error source span: " <> show span')
          actual ->
            assertEqual
              "unresolved names should remain annotated and also populate resolveErrors"
              1
              (length actual),
      testCase "collects missing module imports into ResolveResult" $ do
        let source = "module Main where\nimport Missing.Foo\n"
            config = defaultConfig {parserSourceName = "<test>"}
            (errs, parsed) = parseModule config source
            result = resolve [parsed]
        assertEqual "parser errors" [] errs
        case resolveErrors result of
          [ResolveResolutionError span' name namespace msg] -> do
            assertEqual "error name" "Missing.Foo" name
            assertEqual "error namespace" ResolutionNamespaceModule namespace
            assertEqual "error message" "not found" msg
            case span' of
              SourceSpan _ 2 8 2 19 _ _ -> pure ()
              _ ->
                assertFailure
                  ("unexpected error source span: " <> show span')
          actual ->
            assertEqual
              "missing module imports should remain annotated and also populate resolveErrors"
              1
              (length actual)
    ]
