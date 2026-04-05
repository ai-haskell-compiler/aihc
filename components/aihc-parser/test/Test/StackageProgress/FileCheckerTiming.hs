{-# LANGUAGE NumericUnderscores #-}

module Test.StackageProgress.FileCheckerTiming (stackageProgressFileCheckerTimingTests) where

import StackageProgress.CLI (Parser (..))
import StackageProgress.FileCheckerTiming (maybeVerboseTimingParts)
import Test.Tasty
import Test.Tasty.HUnit

stackageProgressFileCheckerTimingTests :: TestTree
stackageProgressFileCheckerTimingTests =
  testGroup
    "stackage progress file checker timing"
    [ testCase "reports speeds in KB/s when parsing is slow" test_reportsSpeeds,
      testCase "does not report when throughput stays above thresholds" test_hidesFastTimings,
      testCase "hides cpp speed below 0.1ms even when parsing is slow" test_hidesFastCpp,
      testCase "reports slow cpp without parser" test_reportsSlowCppWithoutParser
    ]

test_reportsSpeeds :: Assertion
test_reportsSpeeds =
  assertEqual
    "slow parser emits cpp and parser throughput"
    (Just ["cpp=1250.0KB/s", "aihc=100.0KB/s", "ghc=3.000ms"])
    (maybeVerboseTimingParts [ParserAihc, ParserGhc] 1000 800_000 10_000_000 3_000_000)

test_hidesFastTimings :: Assertion
test_hidesFastTimings =
  assertEqual
    "fast cpp and parser stay quiet"
    Nothing
    (maybeVerboseTimingParts [ParserAihc, ParserGhc] 1000 500_000 4_000_000 3_000_000)

test_hidesFastCpp :: Assertion
test_hidesFastCpp =
  assertEqual
    "cpp speed is omitted below 0.1ms"
    (Just ["aihc=100.0KB/s"])
    (maybeVerboseTimingParts [ParserAihc] 1000 50_000 10_000_000 0)

test_reportsSlowCppWithoutParser :: Assertion
test_reportsSlowCppWithoutParser =
  assertEqual
    "slow cpp still reports without parser timing"
    (Just ["cpp=500.0KB/s"])
    (maybeVerboseTimingParts [] 1000 2_000_000 0 0)
