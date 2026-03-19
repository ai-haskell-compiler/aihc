module Test.StackageProgress.Summary (stackageProgressSummaryTests) where

import StackageProgress.Summary
import Test.Tasty
import Test.Tasty.HUnit

stackageProgressSummaryTests :: TestTree
stackageProgressSummaryTests =
  testGroup
    "stackage progress summary"
    [ testCase "tracks counts without retaining optional output" test_countsOnly,
      testCase "preserves succeeded and failed package output" test_keptOutputs,
      testCase "limits ghc errors and falls back to package reason" test_ghcErrors
    ]

test_countsOnly :: Assertion
test_countsOnly = do
  let summary =
        finalizeSummary $
          addPackageResults
            (SummaryOptions False False 0)
            [ packageResult "alpha" True True True "" Nothing 0,
              packageResult "beta" False True False "parser failed" Nothing 128
            ]
            emptySummary
  assertEqual "ours count" 1 (summarySuccessOursN summary)
  assertEqual "hse count" 2 (summarySuccessHseN summary)
  assertEqual "ghc count" 1 (summarySuccessGhcN summary)
  assertEqual "succeeded packages omitted" [] (summarySucceededPackages summary)
  assertEqual "failed packages omitted" [] (summaryFailedPackages summary)
  assertEqual "ghc errors omitted" [] (summaryGhcErrors summary)

test_keptOutputs :: Assertion
test_keptOutputs = do
  let summary =
        finalizeSummary $
          addPackageResults
            (SummaryOptions True True 0)
            [ packageResult "alpha" True True True "" Nothing 0,
              packageResult "beta" False True True "parser failed" Nothing 256,
              packageResult "gamma" True False True "" Nothing 0
            ]
            emptySummary
  assertEqual "succeeded packages keep encounter order" ["alpha-1.0.0", "gamma-1.0.0"] (summarySucceededPackages summary)
  assertEqual
    "failed table keeps parser failures only"
    [FailedPackage "beta-1.0.0" 256]
    (summaryFailedPackages summary)

test_ghcErrors :: Assertion
test_ghcErrors = do
  let summary =
        finalizeSummary $
          addPackageResults
            (SummaryOptions False False 2)
            [ packageResult "alpha" True True False "" (Just "ghc exploded") 0,
              packageResult "beta" False True False "  parser failed early  " Nothing 64,
              packageResult "gamma" False True False "should be truncated" (Just "ignored") 32
            ]
            emptySummary
  assertEqual
    "ghc errors are limited and use fallback reason text"
    [ ("alpha-1.0.0", "ghc exploded"),
      ("beta-1.0.0", "No direct GHC diagnostic; package failed before/around GHC check: parser failed early")
    ]
    (summaryGhcErrors summary)

packageResult :: String -> Bool -> Bool -> Bool -> String -> Maybe String -> Integer -> PackageResult
packageResult name oursOk hseOk ghcOk reason ghcError size =
  PackageResult
    { package = PackageSpec name "1.0.0",
      packageOursOk = oursOk,
      packageHseOk = hseOk,
      packageGhcOk = ghcOk,
      packageReason = reason,
      packageGhcError = ghcError,
      packageSourceSize = size
    }
