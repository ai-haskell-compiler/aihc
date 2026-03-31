module Test.StackageProgress.Summary (stackageProgressSummaryTests) where

import Data.List (isInfixOf)
import StackageProgress.Summary
import Test.Tasty
import Test.Tasty.HUnit

stackageProgressSummaryTests :: TestTree
stackageProgressSummaryTests =
  testGroup
    "stackage progress summary"
    [ testCase "tracks counts without retaining optional output" test_countsOnly,
      testCase "preserves succeeded and failed package output" test_keptOutputs,
      testCase "limits ghc errors and falls back to package reason" test_ghcErrors,
      testCase "extracts prompt candidates from parser failures" test_promptCandidate,
      testCase "extracts prompt candidates from any parser failure" test_promptCandidateIncludesAnyFailure,
      testCase "renders prompt text and deterministic candidate selection" test_promptRendering
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
    [FailedPackage "beta-1.0.0" 256 []]
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

test_promptCandidate :: Assertion
test_promptCandidate = do
  let result =
        packageResult
          "monad-st"
          False
          True
          True
          "parse failed in /tmp/Control/Monad/ST/Class.hs: Parse failed: unexpected {'"
          Nothing
          1024
  assertEqual
    "extracts prompt candidate preserving original error text"
    ( Just
        PromptCandidate
          { promptPackageName = "monad-st",
            promptErrorMessage = "parse failed in /tmp/Control/Monad/ST/Class.hs: Parse failed: unexpected {'"
          }
    )
    (promptCandidateFromResult result)

test_promptCandidateIncludesAnyFailure :: Assertion
test_promptCandidateIncludesAnyFailure = do
  let roundtripOnly = packageResult "roundtrip-only" False True True "roundtrip mismatch in /tmp/Foo.hs" Nothing 1024
      parseSucceeded = packageResult "parse-success" True True True "parse failed in /tmp/Bar.hs" Nothing 1024
  assertEqual
    "non-parse failure text is still treated as a parser failure"
    ( Just
        PromptCandidate
          { promptPackageName = "roundtrip-only",
            promptErrorMessage = "roundtrip mismatch in /tmp/Foo.hs"
          }
    )
    (promptCandidateFromResult roundtripOnly)
  assertEqual "successful parser result should be ignored" Nothing (promptCandidateFromResult parseSucceeded)

test_promptRendering :: Assertion
test_promptRendering = do
  let candidates =
        [ PromptCandidate "a" "PARSE_ERROR: a",
          PromptCandidate "b" "PARSE_ERROR: b",
          PromptCandidate "c" "PARSE_ERROR: c"
        ]
  assertEqual "seed 0 chooses first" (Just (PromptCandidate "a" "PARSE_ERROR: a")) (selectPromptCandidate 0 candidates)
  assertEqual "seed 1 chooses second" (Just (PromptCandidate "b" "PARSE_ERROR: b")) (selectPromptCandidate 1 candidates)
  assertEqual "seed wraps by modulo" (Just (PromptCandidate "c" "PARSE_ERROR: c")) (selectPromptCandidate 5 candidates)
  let template =
        unlines
          [ "# Error messages:",
            "{{ERROR_MESSAGES}}",
            "",
            "Re-test by running: nix run .#hackage-tester -- {{PACKAGE_NAME}}",
            "",
            "Fix '{{PACKAGE_NAME}}'"
          ]
      rendered =
        renderPrompt
          template
          PromptCandidate
            { promptPackageName = "monad-st",
              promptErrorMessage = "parse failed in /tmp/Control/Monad/ST/Class.hs"
            }
  assertBool "prompt includes heading" ("# Error messages:" `isInfixOf` rendered)
  assertBool "prompt includes re-test command" ("nix run .#hackage-tester -- monad-st" `isInfixOf` rendered)
  assertBool "prompt includes package replacement" ("Fix 'monad-st'" `isInfixOf` rendered)

packageResult :: String -> Bool -> Bool -> Bool -> String -> Maybe String -> Integer -> PackageResult
packageResult name oursOk hseOk ghcOk reason ghcError size =
  PackageResult
    { package = PackageSpec name "1.0.0",
      packageOursOk = oursOk,
      packageHseOk = hseOk,
      packageGhcOk = ghcOk,
      packageReason = reason,
      packageGhcError = ghcError,
      packageSourceSize = size,
      packageFileErrors = []
    }
