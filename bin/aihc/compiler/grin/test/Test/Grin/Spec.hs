module Test.Grin.Spec (tests) where

import GrinGolden qualified
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: IO TestTree
tests = do
  fixtures <- GrinGolden.loadGrinCases
  pure
    ( testGroup
        "aihc-grin"
        ( testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip
            : map fixtureTest fixtures
        )
    )

fixtureTest :: GrinGolden.GrinCase -> TestTree
fixtureTest fixture = testCase (GrinGolden.caseId fixture) $
  case GrinGolden.evaluateGrinCase fixture of
    (GrinGolden.OutcomePass, _) -> pure ()
    (GrinGolden.OutcomeXFail, _) -> pure ()
    (GrinGolden.OutcomeXPass, details) -> assertFailure ("unexpected pass: " <> details)
    (GrinGolden.OutcomeFail, details) -> assertFailure details
