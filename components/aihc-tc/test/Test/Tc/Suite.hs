{-
Use the existing test fixture framework for all tests.
Use a hand-written unit test only when this framework cannot test an essential property.
In that test, explain fully why the property is essential and why the framework cannot test it.
-}
module Test.Tc.Suite
  ( tcAnnotatedGoldenTests,
  )
where

import TcAnnotatedGolden qualified as TAG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Build the inline annotated golden test tree from YAML fixtures.
tcAnnotatedGoldenTests :: IO TestTree
tcAnnotatedGoldenTests = do
  cases <- TAG.loadTcAnnotatedCases
  let tests = map mkAnnotatedGoldenTest cases
  pure (testGroup "tc-annotated-golden" tests)

mkAnnotatedGoldenTest :: TAG.TcAnnotatedCase -> TestTree
mkAnnotatedGoldenTest tcase = testCase (TAG.caseId tcase) $ do
  let (outcome, details) = TAG.evaluateTcAnnotatedCase tcase
  case outcome of
    TAG.OutcomePass -> pure ()
    TAG.OutcomeXFail -> pure ()
    TAG.OutcomeFail ->
      assertFailure
        ( "TC annotated golden test failed: "
            <> TAG.caseId tcase
            <> " details="
            <> details
        )
    TAG.OutcomeXPass ->
      assertFailure
        ( "Unexpected pass in TC annotated golden test: "
            <> TAG.caseId tcase
            <> " details="
            <> details
        )
