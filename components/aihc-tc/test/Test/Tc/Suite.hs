{-
Use the existing test fixture framework for all tests.
Use a hand-written unit test only when this framework cannot test an essential property.
In that test, explain fully why the property is essential and why the framework cannot test it.
-}
module Test.Tc.Suite
  ( tcAnnotatedGoldenRegressionTests,
    tcAnnotatedGoldenTests,
  )
where

import Data.List (find)
import TcAnnotatedGolden qualified as TAG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tcAnnotatedGoldenRegressionTests :: IO TestTree
tcAnnotatedGoldenRegressionTests = do
  cases <- TAG.loadTcAnnotatedCases
  selected <- mapM (`requiredCase` cases) regressionCaseIds
  pure (testGroup "tc-annotated-regressions" (map mkAnnotatedGoldenTest selected))

regressionCaseIds :: [String]
regressionCaseIds =
  [ "do-notation-monad.yaml",
    "integer-literal-signatures.yaml",
    "integer-pattern-lambda-case.yaml",
    "list-comprehension-guard.yaml",
    "overloaded-integer-inferred-types.yaml",
    "stock-deriving-strategy-selection.yaml",
    "typeclass-evidence.yaml"
  ]

requiredCase :: String -> [TAG.TcAnnotatedCase] -> IO TAG.TcAnnotatedCase
requiredCase identifier cases =
  case find ((== identifier) . TAG.caseId) cases of
    Just testCase' -> pure testCase'
    Nothing -> fail ("Required TC annotated fixture does not exist: " <> identifier)

-- | Build the inline annotated golden test tree from YAML fixtures.
tcAnnotatedGoldenTests :: IO TestTree
tcAnnotatedGoldenTests = do
  cases <- TAG.loadTcAnnotatedCases
  let tests = map mkAnnotatedGoldenTest cases
  pure (testGroup "tc-annotated-golden" tests)

mkAnnotatedGoldenTest :: TAG.TcAnnotatedCase -> TestTree
mkAnnotatedGoldenTest tcase = testCase (TAG.caseId tcase) $ do
  (outcome, details) <- TAG.evaluateTcAnnotatedCase tcase
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
