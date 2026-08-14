{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
-- Use the existing test fixture framework for all tests.
-- Use a hand-written unit test only when this framework cannot test an essential property.
-- In that test, explain fully why the property is essential and why the framework cannot test it.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcEvalFixtureTests,
  )
where

import Aihc.Fc
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (find)
import Data.Text (Text)
import FcGolden
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

-- | Build the golden test tree from fixtures.
fcGoldenTests :: IO TestTree
fcGoldenTests = do
  cases <- loadFcCases
  let tests = map mkTest cases
  pure (testGroup "FC golden tests" tests)

mkTest :: FcCase -> TestTree
mkTest tc = testCase (caseId tc) $ do
  let (outcome, details) = evaluateFcCase tc
  case outcome of
    OutcomePass -> pure ()
    OutcomeXFail -> pure ()
    OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    OutcomeFail -> assertFailure details

fcEvalFixtureTests :: IO TestTree
fcEvalFixtureTests = do
  cases <- filter (("fc" `elem`) . EvalGolden.evalCaseEvaluators) <$> EvalGolden.loadEvalCases
  let tests = exactExceptionContractTests cases <> map mkEvalFixtureTest cases
  pure (testGroup "shared evaluation fixtures via FC" tests)

exactExceptionContractTests :: [EvalGolden.EvalCase] -> [TestTree]
exactExceptionContractTests cases =
  case find ((== "base/data-functor-identity-strictness.yaml") . EvalGolden.evalCaseId) cases of
    Nothing -> [testCase "exact exception contract fixture exists" (assertFailure "Identity strictness fixture is missing")]
    Just evalCase ->
      let rejects label result = testCase ("exception assertion rejects " <> label) $ do
            (outcome, _) <- EvalGolden.evaluateEvalCase (\_ _ -> pure result) evalCase
            assertEqual "outcome" EvalGolden.OutcomeFail outcome
       in [ rejects "a different raised value" (Left (EvalGolden.EvaluationRaised "Different")),
            rejects "a generic evaluation error" (Left (EvalGolden.EvaluationError "evaluator failed")),
            rejects "successful evaluation" (Right "Unit"),
            testCase "exception assertion rejects compilation failure" $ do
              (outcome, _) <-
                EvalGolden.evaluateEvalCase
                  (\_ _ -> pure (Left (EvalGolden.EvaluationRaised "Unit")))
                  evalCase {EvalGolden.evalCaseExpression = "missingName"}
              assertEqual "outcome" EvalGolden.OutcomeFail outcome
          ]

mkEvalFixtureTest :: EvalGolden.EvalCase -> TestTree
mkEvalFixtureTest tc = testCase (EvalGolden.evalCaseId tc) $ do
  (outcome, details) <- EvalGolden.evaluateEvalCase evaluateFcProgram tc
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

evaluateFcProgram :: Text -> FcProgram -> IO (Either EvalGolden.EvaluationFailure Text)
evaluateFcProgram name program = do
  result <- evalProgramBinding name program
  case result of
    Left (EvalRaisedException exception) -> do
      rendered <- renderRawValue exception
      pure $
        Left $
          case rendered of
            Right value -> EvalGolden.EvaluationRaised value
            Left err -> EvalGolden.EvaluationError (show err)
    Left err -> pure (Left (EvalGolden.EvaluationError (show err)))
    Right value -> do
      rendered <- renderRawValue value
      pure $
        case rendered of
          Left err -> Left (EvalGolden.EvaluationError (show err))
          Right text -> Right text
