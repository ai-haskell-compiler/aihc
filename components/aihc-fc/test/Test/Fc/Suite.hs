{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
-- Use the existing test fixture framework for all tests.
-- Use a hand-written unit test only when this framework cannot test an essential property.
-- In that test, explain fully why the property is essential and why the framework cannot test it.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcEvalFixtureTests,
    fcLintTests,
  )
where

import Aihc.Fc
import Aihc.Tc.Types (Kind (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), mkTyCon, setTyVarKind)
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (find)
import Data.Text (Text)
import FcGolden
import Hedgehog (property, success)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

fcLintTests :: TestTree
fcLintTests =
  testGroup
    "Core lint"
    [ testCase "rejects a wrong type application kind" $ do
        let errors = lintProgram emptyLintEnv (typeApplicationProgram liftedType)
        assertBool "expected a kind error" (any isKindError errors),
      testCase "accepts a correct type application kind" $
        assertEqual "lint errors" [] (lintProgram emptyLintEnv (typeApplicationProgram runtimeRepType)),
      testCase "rejects a wrong type constructor argument kind" $ do
        let proxyTyCon = mkTyCon "RuntimeRepProxy" 1 (KFun KRuntimeRep KType)
            badType = TcTyCon proxyTyCon [liftedType]
            program = FcProgram testModule [FcExternal (FcTopLevelOrigin "test" "Test" "value") badType]
        assertBool "expected a kind error" (any isKindError (lintProgram emptyLintEnv program)),
      testProperty "Hedgehog options" (property success)
    ]

isKindError :: LintError -> Bool
isKindError KindMismatch {} = True
isKindError InvalidKindApplication {} = True
isKindError NonValueKind {} = True
isKindError _ = False

typeApplicationProgram :: TcType -> FcProgram
typeApplicationProgram argumentType =
  FcProgram
    testModule
    [ FcExternal origin polymorphicType,
      FcTopBind (FcNonRec result (FcTyApp (FcVar imported) argumentType))
    ]
  where
    origin = FcTopLevelOrigin "test" "Test" "value"
    imported = fcExternalVar origin polymorphicType
    result = Var "result" (Unique 2) liftedType

testModule :: FcModuleId
testModule = FcModuleId "test" "Test"

polymorphicType :: TcType
polymorphicType = TcForAllTy runtimeRepVariable liftedType

runtimeRepVariable :: TyVarId
runtimeRepVariable = setTyVarKind KRuntimeRep (TyVarId "rep" (Unique 1))

liftedType :: TcType
liftedType = TcTyCon (TyCon "Int" 0) []

runtimeRepType :: TcType
runtimeRepType = TcTyCon (TyCon "'IntRep" 0) []

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
