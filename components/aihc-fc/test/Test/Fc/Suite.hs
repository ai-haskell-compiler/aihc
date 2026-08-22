{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
import Aihc.Fc.Syntax qualified as Fc
import Aihc.Parser (defaultConfig, parseModule)
import Aihc.Resolve (Package (..), PackageId (..), ResolveResult (..), resolveWithDeps)
import Aihc.Tc (DataTypeInfo (..), TcConfig, TcInterface (..), TyConInfo (..), emptyTcInterface, tcConfig, tcModuleBindings, typecheckModulesWithInterface)
import Aihc.Tc.Types (TcType (..), TyVarId (..), TypeScheme (..), Unique (..), setTyVarKind, tyConKey, pattern KFun, pattern KMeta, pattern KRuntimeRep, pattern KType)
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (find, isInfixOf)
import Data.Map.Strict qualified as Map
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
      rejectsWrongCaseBinderType,
      rejectsWrongAlternativeBinderType,
      testCase "rejects a wrong type constructor argument kind" $ do
        let proxyTyCon = legacyTyConWithKind "RuntimeRepProxy" 1 (KFun KRuntimeRep KType)
            badType = TcTyCon proxyTyCon [liftedType]
            program = FcProgram testModule (Map.singleton (tyConKey proxyTyCon) (ForAll [] [] (KFun KRuntimeRep KType))) [FcExternal (FcTopLevelOrigin "test" "Test" "value") badType]
        assertBool "expected a kind error" (any isKindError (lintProgram emptyLintEnv program)),
      missingGlobalEnvironmentTest,
      missingDataTypeInformationTest,
      corruptDataTypeInformationTest,
      corruptClassKindTest,
      missingLiteralTypeTest,
      testProperty "Hedgehog options" (property success)
    ]

rejectsWrongCaseBinderType :: TestTree
rejectsWrongCaseBinderType = testCase "rejects a wrong case binder type" $ do
  let scrutinee = Var "scrutinee" (Unique 10) liftedType
      binder = Var "binder" (Unique 11) (TcTyCon (legacyTyCon "Char" 0) [])
      expression = Fc.FcCase (FcVar scrutinee) binder [FcAlt DefaultAlt [] (FcVar scrutinee)]
      environment = emptyLintEnv {leTerms = Map.singleton (varUnique scrutinee) (varType scrutinee)}
  case lintExpr environment expression of
    Left TypeMismatch {} -> pure ()
    result -> assertFailure ("expected a case binder type error, but got: " <> show result)

rejectsWrongAlternativeBinderType :: TestTree
rejectsWrongAlternativeBinderType = testCase "rejects a wrong case alternative binder type" $ do
  let boxType = TcTyCon (legacyTyConWithKind "Box" 0 KType) []
      fieldType = liftedType
      wrongField = Var "field" (Unique 12) (TcTyCon (legacyTyCon "Char" 0) [])
      scrutinee = Var "scrutinee" (Unique 13) boxType
      caseBinder = Var "caseBinder" (Unique 14) boxType
      constructorOrigin = FcTopLevelOrigin "test" "Test" "Box"
      constructor = fcConstructorIdFromSymbol constructorOrigin
      expression = Fc.FcCase (FcVar scrutinee) caseBinder [FcAlt (DataAlt constructor) [wrongField] (FcVar wrongField)]
      environment =
        emptyLintEnv
          { leTerms = Map.singleton (varUnique scrutinee) (varType scrutinee),
            leDataCons = Map.singleton constructorOrigin ([], [fieldType], boxType)
          }
  case lintExpr environment expression of
    Left TypeMismatch {} -> pure ()
    result -> assertFailure ("expected an alternative binder type error, but got: " <> show result)

-- The golden runner always supplies a complete interface.
-- This unit test is necessary to verify the failure for an incomplete variable environment.
missingGlobalEnvironmentTest :: TestTree
missingGlobalEnvironmentTest = testCase "desugaring rejects a missing global variable" $ do
  let sources =
        [ "module Dependency where\nshared x = x\n",
          "module Test where\nimport Dependency\nuse x = shared x\n"
        ]
      parsed = map (snd . parseModule defaultConfig) sources
      resolved = resolveWithDeps mempty [(Package "" (PackageId ""), modu) | modu <- parsed]
  case resolved of
    ResolveResult {resolvedModules, resolveErrors = []} -> do
      let checkedModules = fst (typecheckModulesWithInterface testTcConfig emptyTcInterface (map snd resolvedModules))
          bindings = concatMap tcModuleBindings checkedModules
      case reverse checkedModules of
        checked : _ -> do
          let result = desugarModuleWithInterface (DesugarConfig (PackageId "aihc-prim")) bindings emptyTcInterface checked
          assertEqual
            "desugar errors"
            ["variable use is not in the desugarer environment: Dependency.shared"]
            (dsErrors result)
        [] -> assertFailure "type checking did not return the test module"
    ResolveResult {resolveErrors} -> assertFailure ("resolve errors: " <> show resolveErrors)

-- The golden runner always supplies a complete interface.
-- This unit test is necessary to verify the failure for missing data type information.
missingDataTypeInformationTest :: TestTree
missingDataTypeInformationTest = testCase "desugaring rejects missing data type information" $ do
  let parsed = snd (parseModule defaultConfig "module Test where\ndata Missing a\n")
      resolved = resolveWithDeps mempty [(Package "" (PackageId ""), parsed)]
  case resolved of
    ResolveResult {resolvedModules = [(_, resolvedModule)], resolveErrors = []} -> do
      let checkedModules = fst (typecheckModulesWithInterface testTcConfig emptyTcInterface [resolvedModule])
      case checkedModules of
        [checked] -> do
          let result = desugarModuleWithInterface (DesugarConfig (PackageId "aihc-prim")) (tcModuleBindings checked) emptyTcInterface checked
          assertEqual
            "desugar errors"
            ["missing checked data type information for Test.Missing"]
            (dsErrors result)
        _ -> assertFailure "type checking did not return the test module"
    ResolveResult {resolveErrors} -> assertFailure ("resolve errors: " <> show resolveErrors)

-- A source fixture cannot construct corrupt type-checker information.
-- This unit test verifies that desugaring rejects a non-final checked result kind.
corruptDataTypeInformationTest :: TestTree
corruptDataTypeInformationTest = testCase "desugaring rejects corrupt data type information" $ do
  let parsed = snd (parseModule defaultConfig "module Test where\ndata Corrupt a\n")
      resolved = resolveWithDeps mempty [(Package "" (PackageId ""), parsed)]
  case resolved of
    ResolveResult {resolvedModules = [(_, resolvedModule)], resolveErrors = []} -> do
      let (checkedModules, interface) = typecheckModulesWithInterface testTcConfig emptyTcInterface [resolvedModule]
          corrupt info
            | dtiName info == "Corrupt" = info {dtiResultKind = KMeta (Unique 999)}
            | otherwise = info
          corruptInterface = interface {tcInterfaceDataTypes = map corrupt (tcInterfaceDataTypes interface)}
      case checkedModules of
        [checked] -> do
          let result = desugarModuleWithInterface (DesugarConfig (PackageId "aihc-prim")) (tcModuleBindings checked) corruptInterface checked
          assertEqual
            "desugar errors"
            ["invalid checked result kind for Test.Corrupt"]
            (dsErrors result)
        _ -> assertFailure "type checking did not return the test module"
    ResolveResult {resolveErrors} -> assertFailure ("resolve errors: " <> show resolveErrors)

-- A source fixture cannot construct a corrupt checked class kind.
-- This unit test verifies that desugaring rejects the corrupt type-checker fact.
corruptClassKindTest :: TestTree
corruptClassKindTest = testCase "desugaring rejects a corrupt checked class kind" $ do
  let sources =
        [ "module Dependency where\nclass C a where\n  method :: a -> a\nvalue :: C a => a -> a\nvalue x = x\n",
          "module Test where\nimport Dependency\nuse = value\n"
        ]
      parsed = map (snd . parseModule defaultConfig) sources
      resolved = resolveWithDeps mempty [(Package "" (PackageId ""), modu) | modu <- parsed]
  case resolved of
    ResolveResult {resolvedModules, resolveErrors = []} -> do
      let (checkedModules, interface) = typecheckModulesWithInterface testTcConfig emptyTcInterface (map snd resolvedModules)
          corrupt info
            | tciName info == "C" = info {tciKindScheme = ForAll [] [] KType}
            | otherwise = info
          corruptInterface = interface {tcInterfaceTyCons = map corrupt (tcInterfaceTyCons interface)}
      case reverse checkedModules of
        checked : _ -> do
          let result = desugarModuleWithInterface (DesugarConfig (PackageId "aihc-prim")) (concatMap tcModuleBindings checkedModules) corruptInterface checked
          assertBool
            ("expected a class kind error, but got: " <> show (dsErrors result))
            (any ("does not have an authoritative Constraint result kind" `isInfixOf`) (dsErrors result))
        [] -> assertFailure "type checking did not return the test module"
    ResolveResult {resolveErrors} -> assertFailure ("resolve errors: " <> show resolveErrors)

-- A checked source fixture always contains literal type information.
-- This unit test verifies that the Core parser rejects a literal without that information.
missingLiteralTypeTest :: TestTree
missingLiteralTypeTest = testCase "Core parsing rejects a literal without a checked type" $
  case parseExpr "1#IntRep" of
    Left _ -> pure ()
    Right expression -> assertFailure ("expected a Core parse error, but got: " <> show expression)

isKindError :: LintError -> Bool
isKindError KindMismatch {} = True
isKindError InvalidKindApplication {} = True
isKindError NonValueKind {} = True
isKindError _ = False

typeApplicationProgram :: TcType -> FcProgram
typeApplicationProgram argumentType =
  FcProgram
    testModule
    mempty
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
liftedType = TcTyCon (legacyTyCon "Int" 0) []

runtimeRepType :: TcType
runtimeRepType = TcTyCon (legacyTyCon "'IntRep" 0) []

testTcConfig :: TcConfig
testTcConfig = tcConfig (PackageId "aihc-prim")

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
