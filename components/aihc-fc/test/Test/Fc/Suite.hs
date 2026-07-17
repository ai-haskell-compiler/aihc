{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcEvalTests,
    fcEvalFixtureTests,
    fcOptimizationTests,
  )
where

import Aihc.Fc
import Aihc.Tc (RuntimeRep (..), TcType (..), TyCon (..), Unique (..))
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Testing.EvalFixture qualified as EvalGolden
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

fcEvalTests :: TestTree
fcEvalTests =
  testGroup
    "FC evaluator"
    [ testCase "renders string literals" $
        assertEvalExpr "\"hello world\"" (FcLit (LitString "hello world")),
      testCase "renders char literals" $
        assertEvalExpr "'x'#" (FcLit (LitChar WordRep 'x')),
      testCase "renders int literals" $
        assertEvalExpr "42" (FcLit (LitInt IntRep 42)),
      testCase "applies lambdas" $
        assertEvalExpr
          "\"ok\""
          (FcApp (FcLam (var "x" stringTy) (FcVar (var "x" stringTy))) (FcLit (LitString "ok"))),
      testCase "evaluates top-level bindings" $
        let program =
              FcProgram
                [ FcTopBind
                    (FcNonRec (var "answer" stringTy) (FcLit (LitString "top")))
                ]
         in do
              result <- evalProgramBinding "answer" program >>= renderEvalResult
              assertEqual "result" (Right "\"top\"") result,
      testCase "renders raw constructor values" $ do
        result <-
          renderRawValue
            (VConstructor ":" [VConstructor "C#" [VLit (LitChar WordRep 'x')], VConstructor "[]" []])
        assertEqual
          "raw result"
          (Right ": 'x' []")
          result
    ]

fcOptimizationTests :: TestTree
fcOptimizationTests =
  testGroup
    "FC optimizations"
    [ testCase "eliminates values and types unreachable from the entry point" $ do
        let liveTy = ty "Live"
            leafTy = ty "Leaf"
            deadTy = ty "Dead"
            mainVar = Var "main" (Unique 1) liveTy
            helperVar = Var "helper" (Unique 2) liveTy
            deadVar = Var "dead" (Unique 3) deadTy
            liveData = FcData "Live" [] [("Live", [leafTy])]
            leafData = FcData "Leaf" [] [("Leaf", [])]
            deadData = FcData "Dead" [] [("Dead", [])]
            helper = FcTopBind (FcNonRec helperVar (FcVar (Var "Live" (Unique 4) (TcFunTy leafTy liveTy))))
            mainBinding = FcTopBind (FcNonRec mainVar (FcVar helperVar))
            deadBinding = FcTopBind (FcNonRec deadVar (FcVar (Var "Dead" (Unique 5) deadTy)))
            program = FcProgram [deadData, deadBinding, leafData, liveData, helper, mainBinding]
        assertEqual
          "reachable program"
          (FcProgram [leafData, liveData, helper, mainBinding])
          (eliminateDeadCode "main" program),
      testCase "does not confuse a local binder with a top-level definition" $ do
        let valueTy = ty "Value"
            local = Var "shadowed" (Unique 10) valueTy
            global = Var "shadowed" (Unique 11) valueTy
            mainVar = Var "main" (Unique 12) (TcFunTy valueTy valueTy)
            program =
              FcProgram
                [ FcTopBind (FcNonRec global (FcVar global)),
                  FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))
                ]
        assertEqual
          "reachable program"
          (FcProgram [FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))])
          (eliminateDeadCode "main" program),
      testCase "lowers newtype construction to a linted representational cast" $ do
        let metersTy = ty "Meters"
            intHashTy = ty "Int#"
            declaration =
              FcNewtypeDecl
                { fcNewtypeName = "Meters",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructor = "Meters",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = metersTy
                }
            constructor = Var "Meters" (Unique 20) (TcFunTy intHashTy metersTy)
            value = Var "value" (Unique 21) metersTy
            source =
              FcProgram
                [ FcNewtype declaration,
                  FcTopBind (FcNonRec value (FcApp (FcVar constructor) (FcLit (LitInt IntRep 42))))
                ]
            lowered = lowerNewtypes source
        assertEqual "idempotent lowering" lowered (lowerNewtypes lowered)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv lowered)
        result <- evalProgramBinding "value" lowered >>= renderEvalResult
        assertEqual "runtime representation" (Right "42") result,
      testCase "lowers dependency newtypes without merging separate units" $ do
        let wrapperTy = ty "Wrapper"
            intHashTy = ty "Int#"
            declaration =
              FcNewtypeDecl
                { fcNewtypeName = "Wrapper",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructor = "Wrap",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = wrapperTy
                }
            constructor = Var "Wrap" (Unique 30) (TcFunTy intHashTy wrapperTy)
            value = Var "value" (Unique 31) wrapperTy
            literal = FcLit (LitInt IntRep 42)
            provider = FcProgram [FcNewtype declaration]
            consumer = FcProgram [FcTopBind (FcNonRec value (FcApp (FcVar constructor) literal))]
            loweredConsumer = FcProgram [FcTopBind (FcNonRec value (FcCast literal (Sym (AxiomInstCo "Wrapper" []))))]
        assertEqual
          "consumer body"
          loweredConsumer
          (lowerNewtypesWithInterface (extractNewtypeInterface provider) consumer)
    ]

fcEvalFixtureTests :: IO TestTree
fcEvalFixtureTests = do
  cases <- EvalGolden.loadEvalCases
  let tests = map mkEvalFixtureTest cases
  pure (testGroup "shared evaluation fixtures via FC" tests)

mkEvalFixtureTest :: EvalGolden.EvalCase -> TestTree
mkEvalFixtureTest tc = testCase (EvalGolden.evalCaseId tc) $ do
  (outcome, details) <- EvalGolden.evaluateEvalCase evaluateFcProgram tc
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

evaluateFcProgram :: Text -> FcProgram -> IO (Either String Text)
evaluateFcProgram name program = do
  result <- evalProgramBinding name program
  case result of
    Left err -> pure (Left (show err))
    Right value -> do
      rendered <- renderRawValue value
      pure $
        case rendered of
          Left err -> Left (show err)
          Right text -> Right text

assertEvalExpr :: Text -> FcExpr -> IO ()
assertEvalExpr expected expr = do
  result <- evalExpr expr >>= renderEvalResult
  assertEqual "result" (Right expected) result

renderEvalResult :: Either EvalError Value -> IO (Either EvalError Text)
renderEvalResult result =
  case result of
    Left err -> pure (Left err)
    Right value -> renderValue value

var :: Text -> TcType -> Var
var name = Var name (Unique 0)

stringTy :: TcType
stringTy = TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []]

ty :: Text -> TcType
ty name = TcTyCon (TyCon name 0) []
