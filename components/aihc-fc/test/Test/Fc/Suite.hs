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
import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Tc (RuntimeRep (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.Text (Text)
import FcGolden hiding (FcCase)
import FcGolden qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

-- | Build the golden test tree from fixtures.
fcGoldenTests :: IO TestTree
fcGoldenTests = do
  cases <- loadFcCases
  let tests = map mkTest cases
  pure (testGroup "FC golden tests" tests)

mkTest :: FcGolden.FcCase -> TestTree
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
      testCase "records Addr# literal type and representation" $ do
        let literal = LitAddr "hello"
        assertEqual "runtime representation" AddrRep (literalRuntimeRep literal)
        assertEqual "literal type" (Just (TcTyCon (TyCon "Addr#" 0) [])) (literalType literal),
      testCase "keeps free rigid variables in first-occurrence order" $ do
        let first = TyVarId "first" (Unique 1)
            second = TyVarId "second" (Unique 2)
        assertEqual "ordered variables" [first, second] (freeRigidTyVarsOf [TcTyVar first, TcTyVar second, TcTyVar first]),
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
          result,
      testCase "evaluates wide Word# multiplication" $ do
        let wordTy = ty "Word#"
            primitive = Var "timesWord2#" (Unique 100) (TcFunTy wordTy (TcFunTy wordTy wordTy))
            result = Var "wideProduct" (Unique 101) wordTy
            expression =
              FcApp
                (FcApp (FcVar primitive) (FcLit (LitInt WordRep 0xffffffffffffffff)))
                (FcLit (LitInt WordRep 2))
            program = FcProgram [FcPrimitive primitive 2, FcTopBind (FcNonRec result expression)]
        actual <- evalProgramBinding "wideProduct" program >>= traverse renderRawValue
        assertEqual "high and low words" (Right (Right "(1,18446744073709551614)")) actual
    ]

fcOptimizationTests :: TestTree
fcOptimizationTests =
  testGroup
    "FC optimizations"
    [ testCase "copy propagates simple non-recursive lets" $ do
        let value = Var "value" (Unique 1) stringTy
            alias = Var "alias" (Unique 2) stringTy
            consume = Var "consume" (Unique 3) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 4) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcNonRec alias (FcVar value)) (FcApp (FcVar consume) (FcVar alias))))]
            expected = FcProgram [FcTopBind (FcNonRec result (FcApp (FcVar consume) (FcVar value)))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "runs the Core optimization set to a fixpoint" $ do
        let value = Var "value" (Unique 5) stringTy
            outer = Var "outer" (Unique 6) stringTy
            inner = Var "inner" (Unique 7) stringTy
            result = Var "result" (Unique 8) stringTy
            source =
              FcProgram
                [ FcTopBind
                    ( FcNonRec
                        result
                        ( FcLet
                            (FcNonRec outer (FcLet (FcNonRec inner (FcVar value)) (FcVar inner)))
                            (FcVar outer)
                        )
                    )
                ]
            expected = FcProgram [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "copy propagates non-recursive singleton rec groups" $ do
        let value = Var "value" (Unique 13) stringTy
            alias = Var "alias" (Unique 14) stringTy
            result = Var "result" (Unique 15) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar value)]) (FcVar alias)))]
            expected = FcProgram [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "retains genuinely recursive singleton aliases" $ do
        let alias = Var "alias" (Unique 16) stringTy
            result = Var "result" (Unique 17) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar alias)]) (FcVar alias)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "retains non-trivial let right-hand sides" $ do
        let value = Var "value" (Unique 9) stringTy
            binder = Var "computed" (Unique 10) stringTy
            identity = Var "identity" (Unique 11) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 12) stringTy
            computed = FcApp (FcVar identity) (FcVar value)
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcNonRec binder computed) (FcVar binder)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "inlines a direct function at every call site" $ do
        let argument = Var "argument" (Unique 20) stringTy
            identity = Var "identity" (Unique 21) (TcFunTy stringTy stringTy)
            firstValue = Var "firstValue" (Unique 22) stringTy
            secondValue = Var "secondValue" (Unique 23) stringTy
            first = Var "first" (Unique 24) stringTy
            second = Var "second" (Unique 25) stringTy
            source =
              FcProgram
                [ FcTopBind (FcNonRec identity (FcLam argument (FcVar argument))),
                  FcTopBind (FcNonRec first (FcApp (FcVar identity) (FcVar firstValue))),
                  FcTopBind (FcNonRec second (FcApp (FcVar identity) (FcVar secondValue)))
                ]
            optimized = optimizeProgram source
        assertEqual "first call" (Just (FcVar firstValue)) (topBindingRhs "first" optimized)
        assertEqual "second call" (Just (FcVar secondValue)) (topBindingRhs "second" optimized),
      testCase "preserves sharing when an inlined argument is used twice" $ do
        let pairTy = ty "Pair"
            pair = Var "Pair" (Unique 30) (TcFunTy stringTy (TcFunTy stringTy pairTy))
            expensive = Var "expensive" (Unique 31) (TcFunTy stringTy stringTy)
            sourceValue = Var "source" (Unique 32) stringTy
            duplicate = Var "duplicate" (Unique 33) (TcFunTy stringTy pairTy)
            argument = Var "argument" (Unique 34) stringTy
            mainVar = Var "main" (Unique 35) pairTy
            duplicateRhs = FcLam argument (FcApp (FcApp (FcVar pair) (FcVar argument)) (FcVar argument))
            computed = FcApp (FcVar expensive) (FcVar sourceValue)
            program =
              FcProgram
                [ FcData "Pair" [] [("Pair", [stringTy, stringTy])],
                  FcPrimitive expensive 1,
                  FcTopBind (FcNonRec duplicate duplicateRhs),
                  FcTopBind (FcNonRec mainVar (FcApp (FcVar duplicate) computed))
                ]
            mainRhs = topBindingRhs "main" (optimizeProgram program)
        assertEqual "callee removed" 0 (maybe 0 (countVariableName "duplicate") mainRhs)
        assertEqual "argument computation retained once" 1 (maybe 0 (countVariableName "expensive") mainRhs)
        assertEqual "shared through a let" True (maybe False containsNonRecursiveLet mainRhs),
      testCase "does not inline a computed CAF" $ do
        let expensive = Var "expensive" (Unique 40) (TcFunTy stringTy stringTy)
            sourceValue = Var "source" (Unique 41) stringTy
            cached = Var "cached" (Unique 42) stringTy
            mainVar = Var "main" (Unique 43) stringTy
            program =
              FcProgram
                [ FcPrimitive expensive 1,
                  FcTopBind (FcNonRec cached (FcApp (FcVar expensive) (FcVar sourceValue))),
                  FcTopBind (FcNonRec mainVar (FcVar cached))
                ]
        assertEqual "computed thunk retained" program (optimizeProgram program),
      testCase "keeps an unused unlifted argument strict" $ do
        let intHashTy = ty "Int#"
            unitTy = ty "Unit"
            force = Var "force" (Unique 50) (TcFunTy intHashTy intHashTy)
            sourceValue = Var "source" (Unique 51) intHashTy
            ignore = Var "ignore" (Unique 52) (TcFunTy intHashTy unitTy)
            argument = Var "argument" (Unique 53) intHashTy
            unitValue = Var "()" (Unique 54) unitTy
            mainVar = Var "main" (Unique 55) unitTy
            forcedArgument = FcApp (FcVar force) (FcVar sourceValue)
            program =
              FcProgram
                [ FcData "Unit" [] [("()", [])],
                  FcPrimitive force 1,
                  FcTopBind (FcNonRec ignore (FcLam argument (FcVar unitValue))),
                  FcTopBind (FcNonRec mainVar (FcApp (FcVar ignore) forcedArgument))
                ]
        case topBindingRhs "main" (optimizeProgram program) of
          Just (FcCase scrutinee _ [FcAlt DefaultAlt [] (FcVar result)]) -> do
            assertEqual "strict argument" forcedArgument scrutinee
            assertEqual "result" unitValue result
          other -> assertFailure ("expected a strict case binding, got: " <> show other),
      testCase "eliminates repeated dictionary-dispatched binds" $ do
        let unitTy = ty "Unit"
            dictionaryTy = ty "MonadBox"
            continuationTy = TcFunTy unitTy unitTy
            methodTy = TcFunTy unitTy (TcFunTy continuationTy unitTy)
            constructor = Var "$Dict$MonadBox" (Unique 60) (TcFunTy methodTy dictionaryTy)
            selector = Var ">>=" (Unique 61) (TcFunTy dictionaryTy methodTy)
            dictionary = Var "$fMonadBox" (Unique 62) dictionaryTy
            implementation = Var "bindBox" (Unique 63) methodTy
            dictionaryArgument = Var "$dictionary" (Unique 64) dictionaryTy
            caseBinder = Var "$case" (Unique 65) dictionaryTy
            selectedMethod = Var "$method" (Unique 66) methodTy
            valueArgument = Var "value" (Unique 67) unitTy
            continuation = Var "continuation" (Unique 68) continuationTy
            ignoredFirst = Var "ignoredFirst" (Unique 69) unitTy
            ignoredSecond = Var "ignoredSecond" (Unique 70) unitTy
            firstAction = Var "firstAction" (Unique 71) unitTy
            secondAction = Var "secondAction" (Unique 72) unitTy
            finalAction = Var "finalAction" (Unique 73) unitTy
            mainVar = Var "main" (Unique 74) unitTy
            selectorRhs =
              FcLam
                dictionaryArgument
                ( FcCase
                    (FcVar dictionaryArgument)
                    caseBinder
                    [FcAlt (DataAlt "$Dict$MonadBox") [selectedMethod] (FcVar selectedMethod)]
                )
            implementationRhs =
              FcLam valueArgument (FcLam continuation (FcApp (FcVar continuation) (FcVar valueArgument)))
            callBind action = FcApp (FcApp (FcApp (FcVar selector) (FcVar dictionary)) action)
            mainRhs =
              callBind
                (FcVar firstAction)
                ( FcLam
                    ignoredFirst
                    ( callBind
                        (FcVar secondAction)
                        (FcLam ignoredSecond (FcVar finalAction))
                    )
                )
            program =
              FcProgram
                [ FcData "Unit" [] [("()", [])],
                  FcData "MonadBox" [] [("$Dict$MonadBox", [methodTy])],
                  FcTopBind (FcNonRec selector selectorRhs),
                  FcTopBind (FcNonRec implementation implementationRhs),
                  FcTopBind (FcNonRec dictionary (FcApp (FcVar constructor) (FcVar implementation))),
                  FcTopBind (FcNonRec mainVar mainRhs)
                ]
            optimizedMain = topBindingRhs "main" (optimizeProgram program)
        assertEqual "bind chain" (Just (FcVar finalAction)) optimizedMain
        assertEqual "selector eliminated" 0 (maybe 0 (countVariableName ">>=") optimizedMain)
        assertEqual "dictionary eliminated" 0 (maybe 0 (countVariableName "$fMonadBox") optimizedMain)
        assertEqual "implementation eliminated" 0 (maybe 0 (countVariableName "bindBox") optimizedMain),
      testCase "eliminates values and types unreachable from the entry point" $ do
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
      testCase "retains ordinary dictionary constructor declarations" $ do
        let dictionaryTy = ty "Test"
            dictionaryData = FcData "Test" [] [("$Dict$Test", [stringTy])]
            dictionaryConstructor = Var "$Dict$Test" (Unique 14) (TcFunTy stringTy dictionaryTy)
            dictionaryBinder = Var "$dictionary" (Unique 15) dictionaryTy
            methodBinder = Var "$method" (Unique 16) stringTy
            mainBinding =
              FcTopBind
                ( FcNonRec
                    (Var "main" (Unique 13) stringTy)
                    ( Aihc.Fc.FcCase
                        (FcApp (FcVar dictionaryConstructor) (FcLit (LitString "method")))
                        dictionaryBinder
                        [FcAlt (DataAlt "$Dict$Test") [methodBinder] (FcVar methodBinder)]
                    )
                )
            program = FcProgram [dictionaryData, mainBinding]
        assertEqual
          "reachable dictionary declaration"
          program
          (eliminateDeadCode "main" program)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv program),
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

topBindingRhs :: Text -> FcProgram -> Maybe FcExpr
topBindingRhs name (FcProgram topBinds) =
  case [ rhs
       | FcTopBind (FcNonRec binder rhs) <- topBinds,
         varName binder == name
       ] of
    rhs : _ -> Just rhs
    [] -> Nothing

countVariableName :: Text -> FcExpr -> Int
countVariableName name expression =
  case expression of
    FcVar variable -> fromEnum (varName variable == name)
    FcLit {} -> 0
    FcApp function argument -> countVariableName name function + countVariableName name argument
    FcTyApp function _ -> countVariableName name function
    FcLam _ body -> countVariableName name body
    FcTyLam _ body -> countVariableName name body
    FcLet (FcNonRec _ rhs) body -> countVariableName name rhs + countVariableName name body
    FcLet (FcRec bindings) body -> sum (map (countVariableName name . snd) bindings) + countVariableName name body
    FcCase scrutinee _ alternatives ->
      countVariableName name scrutinee + sum (map (countVariableName name . altRhs) alternatives)
    FcCast body _ -> countVariableName name body
    FcCallForeign _ arguments -> sum (map (countVariableName name) arguments)

containsNonRecursiveLet :: FcExpr -> Bool
containsNonRecursiveLet expression =
  case expression of
    FcVar {} -> False
    FcLit {} -> False
    FcApp function argument -> containsNonRecursiveLet function || containsNonRecursiveLet argument
    FcTyApp function _ -> containsNonRecursiveLet function
    FcLam _ body -> containsNonRecursiveLet body
    FcTyLam _ body -> containsNonRecursiveLet body
    FcLet FcNonRec {} _ -> True
    FcLet (FcRec bindings) body -> any (containsNonRecursiveLet . snd) bindings || containsNonRecursiveLet body
    FcCase scrutinee _ alternatives ->
      containsNonRecursiveLet scrutinee || any (containsNonRecursiveLet . altRhs) alternatives
    FcCast body _ -> containsNonRecursiveLet body
    FcCallForeign _ arguments -> any containsNonRecursiveLet arguments

var :: Text -> TcType -> Var
var name = Var name (Unique 0)

stringTy :: TcType
stringTy = TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []]

ty :: Text -> TcType
ty name = TcTyCon (TyCon name 0) []
