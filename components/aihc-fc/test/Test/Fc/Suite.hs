{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcDesugarTests,
    fcEvalTests,
    fcEvalFixtureTests,
    fcLoweringTests,
    fcMainTests,
    fcMergeTests,
    fcOptimizationTests,
  )
where

import Aihc.Fc
import Aihc.Fc qualified as Fc
import Aihc.Fc.Desugar.Expr (DsState (dsModuleName))
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax qualified as Surface
import Aihc.Resolve (PackageId (..), ResolveResult (..), resolveWithDeps, unnamedPackage)
import Aihc.Tc (Kind (KType), RuntimeRep (..), TcInterface (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), emptyTcInterface, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithInterface)
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (tyConModuleName, tyConPackageId)
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import FcGolden
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

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

fcDesugarTests :: TestTree
fcDesugarTests =
  testGroup
    "FC desugaring"
    [ testCase "requires a module name in the desugar state" $ do
        let requireModuleName :: DsState -> Text
            requireModuleName = dsModuleName
        requireModuleName `seq` pure (),
      testCase "counts every label in a grouped record field" $ do
        let (_, parsedModule) = parseModule defaultConfig "module Test where\ndata Pair a = Pair { left, right :: a }\n"
            constructors = concatMap declarationConstructors (Surface.moduleDecls parsedModule)
        case constructors of
          [constructor] -> assertEqual "constructor arity" ("Pair", 2) (dsDataConPure constructor)
          _ -> assertFailure ("expected one parsed constructor, got: " <> show constructors),
      testCase "stock Eq dictionaries pass Core lint" $ do
        let source =
              "{-# LANGUAGE DerivingStrategies #-}\n\
              \module Test where\n\
              \data Bool = False | True\n\
              \class Eq a where\n\
              \  (==) :: a -> a -> Bool\n\
              \  (/=) :: a -> a -> Bool\n\
              \data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving stock Eq\n"
            config = defaultConfig {parserExtensions = [Surface.DerivingStrategies]}
            (parseErrors, parsedModule) = parseModule config source
        assertEqual "source parses" [] parseErrors
        (tcModule, tcInterface) <- resolveAndTypecheck parsedModule
        let result =
              desugarModuleWithDataTypes
                (DesugarConfig {primPackageId = PackageId "aihc-prim"})
                (tcModuleBindings tcModule)
                (tcInterfaceDataTypes tcInterface)
                tcModule
        assertBool ("desugaring succeeds: " <> show (dsErrors result)) (dsSuccess result)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv (dsProgram result)),
      testCase "uses the aihc-prim Bool type for if binders" $ do
        let source =
              "module Test where\n\
              \data Bool = False | True\n\
              \value = if True then True else False\n"
            (parseErrors, parsedModule) = parseModule defaultConfig source
        assertEqual "source parses" [] parseErrors
        (tcModule, tcInterface) <- resolveAndTypecheck parsedModule
        let result =
              desugarModuleWithDataTypes
                (DesugarConfig {primPackageId = PackageId "aihc-prim"})
                (tcModuleBindings tcModule)
                (tcInterfaceDataTypes tcInterface)
                tcModule
            ifBinderTypes =
              [ varType binder
              | FcTopBind (FcNonRec _ (Fc.FcCase _ binder _)) <- fcTopBinds (dsProgram result)
              ]
        assertBool ("desugaring succeeds: " <> show (dsErrors result)) (dsSuccess result)
        case ifBinderTypes of
          [TcTyCon tyCon []] -> do
            assertEqual "package ID" (PackageId "aihc-prim") (tyConPackageId tyCon)
            assertEqual "module name" "GHC.Types" (tyConModuleName tyCon)
          other -> assertFailure ("expected one Bool case binder, got: " <> show other)
    ]
  where
    declarationConstructors declaration =
      case declaration of
        Surface.DeclAnn _ inner -> declarationConstructors inner
        Surface.DeclData dataDeclaration -> Surface.dataDeclConstructors dataDeclaration
        _ -> []

resolveAndTypecheck :: Surface.Module -> IO (Surface.Module, TcInterface)
resolveAndTypecheck parsedModule =
  case resolveWithDeps mempty [(unnamedPackage, parsedModule)] of
    ResolveResult {resolvedModules = [(_, resolvedModule)], resolveErrors = []} ->
      case typecheckModulesWithInterface emptyTcInterface [resolvedModule] of
        ([tcModule], tcInterface)
          | tcModuleSuccess tcModule -> pure (tcModule, tcInterface)
          | otherwise -> assertFailure ("type-check errors: " <> show (tcModuleDiagnostics tcModule))
        (tcModules, _) -> assertFailure ("unexpected type-check module count: " <> show (length tcModules))
    ResolveResult {resolveErrors} -> assertFailure ("resolve errors: " <> show resolveErrors)

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
      testCase "binds evaluated case scrutinees in alternatives" $
        let binder = var "evaluated" (ty "Int#")
         in assertEvalExpr
              "42"
              (Aihc.Fc.FcCase (FcLit (LitInt IntRep 42)) binder [FcAlt DefaultAlt [] (FcVar binder)]),
      testCase "evaluates top-level bindings" $
        let program =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    (FcNonRec (var "answer" stringTy) (FcLit (LitString "top")))
                ]
         in do
              result <- evalProgramBinding "answer" program >>= renderEvalResult
              assertEqual "result" (Right "\"top\"") result,
      testCase "binds a case scrutinee for the alternative result" $
        let caseBinder = Var "scrutinee" (Unique 1) stringTy
            result = Var "answer" (Unique 2) stringTy
            program =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    ( FcNonRec
                        result
                        (Aihc.Fc.FcCase (FcLit (LitString "forced")) caseBinder [FcAlt DefaultAlt [] (FcVar caseBinder)])
                    )
                ]
         in do
              actual <- evalProgramBinding "answer" program >>= renderEvalResult
              assertEqual "case result" (Right "\"forced\"") actual,
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
            program = FcProgram (FcModuleId "test" "Test") [FcPrimitive primitive 2, FcTopBind (FcNonRec result expression)]
        actual <- evalProgramBinding "wideProduct" program >>= traverse renderRawValue
        assertEqual "high and low words" (Right (Right "(1,18446744073709551614)")) actual
    ]

fcMergeTests :: TestTree
fcMergeTests =
  testGroup
    "FC module merge"
    [ testCase "keeps equal source names from different modules separate" $ do
        let valueType = stringTy
            secondOrigin = FcTopLevelOrigin "pkg-b" "Data.Sequence" "toList"
            imported = (Var "toList" (Unique 30) valueType) {varResolvedName = Just secondOrigin}
            first = FcProgram (FcModuleId "pkg-a" "Data.Foldable") [FcTopBind (FcNonRec (Var "toList" (Unique 1) valueType) (FcLit (LitString "first")))]
            second = FcProgram (FcModuleId "pkg-b" "Data.Sequence") [FcTopBind (FcNonRec (Var "toList" (Unique 1) valueType) (FcLit (LitString "second")))]
            consumer =
              FcProgram
                (FcModuleId "exe" "Consumer")
                [ FcExternal secondOrigin valueType,
                  FcTopBind (FcNonRec (Var "result" (Unique 1) valueType) (FcVar imported))
                ]
        case mergePrograms (FcModuleId "exe" "Main") (first :| [second, consumer]) of
          Left errors -> assertFailure (show errors)
          Right merged -> do
            let binderNames = [varName binder | FcTopBind bind <- fcTopBinds merged, binder <- binders bind]
            assertBool "first global name is present" (any ("pkg_2d_a$Data_2e_Foldable$toList" `Text.isInfixOf`) binderNames)
            assertBool "second global name is present" (any ("pkg_2d_b$Data_2e_Sequence$toList" `Text.isInfixOf`) binderNames)
            assertEqual "resolved imports" [] [origin | FcExternal origin _ <- fcTopBinds merged, origin == secondOrigin]
            result <- evalProgramBinding "result" merged >>= renderEvalResult
            assertEqual "selected definition" (Right "\"second\"") result,
      testCase "keeps an import without a provider" $ do
        let origin = FcTopLevelOrigin "missing" "Module" "value"
            valueType = stringTy
            imported = fcExternalVar origin valueType
            consumer = FcProgram (FcModuleId "exe" "Consumer") [FcExternal origin valueType, FcTopBind (FcNonRec (Var "result" (Unique 1) valueType) (FcVar imported))]
        case mergePrograms (FcModuleId "exe" "Main") (consumer :| []) of
          Left errors -> assertFailure (show errors)
          Right merged -> assertEqual "unresolved import" [origin] [externalOrigin | FcExternal externalOrigin _ <- fcTopBinds merged],
      testCase "requires one module declaration during parsing" $ do
        assertBool "missing module declaration" (isLeft (parseProgram "value : Int =\n  1"))
        assertBool
          "multiple module declarations"
          (isLeft (parseProgram "module \"one\" One where\n\nmodule \"two\" Two where")),
      testCase "parses the module package as a package ID" $
        case parseProgram "module \"example\" Example where" of
          Left err -> assertFailure (renderParseError err)
          Right program -> assertEqual "package ID" (PackageId "example") (fcModulePackage (fcProgramModule program))
    ]
  where
    binders bind =
      case bind of
        FcNonRec binder _ -> [binder]
        FcRec bindings -> map fst bindings
    isLeft result =
      case result of
        Left _ -> True
        Right _ -> False

fcLoweringTests :: TestTree
fcLoweringTests =
  testGroup
    "FC compulsory lowering"
    [ testCase "expands saturated seq to a case on its first argument" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            second = Var "second" (Unique 2) boolTy
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    (FcNonRec result (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcVar second)))
                ]
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcVar second)]))]
        assertEqual "lowered program" expected (lowerPseudoOps source),
      testCase "expands partially applied seq to an explicit lambda" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            seqVar = Var "$aihc.seq" (Unique 2) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "forceFirst" (Unique 3) (TcFunTy boolTy boolTy)
            second = Var "$seq_second" (Unique 4) boolTy
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcApp (FcVar seqVar) (FcVar first)))]
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLam second (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcVar second)])))]
        assertEqual "lowered program" expected (lowerPseudoOps source),
      testCase "uses the evaluated case binder for later references to the first argument" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            consume = Var "consume" (Unique 2) (TcFunTy stringTy boolTy)
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    ( FcNonRec
                        result
                        (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcApp (FcVar consume) (FcVar first)))
                    )
                ]
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            expected =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    ( FcNonRec
                        result
                        (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcApp (FcVar consume) (FcVar caseBinder))])
                    )
                ]
        assertEqual "lowered program" expected (lowerPseudoOps source)
    ]

fcOptimizationTests :: TestTree
fcOptimizationTests =
  testGroup
    "FC optional optimizations"
    [ testCase "does not perform compulsory pseudo-op lowering" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            second = Var "second" (Unique 2) boolTy
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    (FcNonRec result (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcVar second)))
                ]
        assertEqual "optimization leaves pseudo-op intact" source (optimizeProgram source),
      testCase "copy propagates simple non-recursive lets" $ do
        let value = Var "value" (Unique 1) stringTy
            alias = Var "alias" (Unique 2) stringTy
            consume = Var "consume" (Unique 3) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 4) stringTy
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcNonRec alias (FcVar value)) (FcApp (FcVar consume) (FcVar alias))))]
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcApp (FcVar consume) (FcVar value)))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "runs the Core optimization set to a fixpoint" $ do
        let value = Var "value" (Unique 5) stringTy
            outer = Var "outer" (Unique 6) stringTy
            inner = Var "inner" (Unique 7) stringTy
            result = Var "result" (Unique 8) stringTy
            source =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind
                    ( FcNonRec
                        result
                        ( FcLet
                            (FcNonRec outer (FcLet (FcNonRec inner (FcVar value)) (FcVar inner)))
                            (FcVar outer)
                        )
                    )
                ]
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "copy propagates non-recursive singleton rec groups" $ do
        let value = Var "value" (Unique 13) stringTy
            alias = Var "alias" (Unique 14) stringTy
            result = Var "result" (Unique 15) stringTy
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar value)]) (FcVar alias)))]
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "retains genuinely recursive singleton aliases" $ do
        let alias = Var "alias" (Unique 16) stringTy
            result = Var "result" (Unique 17) stringTy
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar alias)]) (FcVar alias)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "retains non-trivial let right-hand sides" $ do
        let value = Var "value" (Unique 9) stringTy
            binder = Var "computed" (Unique 10) stringTy
            identity = Var "identity" (Unique 11) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 12) stringTy
            computed = FcApp (FcVar identity) (FcVar value)
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcNonRec binder computed) (FcVar binder)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "eliminates unused lifted non-recursive lets" $ do
        let value = Var "value" (Unique 17) stringTy
            unused = Var "unused" (Unique 18) stringTy
            result = Var "result" (Unique 19) stringTy
            compute = Var "compute" (Unique 20) (TcFunTy stringTy stringTy)
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcNonRec unused (FcApp (FcVar compute) (FcVar value))) (FcLit (LitString "result"))))]
            expected = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLit (LitString "result")))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "retains unused unlifted non-recursive lets" $ do
        let intHashTy = ty "Int#"
            value = Var "value" (Unique 21) intHashTy
            unused = Var "unused" (Unique 22) intHashTy
            result = Var "result" (Unique 23) stringTy
            compute = Var "compute" (Unique 24) (TcFunTy intHashTy intHashTy)
            source = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec result (FcLet (FcNonRec unused (FcApp (FcVar compute) (FcVar value))) (FcLit (LitString "result"))))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "eliminates values and types unreachable from the entry point" $ do
        let liveTy = ty "Live"
            leafTy = ty "Leaf"
            deadTy = ty "Dead"
            mainVar = Var "main" (Unique 1) liveTy
            helperVar = Var "helper" (Unique 2) liveTy
            deadVar = Var "dead" (Unique 3) deadTy
            liveData = fcData "Live" [] [("Live", [leafTy])]
            leafData = fcData "Leaf" [] [("Leaf", [])]
            deadData = fcData "Dead" [] [("Dead", [])]
            helper = FcTopBind (FcNonRec helperVar (FcVar (Var "Live" (Unique 4) (TcFunTy leafTy liveTy))))
            mainBinding = FcTopBind (FcNonRec mainVar (FcVar helperVar))
            deadBinding = FcTopBind (FcNonRec deadVar (FcVar (Var "Dead" (Unique 5) deadTy)))
            program = FcProgram (FcModuleId "test" "Test") [deadData, deadBinding, leafData, liveData, helper, mainBinding]
        assertEqual
          "reachable program"
          (FcProgram (FcModuleId "test" "Test") [leafData, liveData, helper, mainBinding])
          (eliminateDeadCode "main" program),
      testCase "does not confuse a local binder with a top-level definition" $ do
        let valueTy = ty "Value"
            local = Var "shadowed" (Unique 10) valueTy
            global = Var "shadowed" (Unique 11) valueTy
            mainVar = Var "main" (Unique 12) (TcFunTy valueTy valueTy)
            program =
              FcProgram
                (FcModuleId "test" "Test")
                [ FcTopBind (FcNonRec global (FcVar global)),
                  FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))
                ]
        assertEqual
          "reachable program"
          (FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))])
          (eliminateDeadCode "main" program),
      testCase "retains ordinary dictionary constructor declarations" $ do
        let dictionaryDeclaration =
              FcDataDecl
                (FcTopLevelOrigin "test" "Test" "Test")
                "Test"
                []
                KType
                [FcDataConDecl (FcTopLevelOrigin "test" "Test" "$Dict$Test") "$Dict$Test" [stringTy]]
            dictionaryTy = fcDataResultType dictionaryDeclaration
            dictionaryData = FcData dictionaryDeclaration
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
            program = FcProgram (FcModuleId "test" "Test") [dictionaryData, mainBinding]
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
                { fcNewtypeOrigin = FcTopLevelOrigin "test" "Test" "Meters",
                  fcNewtypeName = "Meters",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructorOrigin = FcTopLevelOrigin "test" "Test" "Meters",
                  fcNewtypeConstructor = "Meters",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = metersTy
                }
            constructor = Var "Meters" (Unique 20) (TcFunTy intHashTy metersTy)
            value = Var "value" (Unique 21) metersTy
            source =
              FcProgram
                (FcModuleId "test" "Test")
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
                { fcNewtypeOrigin = FcTopLevelOrigin "test" "Test" "Wrapper",
                  fcNewtypeName = "Wrapper",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructorOrigin = FcTopLevelOrigin "test" "Test" "Wrap",
                  fcNewtypeConstructor = "Wrap",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = wrapperTy
                }
            constructor = Var "Wrap" (Unique 30) (TcFunTy intHashTy wrapperTy)
            value = Var "value" (Unique 31) wrapperTy
            literal = FcLit (LitInt IntRep 42)
            provider = FcProgram (FcModuleId "test" "Test") [FcNewtype declaration]
            consumer = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec value (FcApp (FcVar constructor) literal))]
            loweredConsumer = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec value (FcCast literal (Sym (AxiomInstCo "Wrapper" []))))]
        assertEqual
          "consumer body"
          loweredConsumer
          (lowerNewtypesWithInterface (extractNewtypeInterface provider) consumer)
        assertEqual
          "consumer lint"
          []
          (lintProgramWithAxiomInterface (extractAxiomInterface provider) emptyLintEnv loweredConsumer),
      testCase "lints explicit equality axioms" $ do
        let familyTy = ty "Family"
            representationTy = ty "Int#"
            declaration =
              FcAxiomDecl
                { fcAxiomName = "axFamily",
                  fcAxiomTyVars = [],
                  fcAxiomRole = FcNominal,
                  fcAxiomLeft = familyTy,
                  fcAxiomRight = representationTy
                }
            value = Var "main" (Unique 40) familyTy
            binding = FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" []))))
            program = FcProgram (FcModuleId "test" "Test") [FcAxiom declaration, binding]
        assertEqual "Core lint" [] (lintProgram emptyLintEnv program)
        assertEqual "reachable axiom" program (eliminateDeadCode "main" program)
        assertEqual
          "pretty axiom"
          "axiom axFamily : Family ~N Int#"
          (renderTopBind (FcAxiom declaration)),
      testCase "imports equality axioms across compilation units" $ do
        let parameter = TyVarId "a" (Unique 42)
            representationTy = ty "Int#"
            familyTy argument = TcTyCon (TyCon "Family" 1) [argument]
            declaration =
              FcAxiomDecl
                { fcAxiomName = "axFamily",
                  fcAxiomTyVars = [parameter],
                  fcAxiomRole = FcRepresentational,
                  fcAxiomLeft = familyTy (TcTyVar parameter),
                  fcAxiomRight = TcTyVar parameter
                }
            value = Var "main" (Unique 41) (familyTy representationTy)
            provider = FcProgram (FcModuleId "test" "Test") [FcAxiom declaration]
            consumer = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" [representationTy]))))]
            wrongArity = FcProgram (FcModuleId "test" "Test") [FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" []))))]
            interface = extractAxiomInterface provider
        assertBool "consumer needs imported axiom" (not (null (lintProgram emptyLintEnv consumer)))
        assertBool "axiom arity is checked" (not (null (lintProgramWithAxiomInterface interface emptyLintEnv wrongArity)))
        assertEqual "consumer lint" [] (lintProgramWithAxiomInterface interface emptyLintEnv consumer)
        assertEqual "serialized interface" interface (read (show interface))
    ]

fcMainTests :: TestTree
fcMainTests =
  testGroup
    "FC executable entry point"
    [ testCase "calls GHC.TopHandler.runMainIO with Main.main" $ do
        let unitType = ty "Unit"
            mainType = TcTyCon (TyCon "IO" 1) [unitType]
            actionOrigin = FcTopLevelOrigin "test-base" "Test.Runtime" "action"
            actionVar = fcExternalVar actionOrigin mainType
            mainVar = Var "main" (Unique 1) mainType
            runMainOrigin = FcTopLevelOrigin "test-base" "GHC.TopHandler" "runMainIO"
            source =
              FcProgram
                (FcModuleId "" "Main")
                [ FcExternal actionOrigin mainType,
                  FcTopBind (FcNonRec mainVar (FcVar actionVar))
                ]
        case addMainEntrypoint runMainOrigin source of
          Left err -> assertFailure (show err)
          Right program@(FcProgram _ topBinds) -> do
            assertEqual "Core lint" [] (lintProgram emptyLintEnv program)
            case reverse topBinds of
              FcTopBind (FcNonRec entryVar (FcApp (FcTyApp (FcVar runMainVar) typeArgument) (FcVar calledMain))) : FcExternal externalOrigin externalType : _ -> do
                assertEqual "entry name" mainEntryBindingName (varName entryVar)
                assertEqual "entry type" mainType (varType entryVar)
                assertEqual "type argument" unitType typeArgument
                assertEqual "called main" mainVar calledMain
                assertEqual "runMainIO origin" runMainOrigin externalOrigin
                assertEqual "runMainIO occurrence origin" (Just runMainOrigin) (varResolvedName runMainVar)
                assertEqual "runMainIO type" externalType (varType runMainVar)
              _ -> assertFailure ("unexpected entry point: " <> show topBinds),
      testCase "requires the Main module" $ do
        let mainType = TcTyCon (TyCon "IO" 1) [ty "Unit"]
            program = FcProgram (FcModuleId "" "Other") [FcTopBind (FcNonRec (Var "main" (Unique 1) mainType) (FcLit (LitString "unused")))]
        assertEqual "entry point error" (Left MainModuleMissing) (addMainEntrypoint testRunMainOrigin program),
      testCase "requires an IO main binding" $ do
        let valueType = ty "Value"
            program = FcProgram (FcModuleId "" "Main") [FcTopBind (FcNonRec (Var "main" (Unique 1) valueType) (FcLit (LitString "unused")))]
        assertEqual "entry point error" (Left (MainBindingNotIO valueType)) (addMainEntrypoint testRunMainOrigin program)
    ]
  where
    testRunMainOrigin = FcTopLevelOrigin "test-base" "GHC.TopHandler" "runMainIO"

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

fcData :: Text -> [TyVarId] -> [(Text, [TcType])] -> FcTopBind
fcData dataName tyVars constructors =
  FcData
    ( FcDataDecl
        (testOrigin dataName)
        dataName
        tyVars
        KType
        [FcDataConDecl (testOrigin constructorName) constructorName fields | (constructorName, fields) <- constructors]
    )
  where
    testOrigin = FcTopLevelOrigin "test" "Test"
