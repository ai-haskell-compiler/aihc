{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinGoldenTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Newtype (extractNewtypeInterface, lowerNewtypes, lowerNewtypesWithInterface)
import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), runtimeRepOfType)
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GrinGolden qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

grinUnitTests :: TestTree
grinUnitTests =
  testGroup
    "GRIN"
    [ testCase "FC lowering passes known WHNF arguments without thunk cells" $ do
        let program = lowerProgram applicationProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "does not allocate a constructor argument thunk" (not ("store (F$grin_thunk" `isInfixOf` rendered))
        assertBool "contains explicit application" ("apply " `isInfixOf` rendered),
      testCase "interpreter implements fetch and update" $ do
        result <- interpretProgramBinding "answer" heapProgram
        assertEqual "result" (Right "Box 2") result,
      testCase "interpreter rejects unlifted thunk results" $ do
        result <- interpretProgramBinding "bad" (unliftedHeapProgram IntRep)
        assertEqual "result" (Left (InterpretInvalidThunkResultRep unliftedThunkFunction IntRep)) result,
      testCase "interpreter rejects unlifted heap updates" $ do
        result <- interpretProgramBinding "answer" invalidUpdateProgram
        assertEqual "result" (Left (InterpretInvalidUpdateValue (RuntimeLit (GrinLitInt IntRep 2)))) result,
      testCase "lint rejects unlifted thunk results and heap updates" $
        forM_ unliftedRuntimeReps $ \runtimeRep ->
          let errors = lintProgram (unliftedHeapProgram runtimeRep)
           in do
                assertBool
                  ("accepted thunk returning " <> show runtimeRep)
                  (GrinLintThunkResult unliftedThunkFunction runtimeRep `elem` errors)
                assertBool
                  ("accepted heap update with " <> show runtimeRep)
                  (GrinLintUpdateNonLifted runtimeRep `elem` errors),
      testCase "lint rejects constructor field representation mismatches" $ do
        let program = heapProgram {grinConstructors = [("Box", [WordRep])]}
        assertBool
          "constructor layout mismatch"
          (GrinLintConstructorLayout "Box" [WordRep] [IntRep] `elem` lintProgram program),
      testCase "FC lowering makes exception control explicit" $ do
        let program = lowerProgram exceptionProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains explicit throw" ("throw " `isInfixOf` rendered)
        assertBool "contains explicit catch" ("catch " `isInfixOf` rendered),
      testCase "CPS-GRIN allocates and applies ordinary continuation closures" $ do
        cps <- expectCpsGrin heapProgram
        let program = cpsGrinProgram cps
            rendered = renderProgram program
        assertEqual "transformed lint" [] (lintProgram program)
        assertBool "allocates a continuation closure" ("store (P$cps$" `isInfixOf` rendered)
        assertBool "invokes a continuation closure" ("apply @BoxedRep Lifted $cps_continuation" `isInfixOf` rendered)
        assertBool "generated continuation captures its environment" (any continuationHasCaptures (grinFunctions program)),
      testCase "CPS-GRIN preserves evaluation semantics" $ do
        cps <- expectCpsGrin heapProgram
        directResult <- interpretProgramBinding "answer" heapProgram
        cpsResult <- interpretProgramBinding "answer" (cpsGrinProgram cps)
        assertEqual "result" directResult cpsResult,
      testCase "CPS-GRIN requires exception control to be eliminated" $ do
        assertEqual
          "throw"
          (Left (CpsGrinUnexpectedThrow exceptionBoundaryFunction))
          (toCpsGrin (exceptionBoundaryProgram (GrinThrow (GrinLitValue (GrinLitInt IntRep 1)))))
        assertEqual
          "catch"
          (Left (CpsGrinUnexpectedCatch exceptionBoundaryFunction))
          (toCpsGrin (exceptionBoundaryProgram (GrinCatch IntRep exceptionAction exceptionHandler []))),
      testCase "FC lowering evaluates Int# arguments without allocating thunks" $ do
        let program = lowerProgram unboxedApplicationProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "records IntRep" ("IntRep" `isInfixOf` rendered)
        assertBool "does not allocate an argument thunk" (not ("store (F$grin_thunk" `isInfixOf` rendered)),
      testCase "FC lowering calls saturated primitives without global slots" $ do
        let program = lowerProgram primitiveCallProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains a direct primitive call" ("primitive-call @IntRep +#" `isInfixOf` rendered)
        assertBool "primitive is not global" (not ("global +#" `isInfixOf` rendered)),
      testCase "FC lowering stores saturated constructor nodes" $ do
        let program = lowerProgram saturatedConstructorProgram
        assertEqual "lint" [] (lintProgram program)
        case grinFunctions program of
          [function] ->
            assertEqual
              "constructor body"
              (GrinStore (GrinNode (GrinConstructor "I32#") [GrinVarValue (GrinVar "value" 62 Int32Rep)]))
              (grinFunctionBody function)
          functions -> assertFailure ("expected one constructor function, got " <> show (length functions)),
      testCase "FC lowering emits saturated strict foreign calls" $ do
        let program = lowerProgram foreignProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains a direct foreign call" ("foreign-call $ffi$abs" `isInfixOf` rendered)
        assertBool "does not apply a foreign function value" (not ("apply @Int32Rep $ffi$abs" `isInfixOf` rendered)),
      testCase "lint rejects undersaturated foreign calls" $ do
        let program = lowerProgram undersaturatedForeignProgram
        assertBool
          "foreign arity mismatch"
          (GrinLintForeignArity "$ffi$abs" 1 0 `elem` lintProgram program),
      testCase "FC lowering distinguishes shadowing locals from globals" $ do
        let program = lowerProgram shadowingProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "raw local is emitted as a constant" ("constant answer%2 :: IntRep" `isInfixOf` rendered)
        assertBool "raw local is not treated as a global cell" (not ("eval @IntRep answer%2" `isInfixOf` rendered)),
      testCase "FC lowering still evaluates CAF references" $ do
        let program = lowerProgram cafReferenceProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "CAF reference is evaluated" ("eval @BoxedRep Lifted source%" `isInfixOf` rendered),
      testCase "FC lowering initializes static constructor values without CAF cells" $ do
        let program = lowerProgram staticConstructorProgram
        assertEqual "lint" [] (lintProgram program)
        assertEqual "static globals" ["source"] (map (grinVarName . fst) (grinWhnfGlobals program))
        assertEqual "no CAFs" [] (grinCafs program),
      testCase "FC lowering only makes computed function values CAFs" $ do
        let program = lowerProgram functionClassificationProgram
        assertEqual "lint" [] (lintProgram program)
        assertEqual "direct functions are not globals" [] (map (grinVarName . fst) (grinWhnfGlobals program))
        assertEqual "computed function CAFs" ["computed"] (map (grinVarName . fst) (grinCafs program)),
      testCase "FC lowering flattens top-level lambdas into one exported entry" $ do
        let program = lowerProgram functionClassificationProgram
            exported = [function | function <- grinFunctions program, grinFunctionLinkName function == Just "direct"]
        case exported of
          [function] -> assertEqual "runtime parameters" 2 (length (grinFunctionParameters function))
          _ -> assertFailure ("expected one exported direct entry, got " <> show (length exported)),
      testCase "recursive function bindings store closures without thunk wrappers" $ do
        let program = lowerProgram recursiveFunctionProgram
            recursiveNodes =
              [ node
              | function <- grinFunctions program,
                GrinStoreRec bindings _ <- [grinFunctionBody function],
                (_, node) <- bindings
              ]
        assertEqual "lint" [] (lintProgram program)
        case recursiveNodes of
          [GrinNode GrinClosure {} _] -> pure ()
          nodes -> assertFailure ("expected one recursive closure node, got " <> show nodes)
        result <- interpretProgramBinding "answer" program
        assertEqual "result" (Right "<function>") result,
      testCase "FC dictionaries lower to ordinary constructor nodes and cases" $ do
        let program = lowerProgram dictionaryProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertEqual
          "dictionary constructor has an ordinary declared layout"
          [("Box", [IntRep]), ("$Dict$Test", [BoxedRep Lifted, BoxedRep Lifted])]
          (grinConstructors program)
        assertBool ("explicit dictionary constructor store:\n" <> rendered) ("store (C$Dict$Test" `isInfixOf` rendered)
        assertBool "ordinary dictionary case" ("$Dict$Test" `isInfixOf` rendered && "case " `isInfixOf` rendered)
        assertBool "no tuple encoding" (not ("C(,)" `isInfixOf` rendered || "C()" `isInfixOf` rendered))
        assertBool "no projection operation" (not ("project " `isInfixOf` rendered))
        result <- interpretProgramBinding "answer" program
        assertEqual "selected field is evaluated" (Right "Box 2") result,
      testCase "separate FC units do not capture dependency globals" $ do
        case separatePrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
                parameters = concatMap grinFunctionParameters (grinFunctions consumer)
            assertBool "dependency CAF remains global" (all ((/= "source") . grinVarName) parameters)
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units call dependency code entries directly" $ do
        case separateFunctionPrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
                rendered = renderProgram consumer
            assertEqual "lint" [] (lintProgram consumer)
            assertEqual "external code" ["identity"] (map grinCodeSourceName (grinExternalFunctions consumer))
            assertBool "direct dependency call" ("call @BoxedRep Lifted $entry$identity" `isInfixOf` rendered)
            assertBool "dependency function has no global slot" (not ("global identity" `isInfixOf` rendered))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units store saturated dependency constructors directly" $ do
        case separateConstructorPrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
            case grinFunctions consumer of
              [function] ->
                assertEqual
                  "constructor body"
                  (GrinStore (GrinNode (GrinConstructor "I32#") [GrinVarValue (GrinVar "value" 72 Int32Rep)]))
                  (grinFunctionBody function)
              functions -> assertFailure ("expected one constructor function, got " <> show (length functions))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units erase dependency newtypes" $ do
        case separateNewtypePrograms of
          [providerCore, sourceConsumer] -> do
            let consumerCore = lowerNewtypesWithInterface (extractNewtypeInterface providerCore) sourceConsumer
                provider = lowerProgram providerCore
                consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
            assertEqual "newtype declaration emits no constructor" [] (grinConstructors provider)
            assertEqual "consumer lint" [] (lintProgram consumer)
            assertBool "newtype constructor is erased across units" (not ("Wrap" `isInfixOf` renderProgram consumer))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs))
    ]

expectCpsGrin :: GrinProgram -> IO CpsGrinProgram
expectCpsGrin program =
  case toCpsGrin program of
    Left err -> assertFailure ("expected CPS-GRIN conversion to succeed, got " <> show err)
    Right cps -> pure cps

continuationHasCaptures :: GrinFunction -> Bool
continuationHasCaptures function =
  "$cps$" `T.isInfixOf` unFunctionName (grinFunctionName function)
    && length (grinFunctionParameters function) > 1

grinGoldenTests :: IO TestTree
grinGoldenTests =
  testGroup "GRIN golden tests" . map goldenTest <$> GrinGolden.loadGrinCases
  where
    goldenTest fixture = testCase (GrinGolden.caseId fixture) $ do
      let (outcome, details) = GrinGolden.evaluateGrinCase fixture
      case outcome of
        GrinGolden.OutcomePass -> pure ()
        GrinGolden.OutcomeXFail -> pure ()
        GrinGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
        GrinGolden.OutcomeFail -> assertFailure details

grinEvalFixtureTests :: IO TestTree
grinEvalFixtureTests = do
  cases <- EvalGolden.loadEvalCases
  let tests = map mkEvalFixtureTest cases
  pure (testGroup "shared evaluation fixtures via GRIN" tests)

mkEvalFixtureTest :: EvalGolden.EvalCase -> TestTree
mkEvalFixtureTest evalCase = testCase (EvalGolden.evalCaseId evalCase) $ do
  (outcome, details) <- EvalGolden.evaluateEvalCase evaluateGrinProgram evalCase
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

evaluateGrinProgram :: Text -> FcProgram -> IO (Either String Text)
evaluateGrinProgram name fcProgram = do
  case prepareEvalProgram name (lowerNewtypes fcProgram) of
    Left err -> pure (Left err)
    Right (preparedProgram, entry, unwrapResult) -> do
      let program = lowerProgram preparedProgram
      case lintProgram program of
        [] -> do
          result <-
            case entry of
              EvaluateBinding -> interpretProgramBinding name program
              RunIoAction -> interpretProgramIoBinding name program
          pure $
            case result of
              Left err -> Left (show err)
              Right value -> Right (unwrapResult value)
        lintErrors -> pure (Left ("GRIN lint error: " <> show lintErrors))

data BindingEntry
  = EvaluateBinding
  | RunIoAction

prepareEvalProgram :: Text -> FcProgram -> Either String (FcProgram, BindingEntry, Text -> Text)
prepareEvalProgram name program@(FcProgram topBinds) =
  case break isEvalBinding topBinds of
    (_, []) -> Left ("missing evaluation binding " <> T.unpack name)
    (_, FcTopBind (FcRec _) : _) -> Left "recursive evaluation binding is unsupported"
    (before, FcTopBind (FcNonRec var rhs) : after) -> do
      runtimeRep <- runtimeRepOfType (varType var)
      if runtimeRep == BoxedRep Lifted
        then Right (program, bindingEntry (varType var), id)
        else do
          let componentCount = length (runtimeRepComponents runtimeRep)
              constructorName = evalResultConstructor componentCount
              wrapperType = TcTyCon (TyCon "__AihcEvalResultType" 0) []
              constructorVar = Var constructorName (Unique (-1000000)) (TcFunTy (varType var) wrapperType)
              wrappedVar = var {varType = wrapperType}
              declaration = FcData "__AihcEvalResultType" [] [(constructorName, [varType var])]
              wrappedBinding = FcTopBind (FcNonRec wrappedVar (FcApp (FcVar constructorVar) rhs))
          Right
            ( FcProgram (declaration : before <> (wrappedBinding : after)),
              EvaluateBinding,
              unwrapEvalResult componentCount constructorName
            )
    (_, _ : _) -> Left "invalid evaluation binding"
  where
    isEvalBinding topBind =
      case topBind of
        FcTopBind (FcNonRec var _) -> varName var == name
        FcTopBind (FcRec bindings) -> any ((== name) . varName . fst) bindings
        _ -> False

bindingEntry :: TcType -> BindingEntry
bindingEntry ty
  | isIOType ty = RunIoAction
  | otherwise = EvaluateBinding

isIOType :: TcType -> Bool
isIOType ty =
  case ty of
    TcTyCon (TyCon name arity) [_] -> name == "IO" && arity == 1
    TcForAllTy _ body -> isIOType body
    TcQualTy _ body -> isIOType body
    _ -> False

evalResultConstructor :: Int -> Text
evalResultConstructor componentCount
  | componentCount == 0 = "()"
  | componentCount == 1 = "__AihcEvalResult"
  | otherwise = "(" <> T.replicate (componentCount - 1) "," <> ")"

unwrapEvalResult :: Int -> Text -> Text -> Text
unwrapEvalResult componentCount constructorName rendered
  | componentCount == 0 = "<state>"
  | componentCount == 1 = fromMaybe rendered (T.stripPrefix (constructorName <> " ") rendered)
  | otherwise = rendered

applicationProgram :: FcProgram
applicationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcLam argumentVar (FcVar argumentVar))
                (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) boxedIntTy
    argumentVar = Var "argument" (Unique 2) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 3) (TcFunTy intTy boxedIntTy)

unboxedApplicationProgram :: FcProgram
unboxedApplicationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcVar boxConstructorVar)
                (FcApp (FcLam argumentVar (FcVar argumentVar)) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 10) boxedIntTy
    argumentVar = Var "argument" (Unique 11) intTy
    boxConstructorVar = Var "BoxedInt" (Unique 12) (TcFunTy intTy boxedIntTy)

shadowingProgram :: FcProgram
shadowingProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcVar boxConstructorVar)
                (FcApp (FcLam localAnswerVar (FcVar localAnswerVar)) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) boxedIntTy
    localAnswerVar = Var "answer" (Unique 2) intTy
    boxConstructorVar = Var "BoxedInt" (Unique 3) (TcFunTy intTy boxedIntTy)

cafReferenceProgram :: FcProgram
cafReferenceProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            sourceVar
            ( FcApp
                (FcLam computedVar (FcVar computedVar))
                (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 1)))
            )
        ),
      FcTopBind (FcNonRec answerVar (FcVar sourceVar))
    ]
  where
    sourceVar = Var "source" (Unique 20) boxedIntTy
    answerVar = Var "answer" (Unique 21) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 22) (TcFunTy intTy boxedIntTy)
    computedVar = Var "computed" (Unique 23) boxedIntTy

staticConstructorProgram :: FcProgram
staticConstructorProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            sourceVar
            (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 1)))
        )
    ]
  where
    sourceVar = Var "source" (Unique 24) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 25) (TcFunTy intTy boxedIntTy)

recursiveFunctionProgram :: FcProgram
recursiveFunctionProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            answerVar
            ( FcLet
                (FcRec [(functionVar, FcLam argumentVar (FcVar argumentVar))])
                (FcVar functionVar)
            )
        )
    ]
  where
    functionTy = TcFunTy boxedIntTy boxedIntTy
    answerVar = Var "answer" (Unique 26) functionTy
    functionVar = Var "function" (Unique 27) functionTy
    argumentVar = Var "argument" (Unique 28) boxedIntTy

primitiveCallProgram :: FcProgram
primitiveCallProgram =
  FcProgram
    [ FcPrimitive addVar 2,
      FcTopBind
        ( FcNonRec
            addOneVar
            (FcLam argumentVar (FcApp (FcApp (FcVar addVar) (FcVar argumentVar)) (FcLit (LitInt IntRep 1))))
        )
    ]
  where
    addVar = Var "+#" (Unique 29) (TcFunTy intTy (TcFunTy intTy intTy))
    addOneVar = Var "addOne" (Unique 30) (TcFunTy intTy intTy)
    argumentVar = Var "argument" (Unique 31) intTy

functionClassificationProgram :: FcProgram
functionClassificationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            directVar
            (FcTyLam typeVar (FcLam dictionaryVar (FcLam directArgumentVar (FcVar directArgumentVar))))
        ),
      FcTopBind
        ( FcNonRec
            computedVar
            ( FcLet
                (FcNonRec expensiveVar (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 42))))
                (FcLam computedArgumentVar (FcVar expensiveVar))
            )
        )
    ]
  where
    typeVar = TyVarId "a" (Unique 30)
    typeVarTy = TcTyVar typeVar
    dictionaryVar = Var "$dictionary" (Unique 31) boxedIntTy
    directArgumentVar = Var "argument" (Unique 32) typeVarTy
    directVar = Var "direct" (Unique 33) (TcForAllTy typeVar (TcFunTy boxedIntTy (TcFunTy typeVarTy typeVarTy)))
    expensiveVar = Var "expensive" (Unique 34) boxedIntTy
    computedArgumentVar = Var "argument" (Unique 35) boxedIntTy
    computedVar = Var "computed" (Unique 36) (TcFunTy boxedIntTy boxedIntTy)
    boxConstructorVar = Var "BoxedInt" (Unique 37) (TcFunTy intTy boxedIntTy)

dictionaryProgram :: FcProgram
dictionaryProgram =
  FcProgram
    [ FcData "Box" [] [("Box", [intTy])],
      FcData "Test" [] [("$Dict$Test", [boxedIntTy, boxedIntTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcCase
                dictionary
                dictionaryBinder
                [FcAlt (DataAlt "$Dict$Test") [firstMethod, secondMethod] (FcVar secondMethod)]
            )
        )
    ]
  where
    dictionaryTy = TcTyCon (TyCon "Test" 0) []
    answerVar = Var "answer" (Unique 40) boxedIntTy
    boxConstructorVar = Var "Box" (Unique 41) (TcFunTy intTy boxedIntTy)
    dictionaryConstructorVar = Var "$Dict$Test" (Unique 42) (TcFunTy boxedIntTy (TcFunTy boxedIntTy dictionaryTy))
    dictionaryBinder = Var "$dictionary" (Unique 43) dictionaryTy
    firstMethod = Var "$method0" (Unique 44) boxedIntTy
    secondMethod = Var "$method1" (Unique 45) boxedIntTy
    dictionary = FcApp (FcApp (FcVar dictionaryConstructorVar) (boxed 1)) (boxed 2)
    boxed value = FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep value))

exceptionProgram :: FcProgram
exceptionProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcPrimitive raiseVar 1,
      FcPrimitive catchVar 3,
      FcTopBind (FcNonRec answerVar (FcApp (FcVar boxConstructorVar) caughtExpression))
    ]
  where
    raiseVar = Var "raise#" (Unique 1) (TcFunTy intTy intTy)
    catchVar = Var "catch#" (Unique 2) (TcFunTy actionTy (TcFunTy handlerTy (TcFunTy intTy intTy)))
    answerVar = Var "answer" (Unique 3) boxedIntTy
    actionState = Var "actionState" (Unique 4) intTy
    exception = Var "exception" (Unique 5) intTy
    handlerState = Var "handlerState" (Unique 6) intTy
    actionTy = TcFunTy intTy intTy
    handlerTy = TcFunTy intTy (TcFunTy intTy intTy)
    action = FcLam actionState (FcApp (FcVar raiseVar) (FcLit (LitInt IntRep 10)))
    handler = FcLam exception (FcLam handlerState (FcVar exception))
    boxConstructorVar = Var "BoxedInt" (Unique 7) (TcFunTy intTy boxedIntTy)
    caughtExpression =
      FcApp
        (FcApp (FcApp (FcVar catchVar) action) handler)
        (FcLit (LitInt IntRep 0))

heapProgram :: GrinProgram
heapProgram =
  GrinProgram
    { grinConstructors = [("Box", [IntRep])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs =
        [ ( answer,
            GrinNode (GrinThunk functionName) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = functionName,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 1)])) $
                  GrinBind [fetched] (GrinFetch (BoxedRep Lifted) (GrinVarValue pointer)) $
                    GrinBind [replacementPointer] (GrinStore replacementBox) $
                      GrinBind [replacement] (GrinFetch (BoxedRep Lifted) (GrinVarValue replacementPointer)) $
                        GrinBind [updated] (GrinUpdate (GrinVarValue pointer) (GrinVarValue replacement)) $
                          GrinEval (BoxedRep Lifted) (GrinVarValue pointer)
            }
        ]
    }
  where
    answer = GrinVar "answer" 1 (BoxedRep Lifted)
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    fetched = GrinVar "fetched" 3 (BoxedRep Lifted)
    replacementPointer = GrinVar "replacement_pointer" 4 (BoxedRep Lifted)
    replacement = GrinVar "replacement" 5 (BoxedRep Lifted)
    updated = GrinVar "updated" 6 (BoxedRep Lifted)
    replacementBox = GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 2)]
    functionName = FunctionName "answer_code"

exceptionBoundaryProgram :: GrinExpr -> GrinProgram
exceptionBoundaryProgram body =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = exceptionBoundaryFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = body
            }
        ]
    }

exceptionBoundaryFunction :: FunctionName
exceptionBoundaryFunction = FunctionName "exception_boundary"

exceptionAction :: GrinValue
exceptionAction = GrinLitValue (GrinLitString "action")

exceptionHandler :: GrinValue
exceptionHandler = GrinLitValue (GrinLitString "handler")

invalidUpdateProgram :: GrinProgram
invalidUpdateProgram =
  heapProgram
    { grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "answer_code",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore initialBox) $
                  GrinBind [updated] (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt IntRep 2))) $
                    GrinConstant [GrinLitValue (GrinLitInt IntRep 0)]
            }
        ]
    }
  where
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    updated = GrinVar "updated" 3 IntRep
    initialBox = GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 1)]

unliftedRuntimeReps :: [RuntimeRep]
unliftedRuntimeReps =
  [ IntRep,
    WordRep,
    BoxedRep Unlifted,
    TupleRep [IntRep, WordRep],
    TupleRep []
  ]

unliftedHeapProgram :: RuntimeRep -> GrinProgram
unliftedHeapProgram runtimeRep =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [(GrinVar "bad" 1 (BoxedRep Lifted), GrinNode (GrinThunk unliftedThunkFunction) [GrinLitValue (GrinLitString "capture")])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = unliftedThunkFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [pointer],
              grinFunctionResultRep = runtimeRep,
              grinFunctionBody =
                GrinBind
                  [updated]
                  (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt runtimeRep 0)))
                  ( GrinConstant
                      [ GrinLitValue (GrinLitInt componentRep 0)
                      | componentRep <- runtimeRepComponents runtimeRep
                      ]
                  )
            }
        ]
    }
  where
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    updated = GrinVar "updated" 3 runtimeRep

unliftedThunkFunction :: FunctionName
unliftedThunkFunction = FunctionName "unlifted_thunk"

intTy :: TcType
intTy = TcTyCon (TyCon "Int#" 0) []

boxedIntTy :: TcType
boxedIntTy = TcTyCon (TyCon "Int" 0) []

foreignProgram :: FcProgram
foreignProgram =
  FcProgram
    [ FcData "BoxedInt32" [] [("BoxedInt32", [int32Ty])],
      FcForeignImport foreignCall,
      FcTopBind
        ( FcNonRec
            foreignAnswerVar
            (FcApp (FcVar foreignBoxConstructorVar) (FcCallForeign foreignCall [FcLit (LitInt Int32Rep 42)]))
        )
    ]

undersaturatedForeignProgram :: FcProgram
undersaturatedForeignProgram =
  FcProgram
    [ FcData "BoxedInt32" [] [("BoxedInt32", [int32Ty])],
      FcForeignImport foreignCall,
      FcTopBind (FcNonRec foreignAnswerVar (FcApp (FcVar foreignBoxConstructorVar) (FcCallForeign foreignCall [])))
    ]

foreignCall :: FcForeignCall
foreignCall =
  FcForeignCall
    { fcForeignCallName = "$ffi$abs",
      fcForeignCallSymbol = "abs",
      fcForeignCallSignature =
        FcForeignSignature
          { fcForeignArgumentTypes = [FcForeignInt32],
            fcForeignResultType = FcForeignInt32,
            fcForeignEffect = FcForeignPure
          }
    }

foreignAnswerVar :: Var
foreignAnswerVar = Var "answer" (Unique 50) boxedIntTy

foreignBoxConstructorVar :: Var
foreignBoxConstructorVar = Var "BoxedInt32" (Unique 51) (TcFunTy int32Ty boxedIntTy)

int32Ty :: TcType
int32Ty = TcTyCon (TyCon "Int32#" 0) []

saturatedConstructorProgram :: FcProgram
saturatedConstructorProgram =
  FcProgram
    [ FcData "Int32" [] [("I32#", [int32Ty])],
      FcTopBind
        ( FcNonRec
            int32WrapperVar
            (FcLam int32ArgumentVar (FcApp (FcVar int32ConstructorVar) (FcVar int32ArgumentVar)))
        )
    ]
  where
    int32WrapperVar = Var "wrapInt32" (Unique 60) (TcFunTy int32Ty boxedInt32Ty)
    int32ConstructorVar = Var "I32#" (Unique 61) (TcFunTy int32Ty boxedInt32Ty)

boxedInt32Ty :: TcType
boxedInt32Ty = TcTyCon (TyCon "Int32" 0) []

int32ArgumentVar :: Var
int32ArgumentVar = Var "value" (Unique 62) int32Ty

separateConstructorPrograms :: [FcProgram]
separateConstructorPrograms =
  [ FcProgram [FcData "Int32" [] [("I32#", [int32Ty])]],
    FcProgram
      [ FcTopBind
          ( FcNonRec
              wrapperVar
              (FcLam argumentVar (FcApp (FcVar constructorVar) (FcVar argumentVar)))
          )
      ]
  ]
  where
    wrapperVar = Var "wrapInt32" (Unique 70) (TcFunTy int32Ty boxedInt32Ty)
    constructorVar = Var "I32#" (Unique 71) (TcFunTy int32Ty boxedInt32Ty)
    argumentVar = Var "value" (Unique 72) int32Ty

separatePrograms :: [FcProgram]
separatePrograms =
  [ FcProgram [FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider")))],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 30) boxedIntTy
    answerVar = Var "answer" (Unique 31) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 32) boxedIntTy

separateFunctionPrograms :: [FcProgram]
separateFunctionPrograms =
  [ FcProgram
      [ FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider"))),
        FcTopBind (FcNonRec identityVar (FcLam argumentVar (FcVar argumentVar)))
      ],
    FcProgram [FcTopBind (FcNonRec answerVar (FcApp (FcVar identityVar) (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 80) boxedIntTy
    identityVar = Var "identity" (Unique 81) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 82) boxedIntTy
    answerVar = Var "answer" (Unique 83) boxedIntTy

separateNewtypePrograms :: [FcProgram]
separateNewtypePrograms =
  [ FcProgram [FcNewtype declaration],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcApp (FcVar constructorVar) (FcLit (LitInt IntRep 42)))))]
  ]
  where
    declaration =
      FcNewtypeDecl
        { fcNewtypeName = "Wrapper",
          fcNewtypeTyVars = [],
          fcNewtypeConstructor = "Wrap",
          fcNewtypeRepresentation = intTy,
          fcNewtypeResult = wrapperTy
        }
    wrapperTy = TcTyCon (TyCon "Wrapper" 0) []
    constructorVar = Var "Wrap" (Unique 40) (TcFunTy intTy wrapperTy)
    answerVar = Var "answer" (Unique 41) (TcFunTy boxedIntTy wrapperTy)
    argumentVar = Var "argument" (Unique 42) boxedIntTy
