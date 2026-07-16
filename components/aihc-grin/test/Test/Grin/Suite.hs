{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinGoldenTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), TcType (..), TyCon (..), Unique (..), runtimeRepOfType)
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
    [ testCase "FC lowering makes laziness and application explicit" $ do
        let program = lowerProgram applicationProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains an allocated thunk" ("store " `isInfixOf` rendered)
        assertBool "contains explicit evaluation" ("eval " `isInfixOf` rendered)
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
      testCase "FC lowering evaluates Int# arguments without allocating thunks" $ do
        let program = lowerProgram unboxedApplicationProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "records IntRep" ("IntRep" `isInfixOf` rendered)
        assertBool "does not allocate an argument thunk" (not ("store " `isInfixOf` rendered)),
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
        assertBool "raw local is returned directly" ("return answer%2 :: IntRep" `isInfixOf` rendered)
        assertBool "raw local is not treated as a global cell" (not ("eval @IntRep answer%2" `isInfixOf` rendered)),
      testCase "FC lowering still evaluates CAF references" $ do
        let program = lowerProgram cafReferenceProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "CAF reference is evaluated" ("eval @BoxedRep Lifted source%" `isInfixOf` rendered),
      testCase "separate FC units do not capture dependency globals" $ do
        case lowerPrograms separatePrograms of
          [_provider, consumer] -> do
            let parameters = concatMap grinFunctionParameters (grinFunctions consumer)
            assertBool "dependency CAF remains global" (all ((/= "source") . grinVarName) parameters)
          programs -> assertFailure ("expected two separately lowered programs, got " <> show (length programs)),
      testCase "separate FC units erase dependency newtypes" $ do
        case lowerPrograms separateNewtypePrograms of
          [provider, consumer] -> do
            assertEqual "newtype declaration emits no constructor" [] (grinConstructors provider)
            assertEqual "consumer lint" [] (lintProgram consumer)
            assertBool "newtype constructor is erased across units" (not ("Wrap" `isInfixOf` renderProgram consumer))
          programs -> assertFailure ("expected two separately lowered programs, got " <> show (length programs))
    ]

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
    Right (preparedProgram, unwrapResult) -> do
      let program = lowerProgram preparedProgram
      case lintProgram program of
        [] -> do
          result <- interpretProgramBinding name program
          pure $
            case result of
              Left err -> Left (show err)
              Right value -> Right (unwrapResult value)
        lintErrors -> pure (Left ("GRIN lint error: " <> show lintErrors))

prepareEvalProgram :: Text -> FcProgram -> Either String (FcProgram, Text -> Text)
prepareEvalProgram name program@(FcProgram topBinds) =
  case break isEvalBinding topBinds of
    (_, []) -> Left ("missing evaluation binding " <> T.unpack name)
    (_, FcTopBind (FcRec _) : _) -> Left "recursive evaluation binding is unsupported"
    (before, FcTopBind (FcNonRec var rhs) : after) -> do
      runtimeRep <- runtimeRepOfType (varType var)
      if runtimeRep == BoxedRep Lifted
        then Right (program, id)
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
              unwrapEvalResult componentCount constructorName
            )
    (_, _ : _) -> Left "invalid evaluation binding"
  where
    isEvalBinding topBind =
      case topBind of
        FcTopBind (FcNonRec var _) -> varName var == name
        FcTopBind (FcRec bindings) -> any ((== name) . varName . fst) bindings
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
            (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 1)))
        ),
      FcTopBind (FcNonRec answerVar (FcVar sourceVar))
    ]
  where
    sourceVar = Var "source" (Unique 20) boxedIntTy
    answerVar = Var "answer" (Unique 21) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 22) (TcFunTy intTy boxedIntTy)

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
      grinIoCafs = mempty,
      grinCafs =
        [ ( answer,
            GrinNode (GrinThunk functionName) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = functionName,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 1)])) $
                  GrinBind [fetched] (GrinFetch (BoxedRep Lifted) (GrinVarValue pointer)) $
                    GrinBind [updated] (GrinUpdate (GrinVarValue pointer) updatedBox) $
                      GrinEval (BoxedRep Lifted) (GrinVarValue pointer)
            }
        ]
    }
  where
    answer = GrinVar "answer" 1 (BoxedRep Lifted)
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    fetched = GrinVar "fetched" 3 (BoxedRep Lifted)
    updated = GrinVar "updated" 4 (BoxedRep Lifted)
    updatedBox = GrinNodeValue (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 2)])
    functionName = FunctionName "answer_code"

invalidUpdateProgram :: GrinProgram
invalidUpdateProgram =
  heapProgram
    { grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "answer_code",
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore initialBox) $
                  GrinBind [updated] (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt IntRep 2))) $
                    GrinReturn [updatedBox]
            }
        ]
    }
  where
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    updated = GrinVar "updated" 3 IntRep
    initialBox = GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 1)]
    updatedBox = GrinNodeValue (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 2)])

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
      grinIoCafs = mempty,
      grinCafs = [(GrinVar "bad" 1 (BoxedRep Lifted), GrinNode (GrinThunk unliftedThunkFunction) [GrinLitValue (GrinLitString "capture")])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = unliftedThunkFunction,
              grinFunctionParameters = [pointer],
              grinFunctionResultRep = runtimeRep,
              grinFunctionBody =
                GrinBind
                  [updated]
                  (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt runtimeRep 0)))
                  ( GrinReturn
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

separatePrograms :: [FcProgram]
separatePrograms =
  [ FcProgram [FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider")))],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 30) boxedIntTy
    answerVar = Var "answer" (Unique 31) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 32) boxedIntTy

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
