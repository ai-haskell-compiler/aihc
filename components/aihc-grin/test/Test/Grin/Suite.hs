{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinGoldenTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (Levity (..), PrimOp (..), RuntimeRep (..), TcType (..), TyCon (..), Unique (..))
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (isInfixOf)
import Data.Text (Text)
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
        assertEqual "result" (Right "2") result,
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
      testCase "cooperative scheduler forks and transfers through an MVar" $ do
        assertEqual "lint" [] (lintProgram schedulerProgram)
        result <- interpretProgramBinding "answer" schedulerProgram
        assertEqual "result" (Right "42") result,
      testCase "timer suspension resumes its CPS continuation" $ do
        assertEqual "lint" [] (lintProgram timerProgram)
        result <- interpretProgramBinding "answer" timerProgram
        assertEqual "result" (Right "7") result,
      testCase "CPS exposes the continuation saved by scheduler operations" $ do
        let schedulerPoints = schedulerContinuations (cpsExpr schedulerActionBody)
        case schedulerPoints of
          (SchedulerNewMVar, CpsBind {}) : _ -> pure ()
          other -> assertFailure ("expected newMVar scheduler continuation, got " <> show other)
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
  let program = lowerProgram fcProgram
  case lintProgram program of
    [] -> do
      result <- interpretProgramBinding name program
      pure $
        case result of
          Left err -> Left (show err)
          Right value -> Right value
    lintErrors -> pure (Left ("GRIN lint error: " <> show lintErrors))

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
    [ FcTopBind
        ( FcNonRec
            answerVar
            (FcApp (FcLam localAnswerVar (FcVar localAnswerVar)) (FcLit (LitInt IntRep 42)))
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) intTy
    localAnswerVar = Var "answer" (Unique 2) intTy

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
    [ FcPrimitive raiseVar PrimRaise,
      FcPrimitive catchVar PrimCatch,
      FcTopBind (FcNonRec answerVar caughtExpression)
    ]
  where
    raiseVar = Var "raise#" (Unique 1) (TcFunTy intTy intTy)
    catchVar = Var "catch#" (Unique 2) (TcFunTy actionTy (TcFunTy handlerTy (TcFunTy intTy intTy)))
    answerVar = Var "answer" (Unique 3) intTy
    actionState = Var "actionState" (Unique 4) intTy
    exception = Var "exception" (Unique 5) intTy
    handlerState = Var "handlerState" (Unique 6) intTy
    actionTy = TcFunTy intTy intTy
    handlerTy = TcFunTy intTy (TcFunTy intTy intTy)
    action = FcLam actionState (FcApp (FcVar raiseVar) (FcLit (LitInt IntRep 10)))
    handler = FcLam exception (FcLam handlerState (FcVar exception))
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
      grinCafs =
        [ ( answer,
            GrinNode (GrinThunk functionName) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = functionName,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody =
                GrinBind pointer (GrinStore (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt IntRep 1)])) $
                  GrinBind fetched (GrinFetch (BoxedRep Lifted) (GrinVarValue pointer)) $
                    GrinBind updated (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt IntRep 2))) $
                      GrinEval IntRep (GrinVarValue pointer)
            }
        ]
    }
  where
    answer = GrinVar "answer" 1 (BoxedRep Lifted)
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    fetched = GrinVar "fetched" 3 (BoxedRep Lifted)
    updated = GrinVar "updated" 4 IntRep
    functionName = FunctionName "answer_code"

intTy :: TcType
intTy = TcTyCon (TyCon "Int#" 0) []

boxedIntTy :: TcType
boxedIntTy = TcTyCon (TyCon "Int" 0) []

schedulerProgram :: GrinProgram
schedulerProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinCafs = [(answer, GrinNode (GrinThunk mainFunction) [])],
      grinFunctions =
        [ GrinFunction mainFunction [] liftedRep mainBody,
          GrinFunction actionFunction [initialState] ioResultRep schedulerActionBody,
          GrinFunction childFunction [childMVar, childState] liftedRep childBody
        ]
    }
  where
    answer = GrinVar "answer" 100 liftedRep
    mainFunction = FunctionName "scheduler_main"
    actionFunction = FunctionName "scheduler_action"
    childFunction = FunctionName "scheduler_child"
    initialState = GrinVar "initialState" 101 stateRep
    childMVar = GrinVar "childMVar" 102 unliftedPointerRep
    childState = GrinVar "childState" 103 stateRep
    mainBody =
      GrinReturn
        ( GrinNodeValue
            (GrinNode (GrinConstructor "IO") [GrinNodeValue (GrinNode (GrinClosure actionFunction) [])])
        )
    childBody =
      GrinBind childNextState (GrinScheduler stateRep SchedulerPutMVar [GrinVarValue childMVar, intValue 42, GrinVarValue childState]) $
        GrinReturn (stateValueResult (GrinVarValue childNextState) unitValue)
    childNextState = GrinVar "childNextState" 104 stateRep

schedulerActionBody :: GrinExpr
schedulerActionBody =
  GrinBind allocatedTuple (GrinScheduler mvarResultRep SchedulerNewMVar [GrinVarValue initialState]) $
    GrinCase
      (GrinVarValue allocatedTuple)
      allocatedCaseBinder
      [ GrinAlt (GrinDataAlt "(#,#)") [allocatedState, mvar] $
          GrinBind forkedTuple (GrinScheduler forkResultRep SchedulerFork [childClosure mvar, GrinVarValue allocatedState]) $
            GrinCase
              (GrinVarValue forkedTuple)
              forkedCaseBinder
              [ GrinAlt (GrinDataAlt "(#,#)") [forkedState, threadId] $
                  GrinScheduler ioResultRep SchedulerTakeMVar [GrinVarValue mvar, GrinVarValue forkedState]
              ]
      ]
  where
    initialState = GrinVar "initialState" 101 stateRep
    allocatedTuple = GrinVar "allocatedTuple" 105 mvarResultRep
    allocatedCaseBinder = GrinVar "allocatedCase" 106 mvarResultRep
    allocatedState = GrinVar "allocatedState" 107 stateRep
    mvar = GrinVar "mvar" 108 unliftedPointerRep
    forkedTuple = GrinVar "forkedTuple" 109 forkResultRep
    forkedCaseBinder = GrinVar "forkedCase" 110 forkResultRep
    forkedState = GrinVar "forkedState" 111 stateRep
    threadId = GrinVar "threadId" 112 unliftedPointerRep
    childClosure capturedMVar =
      GrinNodeValue
        (GrinNode (GrinClosure (FunctionName "scheduler_child")) [GrinVarValue capturedMVar])

timerProgram :: GrinProgram
timerProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinCafs = [(answer, GrinNode (GrinThunk mainFunction) [])],
      grinFunctions =
        [ GrinFunction mainFunction [] liftedRep mainBody,
          GrinFunction actionFunction [initialState] liftedRep actionBody
        ]
    }
  where
    answer = GrinVar "answer" 120 liftedRep
    mainFunction = FunctionName "timer_main"
    actionFunction = FunctionName "timer_action"
    initialState = GrinVar "initialState" 121 stateRep
    delayedState = GrinVar "delayedState" 122 stateRep
    mainBody =
      GrinReturn
        ( GrinNodeValue
            (GrinNode (GrinConstructor "IO") [GrinNodeValue (GrinNode (GrinClosure actionFunction) [])])
        )
    actionBody =
      GrinBind delayedState (GrinScheduler stateRep SchedulerDelay [intValue 1, GrinVarValue initialState]) $
        GrinReturn (stateValueResult (GrinVarValue delayedState) (intValue 7))

stateValueResult :: GrinValue -> GrinValue -> GrinValue
stateValueResult state value =
  GrinNodeValue (GrinNode (GrinConstructor "(#,#)") [state, value])

intValue :: Integer -> GrinValue
intValue = GrinLitValue . GrinLitInt IntRep

unitValue :: GrinValue
unitValue = GrinNodeValue (GrinNode (GrinConstructor "()") [])

liftedRep :: RuntimeRep
liftedRep = BoxedRep Lifted

unliftedPointerRep :: RuntimeRep
unliftedPointerRep = BoxedRep Unlifted

stateRep :: RuntimeRep
stateRep = TupleRep []

mvarResultRep :: RuntimeRep
mvarResultRep = TupleRep [stateRep, unliftedPointerRep]

forkResultRep :: RuntimeRep
forkResultRep = TupleRep [stateRep, unliftedPointerRep]

ioResultRep :: RuntimeRep
ioResultRep = TupleRep [stateRep, IntRep]
