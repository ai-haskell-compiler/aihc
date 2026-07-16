{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinGoldenTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), TcType (..), TyCon (..), Unique (..))
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
      testCase "separate FC units do not capture dependency globals" $ do
        case lowerPrograms separatePrograms of
          [_provider, consumer] -> do
            let parameters = concatMap grinFunctionParameters (grinFunctions consumer)
            assertBool "dependency CAF remains global" (all ((/= "source") . grinVarName) parameters)
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
    [ FcPrimitive raiseVar 1,
      FcPrimitive catchVar 3,
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

separatePrograms :: [FcProgram]
separatePrograms =
  [ FcProgram [FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider")))],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 30) boxedIntTy
    answerVar = Var "answer" (Unique 31) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 32) boxedIntTy
