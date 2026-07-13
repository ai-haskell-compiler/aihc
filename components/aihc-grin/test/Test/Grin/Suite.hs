{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (TcType (..), TyCon (..), Unique (..))
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (isInfixOf)
import Data.Text (Text)
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
      testCase "FC lowering makes exception control explicit" $ do
        let program = lowerProgram exceptionProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains explicit throw" ("throw " `isInfixOf` rendered)
        assertBool "contains explicit catch" ("catch " `isInfixOf` rendered)
    ]

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
    [ FcTopBind
        ( FcNonRec
            answerVar
            (FcApp (FcLam argumentVar (FcVar argumentVar)) (FcLit (LitInt 42)))
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) intTy
    argumentVar = Var "argument" (Unique 2) intTy

exceptionProgram :: FcProgram
exceptionProgram =
  FcProgram
    [ FcPrimitive raiseVar 1,
      FcPrimitive catchVar 3,
      FcTopBind (FcNonRec answerVar caughtExpression)
    ]
  where
    raiseVar = Var "raise#" (Unique 1) intTy
    catchVar = Var "catch#" (Unique 2) intTy
    answerVar = Var "answer" (Unique 3) intTy
    actionState = Var "actionState" (Unique 4) intTy
    exception = Var "exception" (Unique 5) intTy
    handlerState = Var "handlerState" (Unique 6) intTy
    action = FcLam actionState (FcApp (FcVar raiseVar) (FcLit (LitInt 10)))
    handler = FcLam exception (FcLam handlerState (FcVar exception))
    caughtExpression =
      FcApp
        (FcApp (FcApp (FcVar catchVar) action) handler)
        (FcLit (LitInt 0))

heapProgram :: GrinProgram
heapProgram =
  GrinProgram
    { grinConstructors = [],
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
              grinFunctionBody =
                GrinBind pointer (GrinStore (GrinNode (GrinConstructor "Box") [GrinLitValue (GrinLitInt 1)])) $
                  GrinBind fetched (GrinFetch (GrinVarValue pointer)) $
                    GrinBind updated (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt 2))) $
                      GrinEval (GrinVarValue pointer)
            }
        ]
    }
  where
    answer = GrinVar "answer" 1
    pointer = GrinVar "pointer" 2
    fetched = GrinVar "fetched" 3
    updated = GrinVar "updated" 4
    functionName = FunctionName "answer_code"

intTy :: TcType
intTy = TcTyCon (TyCon "Int#" 0) []
