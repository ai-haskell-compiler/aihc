{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cps
import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (IntRep), liftedRuntimeRep)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = defaultMain (testGroup "aihc-cps" tests)

tests :: [TestTree]
tests =
  [ testCase "a GRIN bind becomes a named continuation" testBindLowering,
    testCase "nested binds retain the enclosing continuation" testNestedBinds,
    testCase "the linter rejects a continuation argument layout mismatch" testContinuationMismatch,
    testCase "the renderer identifies Loom IR and explicit transfers" testRendering,
    QC.testProperty "dummy quickcheck property" prop_dummy
  ]

-- | Keep the workspace-wide QuickCheck controls accepted by this suite.
prop_dummy :: Bool -> Bool
prop_dummy _ = True

testBindLowering :: IO ()
testBindLowering =
  case loomFunctions (lowerProgram (singleFunctionProgram body)) of
    [function] -> do
      assertEqual "return continuation layout" [IntRep] (loomContRuntimeReps (loomFunctionReturn function))
      case loomFunctionBody function of
        LoomLetCont continuation (LoomInvoke (LoomEval IntRep _) target) -> do
          assertEqual "operation target" (loomContinuationName continuation) target
          assertEqual "continuation parameters" [result] (loomContinuationParameters continuation)
          assertEqual
            "continuation body"
            (LoomContinue (loomFunctionReturn function) [GrinVarValue result])
            (loomContinuationBody continuation)
        actual -> fail ("unexpected lowering: " <> show actual)
      assertEqual "lowered program lints" [] (lintProgram (lowerProgram (singleFunctionProgram body)))
    actual -> fail ("expected one function, got: " <> show actual)
  where
    input = GrinVar "input" 1 liftedRuntimeRep
    result = GrinVar "result" 2 IntRep
    body = GrinBind [result] (GrinEval IntRep (GrinVarValue input)) (GrinReturn [GrinVarValue result])

testNestedBinds :: IO ()
testNestedBinds =
  case loomFunctionBody (onlyFunction (lowerProgram (singleFunctionProgram body))) of
    LoomLetCont outer (LoomInvoke _ outerTarget) -> do
      assertEqual "first operation targets outer continuation" (loomContinuationName outer) outerTarget
      case loomContinuationBody outer of
        LoomLetCont inner (LoomInvoke _ innerTarget) -> do
          assertEqual "second operation targets inner continuation" (loomContinuationName inner) innerTarget
          assertEqual
            "inner continuation returns to the function caller"
            (LoomContinue returnContinuation [GrinVarValue second])
            (loomContinuationBody inner)
        actual -> fail ("expected nested continuation, got: " <> show actual)
    actual -> fail ("expected outer continuation, got: " <> show actual)
  where
    first = GrinVar "first" 2 liftedRuntimeRep
    second = GrinVar "second" 3 IntRep
    pointer = GrinVar "pointer" 1 liftedRuntimeRep
    body =
      GrinBind
        [first]
        (GrinFetch liftedRuntimeRep (GrinVarValue pointer))
        (GrinBind [second] (GrinEval IntRep (GrinVarValue first)) (GrinReturn [GrinVarValue second]))
    function = onlyFunction (lowerProgram (singleFunctionProgram body))
    returnContinuation = loomFunctionReturn function

testContinuationMismatch :: IO ()
testContinuationMismatch = do
  let returnContinuation = LoomContVar "return" 0 [IntRep]
      function =
        LoomFunction
          { loomFunctionName = FunctionName "main",
            loomFunctionParameters = [],
            loomFunctionReturn = returnContinuation,
            loomFunctionBody = LoomContinue returnContinuation [GrinNodeValue (GrinNode (GrinConstructor "()") [])]
          }
      program = emptyLoomProgram {loomFunctions = [function]}
  assertEqual
    "mismatched value representation"
    [LoomLintContinuationArguments returnContinuation [IntRep] [liftedRuntimeRep]]
    (lintProgram program)

testRendering :: IO ()
testRendering = do
  let rendered = renderProgram (lowerProgram (singleFunctionProgram body))
  assertBool "versioned Loom header" ("loom-ir 1" `isInfixOf` rendered)
  assertBool "explicit return continuation" ("return return%0" `isInfixOf` rendered)
  assertBool "explicit operation transfer" ("eval @IntRep" `isInfixOf` rendered && " -> k%1" `isInfixOf` rendered)
  where
    input = GrinVar "input" 1 liftedRuntimeRep
    result = GrinVar "result" 2 IntRep
    body = GrinBind [result] (GrinEval IntRep (GrinVarValue input)) (GrinReturn [GrinVarValue result])

singleFunctionProgram :: GrinExpr -> GrinProgram
singleFunctionProgram body =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "main",
              grinFunctionParameters = [GrinVar "input" 1 liftedRuntimeRep],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = body
            }
        ]
    }

emptyLoomProgram :: LoomProgram
emptyLoomProgram =
  LoomProgram
    { loomConstructors = [],
      loomPrimitives = [],
      loomForeignCalls = [],
      loomWhnfGlobals = [],
      loomCafs = [],
      loomFunctions = []
    }

onlyFunction :: LoomProgram -> LoomFunction
onlyFunction program =
  case loomFunctions program of
    [function] -> function
    actual -> error ("expected one function, got: " <> show actual)
