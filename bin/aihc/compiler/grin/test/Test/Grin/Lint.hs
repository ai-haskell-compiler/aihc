{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Lint (tests) where

import Aihc.Grin (GrinLintError (..), lintCpsProgram, lintProgram, toCpsGrin)
import Aihc.Grin.Syntax
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "node arities"
    [ testCase "a direct thunk node supplies every parameter" $
        assertEqual "direct GRIN lint" [] (lintProgram (program 2)),
      testCase "a CPS thunk node does not supply the hidden continuation" $
        assertCpsLint [] (program 2),
      testCase "a short direct thunk node fails" $
        assertEqual "direct GRIN lint" [shortThunkError] (lintProgram (program 1)),
      testCase "a short CPS thunk node fails with the same arity" $
        assertCpsLint [shortThunkError] (program 1)
    ]
  where
    shortThunkError = GrinLintFunctionArity (FunctionName "entry") 2 1

assertCpsLint :: [GrinLintError] -> GrinProgram -> IO ()
assertCpsLint expected sourceProgram =
  case toCpsGrin sourceProgram of
    Left problem -> assertFailure ("CPS transformation failed: " <> show problem)
    Right cps -> assertEqual "CPS-GRIN lint" expected (lintCpsProgram cps)

-- | A program whose thunk node gives @fieldCount@ of the two values that its
-- entry needs. The CPS transformation gives the entry a third parameter, but
-- the node keeps its fields. Only two fields are correct in each phase.
program :: Int -> GrinProgram
program fieldCount =
  GrinProgram
    { grinConstructors = [("Unit", [])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [("unit", GrinNode (GrinConstructor "Unit" 0) [])],
      grinFunctions =
        [ GrinFunction
            (FunctionName "entry")
            [boxed "x", boxed "y"]
            liftedGrinRep
            (GrinConstant [GrinVarValue (boxed "x")]),
          GrinFunction
            (FunctionName "suspend")
            []
            liftedGrinRep
            (GrinStore (GrinNode (GrinThunk (FunctionName "entry")) fields))
        ]
    }
  where
    fields = replicate fieldCount (GrinGlobalValue "unit")

boxed :: Text -> GrinVar
boxed name = GrinVar name 0 liftedGrinRep
