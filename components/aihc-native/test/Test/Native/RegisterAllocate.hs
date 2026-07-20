{-# LANGUAGE OverloadedStrings #-}

module Test.Native.RegisterAllocate
  ( tests,
  )
where

import Aihc.Grin.Syntax
import Aihc.Native.RegisterAllocate
import Aihc.Tc.Types (RuntimeRep (IntRep), liftedRuntimeRep)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

data TestReg = First | Second
  deriving (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "native support"
    [ testCase "reuses a parameter register for a tail-recursive result" $ do
        let current = intVar "current" 0
            next = intVar "next" 1
            continuation = pointerVar "continuation" 2
            function =
              grinFunction
                [current, continuation]
                ( GrinBind
                    [next]
                    (GrinPrimitiveCall IntRep "+#" [var current, int 1])
                    (GrinCall IntRep (FunctionName "loop") [var next, var continuation])
                )
            allocation = allocate [First, Second] function
        assertEqual "current and its successor share a register" (location current allocation) (location next allocation)
        assertBool "continuation remains distinct" (location current allocation /= location continuation allocation),
      testCase "keeps simultaneously live values apart" $ do
        let left = intVar "left" 0
            right = intVar "right" 1
            result = intVar "result" 2
            function =
              grinFunction
                [left, right]
                ( GrinBind
                    [result]
                    (GrinPrimitiveCall IntRep "+#" [var left, var right])
                    (GrinHalt [var result])
                )
            allocation = allocate [First, Second] function
        assertBool "operands interfere" (location left allocation /= location right allocation),
      testCase "keeps live and unused incoming parameters apart" $ do
        let used = intVar "used" 0
            unused = intVar "unused" 1
            function = grinFunction [used, unused] (GrinHalt [var used])
            allocation = allocate [First, Second] function
        assertBool "entry copies cannot overwrite a live parameter" (location used allocation /= location unused allocation),
      testCase "spills a value live across a C call" $ do
        let survivor = intVar "survivor" 0
            allocated = pointerVar "allocated" 1
            function =
              grinFunction
                [survivor]
                ( GrinBind
                    [allocated]
                    (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                    (GrinHalt [var survivor, var allocated])
                )
            allocation = allocate [First, Second] function
        assertEqual "call-spanning home" (Just (InHeapSpill 0)) (location survivor allocation),
      testCase "reuses non-overlapping spill slots" $ do
        let first = intVar "first" 0
            firstResult = pointerVar "firstResult" 1
            second = intVar "second" 2
            secondResult = pointerVar "secondResult" 3
            function =
              grinFunction
                [first]
                ( GrinBind
                    [firstResult]
                    (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                    ( GrinBind
                        [second]
                        (GrinConstant [int 2])
                        ( GrinBind
                            [secondResult]
                            (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                            (GrinHalt [var second, var secondResult])
                        )
                    )
                )
            allocation = allocate ([] :: [TestReg]) function
        assertEqual "only simultaneously live spills need distinct slots" 2 (allocationSpillCount allocation),
      testCase "uses only registers supplied by the backend" $ do
        let variables = map (uncurry intVar) [("a", 0), ("b", 1), ("c", 2)]
            function = grinFunction variables (GrinHalt (map var variables))
            allocation = allocate [First, Second] function
        assertBool "second backend register used" (InRegister Second `elem` Map.elems (allocationLocations allocation))
        assertEqual "one heap spill" 1 (allocationSpillCount allocation),
      testCase "does not allocate homes for free globals" $ do
        let global = pointerVar "global" 99
            function = grinFunction [] (GrinHalt [var global])
            allocation = allocate [First, Second] function
        assertBool "globals are materialized by the backend" (Map.notMember global (allocationLocations allocation))
    ]
  where
    allocate registers =
      allocateFunction
        AllocatorConfig
          { allocatorRegisters = registers,
            allocatorFixedLocations = Map.empty
          }
    location variable allocation = Map.lookup variable (allocationLocations allocation)

grinFunction :: [GrinVar] -> GrinExpr -> GrinFunction
grinFunction parameters body =
  GrinFunction
    { grinFunctionName = FunctionName "loop",
      grinFunctionLinkName = Nothing,
      grinFunctionParameters = parameters,
      grinFunctionResultRep = IntRep,
      grinFunctionBody = body
    }

intVar :: Text -> Int -> GrinVar
intVar name unique = GrinVar name unique IntRep

pointerVar :: Text -> Int -> GrinVar
pointerVar name unique = GrinVar name unique liftedRuntimeRep

var :: GrinVar -> GrinValue
var = GrinVarValue

int :: Integer -> GrinValue
int = GrinLitValue . GrinLitInt IntRep
