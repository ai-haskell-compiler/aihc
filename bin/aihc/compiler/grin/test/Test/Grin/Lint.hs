{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Lint (tests) where

import Aihc.Grin (GrinLintError (..), gcGrinProgram, lintCpsProgram, lintGcProgram, lintProgram, lowerGc, parseProgram, toCpsGrin)
import Aihc.Grin.Syntax
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "lint"
    [ testGroup
        "heap reservations"
        [ testCase "a dynamic reservation cannot cover a nursery store" $
            assertHeapLint nurseryProgram [GrinLintNurseryReservation 1 0] replaceNursery,
          testCase "a nursery reservation must cover every store" $
            assertHeapLint nurseryProgram [GrinLintNurseryReservation 1 0] removeWords,
          testCase "a nursery reservation cannot cover a dynamic allocation" $
            assertHeapLint dynamicProgram [GrinLintDynamicReservation "newByteArray#"] replaceDynamic
        ],
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
        ],
      testGroup
        "forwarded results"
        [ testCase "a forwarded function ends in a forwarded apply, and a call of it takes any layout" $
            assertEqual "direct GRIN lint" [] (lintProgram forwardedProgram),
          testCase "the same program passes after the CPS pass" $
            assertCpsLint [] forwardedProgram,
          testCase "a forwarded call cannot be bound" $
            assertEqual
              "direct GRIN lint"
              [GrinLintForwardedResultPlaced "bind"]
              (lintProgram (forwardedProgramWith boundForwardedCall)),
          testCase "a forwarded call cannot end a function with a layout" $
            assertEqual
              "direct GRIN lint"
              [GrinLintForwardedResultPlaced "function result"]
              (lintProgram (forwardedProgramWith placedFunctionForwards)),
          testCase "a forwarded function cannot place a value in tail position" $
            assertEqual
              "direct GRIN lint"
              [GrinLintForwardedFunctionPlaces [IntRep]]
              (lintProgram (forwardedProgramWith forwardedFunctionPlaces)),
          testCase "a call must expect the layout its callee declares" $
            assertEqual
              "direct GRIN lint"
              [GrinLintCallResult (FunctionName "one") liftedResultRep (ResultRep IntRep)]
              (lintProgram (forwardedProgramWith mismatchedCall)),
          testCase "a thunk cannot forward its result" $
            assertEqual
              "direct GRIN lint"
              [GrinLintThunkResult (FunctionName "forward") ResultForwarded]
              (lintProgram (forwardedProgramWith forwardedThunk))
        ]
    ]
  where
    shortThunkError = GrinLintFunctionArity (FunctionName "entry") 2 1

-- | @forward@ applies its argument to unit and forwards whatever comes
-- back; @useInt@ calls it for an 'IntRep' and @useUnit@ for a lifted value.
forwardedProgram :: GrinProgram
forwardedProgram = forwardedProgramWith []

forwardedProgramWith :: [GrinFunction] -> GrinProgram
forwardedProgramWith extra =
  GrinProgram
    { grinConstructors = [pubConstructor "Unit" []],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [pubGlobal "unit" (GrinNode (GrinConstructor "Unit" 0) [])],
      grinFunctions =
        [ GrinFunction
            (FunctionName "forward")
            [boxed "k"]
            ResultForwarded
            (GrinApply ResultForwarded (GrinVarValue (boxed "k")) [[GrinGlobalValue "unit"]]),
          GrinFunction
            (FunctionName "useInt")
            [boxed "k"]
            (ResultRep IntRep)
            ( GrinBind
                [int "n"]
                (GrinCall (ResultRep IntRep) (FunctionName "forward") [GrinVarValue (boxed "k")])
                (GrinConstant [GrinVarValue (int "n")])
            ),
          GrinFunction
            (FunctionName "useUnit")
            [boxed "k"]
            liftedResultRep
            (GrinCall liftedResultRep (FunctionName "forward") [GrinVarValue (boxed "k")]),
          GrinFunction
            (FunctionName "one")
            []
            liftedResultRep
            (GrinConstant [GrinGlobalValue "unit"])
        ]
          <> extra
    }

boundForwardedCall :: [GrinFunction]
boundForwardedCall =
  [ GrinFunction
      (FunctionName "bound")
      [boxed "k"]
      (ResultRep IntRep)
      ( GrinBind
          [int "n"]
          (GrinCall ResultForwarded (FunctionName "forward") [GrinVarValue (boxed "k")])
          (GrinConstant [GrinVarValue (int "n")])
      )
  ]

placedFunctionForwards :: [GrinFunction]
placedFunctionForwards =
  [ GrinFunction
      (FunctionName "placed")
      [boxed "k"]
      (ResultRep IntRep)
      (GrinCall ResultForwarded (FunctionName "forward") [GrinVarValue (boxed "k")])
  ]

forwardedFunctionPlaces :: [GrinFunction]
forwardedFunctionPlaces =
  [ GrinFunction
      (FunctionName "places")
      [int "n"]
      ResultForwarded
      (GrinConstant [GrinVarValue (int "n")])
  ]

mismatchedCall :: [GrinFunction]
mismatchedCall =
  [ GrinFunction
      (FunctionName "mismatched")
      []
      (ResultRep IntRep)
      (GrinCall (ResultRep IntRep) (FunctionName "one") [])
  ]

forwardedThunk :: [GrinFunction]
forwardedThunk =
  [ GrinFunction
      (FunctionName "suspendForward")
      [boxed "k"]
      liftedResultRep
      (GrinStore (GrinNode (GrinThunk (FunctionName "forward")) [GrinVarValue (boxed "k")]))
  ]

int :: Text -> GrinVar
int name = GrinVar name 0 IntRep

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
    { grinConstructors = [pubConstructor "Unit" []],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [pubGlobal "unit" (GrinNode (GrinConstructor "Unit" 0) [])],
      grinFunctions =
        [ GrinFunction
            (FunctionName "entry")
            [boxed "x", boxed "y"]
            liftedResultRep
            (GrinConstant [GrinVarValue (boxed "x")]),
          GrinFunction
            (FunctionName "suspend")
            []
            liftedResultRep
            (GrinStore (GrinNode (GrinThunk (FunctionName "entry")) fields))
        ]
    }
  where
    fields = replicate fieldCount (GrinGlobalValue "unit")

boxed :: Text -> GrinVar
boxed name = GrinVar name 0 liftedGrinRep

-- | Replace the generated reservation, then check the GC-GRIN diagnostics.
assertHeapLint :: Text -> [GrinLintError] -> (GrinExpr -> GrinExpr) -> IO ()
assertHeapLint sourceText expected change = do
  program' <- either (assertFailure . show) pure (parseProgram sourceText)
  cps <- either (assertFailure . show) pure (toCpsGrin program')
  let gc = lowerGc cps
      source = gcGrinProgram gc
      changed = source {grinFunctions = map (\function -> function {grinFunctionBody = alter (grinFunctionBody function)}) (grinFunctions source)}
      alter expression = case expression of
        GrinBind vars value body -> GrinBind vars (alter value) (alter body)
        _ -> change expression
  assertEqual "heap reservation diagnostics" expected (lintGcProgram gc {gcGrinProgram = changed})

replaceNursery :: GrinExpr -> GrinExpr
replaceNursery (GrinEnsureNursery words' roots) = GrinEnsureHeap (GrinLitValue (GrinLitInt WordRep words')) roots
replaceNursery expression = expression

removeWords :: GrinExpr -> GrinExpr
removeWords (GrinEnsureNursery _ roots) = GrinEnsureNursery 0 roots
removeWords expression = expression

replaceDynamic :: GrinExpr -> GrinExpr
replaceDynamic (GrinEnsureHeap _ roots) = GrinEnsureNursery 8 roots
replaceDynamic expression = expression

nurseryProgram :: Text
nurseryProgram = "constructor Unit/0 []\n$entry -> BoxedRep Lifted =\n  store (CUnit)\n"

dynamicProgram :: Text
dynamicProgram = "primitive newByteArray# :: BoxedRep Unlifted/1\n$entry -> BoxedRep Unlifted =\n  primitive-call @(BoxedRep Unlifted) newByteArray# (8 :: IntRep)\n"
