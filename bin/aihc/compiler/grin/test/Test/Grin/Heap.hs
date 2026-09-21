{-# LANGUAGE OverloadedStrings #-}

-- | The shape of the heap reservations 'lowerGc' leaves behind.
--
-- A snapshot fixture reports the bytes a program took from the heap, which is
-- the same whether one reservation covers a group of stores or each store
-- reserves for itself. These tests assert the grouping itself, on the GC-GRIN
-- rather than at run time.
module Test.Grin.Heap (tests) where

import Aihc.Grin
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "heap reservations"
    [ testCase "a case reserves once for its largest branch" $ do
        reservations <- entryReservations caseProgram
        assertEqual "reservations" [7] reservations,
      testCase "stores between two barriers share one reservation" $ do
        reservations <- entryReservations straightLineProgram
        -- Two two-word nodes, reserved once rather than twice.
        assertEqual "reservations" [4] reservations,
      testCase "a call ends a reservation" $ do
        reservations <- allReservations acrossCallProgram
        -- The entry reserves the first node and the continuation the call
        -- needs; the store after the call reserves in that continuation
        -- instead, because the call itself can collect.
        assertEqual
          "reservations"
          [("$entry", [5]), ("$entry_cont", [2])]
          reservations,
      testCase "a primitive that cannot allocate keeps one reservation" $ do
        reservations <- entryReservations (betweenStoresProgram "+#" 2 "(2 :: IntRep) (2 :: IntRep)")
        -- The addition is one instruction and takes nothing from the heap,
        -- so both nodes stay under the reservation that reaches it.
        assertEqual "reservations" [4] reservations,
      testCase "a primitive that allocates ends a reservation" $ do
        reservations <- entryReservations (betweenStoresProgram "newMutVar#" 1 "(0 :: IntRep)")
        -- The first reservation has two slots for the node and three for
        -- the mutable reference. The second store needs a separate reservation.
        assertEqual "reservations" [5, 2] reservations
    ]

-- | The words of every reservation of the entry function, in the order the
-- lowering left them.
entryReservations :: Text -> IO [Integer]
entryReservations source = do
  program <- either (assertFailure . renderParseError) pure (parseProgram source)
  cps <- either (assertFailure . show) pure (toCpsGrin program)
  let functions = grinFunctions (gcGrinProgram (lowerGc cps))
  case [function | function <- functions, grinFunctionName function == FunctionName "$entry"] of
    [function] -> pure (reservationWords (grinFunctionBody function))
    _ -> assertFailure "the program has no single entry function"

-- | The reservations of every function that has one, by name.
allReservations :: Text -> IO [(Text, [Integer])]
allReservations source = do
  program <- either (assertFailure . renderParseError) pure (parseProgram source)
  cps <- either (assertFailure . show) pure (toCpsGrin program)
  pure
    [ (unFunctionName (grinFunctionName function), reservations)
    | function <- grinFunctions (gcGrinProgram (lowerGc cps)),
      let reservations = reservationWords (grinFunctionBody function),
      not (null reservations)
    ]

reservationWords :: GrinExpr -> [Integer]
reservationWords expression =
  case expression of
    GrinBind _ (GrinEnsureHeap (GrinLitValue (GrinLitInt _ requiredWords)) _) body ->
      requiredWords : reservationWords body
    GrinBind _ value body -> reservationWords value <> reservationWords body
    GrinStoreRec _ body -> reservationWords body
    GrinStoreRecUnchecked _ body -> reservationWords body
    GrinCase _ _ alternatives -> concatMap (reservationWords . grinAltRhs) alternatives
    GrinEnsureHeap (GrinLitValue (GrinLitInt _ requiredWords)) _ -> [requiredWords]
    _ -> []

-- | Two branches of different widths. The reservation is hoisted above the
-- case and covers the larger one, so neither branch reserves for itself.
caseProgram :: Text
caseProgram =
  T.unlines
    [ "constructor Small/4 [IntRep, IntRep, IntRep, IntRep]",
      "constructor Large/6 [IntRep, IntRep, IntRep, IntRep, IntRep, IntRep]",
      "",
      "$entry -> BoxedRep Lifted =",
      "  case (1 :: IntRep) as choice%1 :: IntRep of",
      "    0 ->",
      "      store (CSmall (1 :: IntRep) (2 :: IntRep) (3 :: IntRep) (4 :: IntRep))",
      "    _ ->",
      "      store (CLarge (1 :: IntRep) (2 :: IntRep) (3 :: IntRep) (4 :: IntRep) (5 :: IntRep) (6 :: IntRep))"
    ]

-- | Two stores with nothing between them that can collect: one reservation
-- covers both objects.
straightLineProgram :: Text
straightLineProgram =
  T.unlines
    [ "constructor Box/1 [IntRep]",
      "constructor Link/1 [BoxedRep Lifted]",
      "",
      "$entry -> BoxedRep Lifted =",
      "  (inner%1 :: BoxedRep Lifted) <- store (CBox (1 :: IntRep))",
      "  store (CLink (inner%1 :: BoxedRep Lifted))"
    ]

-- | Two stores with one call of the named primitive between them. Whether
-- the primitive allocates decides whether the two stores share one
-- reservation.
betweenStoresProgram :: Text -> Int -> Text -> Text
betweenStoresProgram primitive arity arguments =
  T.unlines
    [ "constructor Box/1 [IntRep]",
      "constructor Link/1 [BoxedRep Lifted]",
      "",
      "primitive " <> primitive <> " :: IntRep/" <> T.pack (show arity),
      "",
      "$entry -> BoxedRep Lifted =",
      "  (first%1 :: BoxedRep Lifted) <- store (CBox (1 :: IntRep))",
      "  (made%2 :: IntRep) <- primitive-call @IntRep " <> primitive <> " " <> arguments,
      "  store (CLink (first%1 :: BoxedRep Lifted))"
    ]

-- | A call between the two stores can collect, so it separates them into two
-- reservations.
acrossCallProgram :: Text
acrossCallProgram =
  T.unlines
    [ "constructor Box/1 [IntRep]",
      "constructor Link/1 [BoxedRep Lifted]",
      "",
      "$other -> IntRep =",
      "  constant (7 :: IntRep)",
      "",
      "$entry -> BoxedRep Lifted =",
      "  (first%1 :: BoxedRep Lifted) <- store (CBox (1 :: IntRep))",
      "  (value%2 :: IntRep) <- call @IntRep $other",
      "  store (CLink (first%1 :: BoxedRep Lifted))"
    ]
