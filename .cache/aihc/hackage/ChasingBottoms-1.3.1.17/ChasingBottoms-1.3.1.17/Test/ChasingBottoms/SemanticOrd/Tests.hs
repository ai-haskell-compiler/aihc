{-# LANGUAGE RankNTypes #-}

-- | Tests for "Test.ChasingBottoms.SemanticOrd". The functions using
-- tweaks are currently not tested.

module Test.ChasingBottoms.SemanticOrd.Tests (tests) where

import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.TestUtilities
import Test.ChasingBottoms.TestUtilities.Generators as G hiding (tests)
import Test.QuickCheck
import Data.Generics
import Control.Monad
import Data.Maybe

------------------------------------------------------------------------
-- The actual tests

prop_SemanticEq_congruence :: (Data a, Show a)
                           => Gen a
                           -> NotEqualGen a
                           -> Cogen a
                           -> [Property]
prop_SemanticEq_congruence element notEqualTo coGen =
  isCongruence element return notEqualTo (==!) (/=!)
               (G.function coGen integer) (==!)

prop_SemanticOrd_partial_order :: (Data a, Show a)
                               => Gen a
                               -> NotEqualGen a
                               -> GreaterEqualGen a
                               -> JoinableGen a
                               -> [Property]
prop_SemanticOrd_partial_order element notEqualTo greaterThan joinable =
  isPartialOrder element return notEqualTo greaterThan (==!) (<=!) ++
  isPartialOrderOperators element greaterThan (==!) (<=!) (<!) (>=!) (>!) ++
  [ compare
  , meet_associative, meet_commutative, meet_idempotent, meet_lt
  , join_associative, join_commutative, join_idempotent, join_lt
  , join_meet_absorption, meet_join_absorption
  ]
  where
  twoElems = pair3 element greaterThan

  compare =
    forAll twoElems $ \(x, y) ->
      case semanticCompare noTweak x y of
        Nothing         -> not (x <=! y) && not (x >=! y)
        Just LT         -> x <! y
        Just EQ         -> x ==! y
        Just Prelude.GT -> x >! y

  meet_associative = isAssociative (oneof [ liftM3 (,,) element element element
                                          , triple element joinable joinable
                                          ]
                                   )
                                   (==!) (/\!)
  meet_commutative = isCommutative (oneof [ liftM2 (,) element element
                                          , pair element joinable
                                          ]
                                   )
                                   (==!) (/\!)
  meet_idempotent  = isIdempotent element (==!) (/\!)

  join_associative =
    forAll (triple element joinable joinable) $ \(x, y, z) ->
      (x \/! y >>= (\/! z)) ==! ((x \/!) =<< y \/! z)
  join_commutative = isCommutative (pair element joinable) (==!) (\/!)
  join_idempotent  =
    forAll element $ \x ->
      x \/! x ==! Just x

  join_meet_absorption =
    forAll jmPair $ \(x, y) ->
      x \/! (x /\! y) ==! Just x
    where jmPair = oneof [ liftM2 (,) element element
                         , pair element joinable
                         ]
  meet_join_absorption =
    forAll (pair element joinable) $ \(x, y) ->
      x /\! fromJust (x \/! y) ==! x

  twoElems' = frequency [ (2, twoElems), (1, pair element joinable) ]

  meet_lt =
    forAll twoElems' $ \(x, y) ->
      (x <=! y) == (x /\! y ==! x)
  join_lt =
    forAll twoElems' $ \(x, y) ->
      (x <=! y) == (x \/! y ==! Just y)

-- | All tests collected together.

tests :: IO Bool
tests = runQuickCheckTests $ map run $ concat theTests
  where
  theTests =
    [ prop_SemanticEq_congruence bool neBool coBool
    , prop_SemanticEq_congruence integer neInteger coInteger
    , prop_SemanticEq_congruence (finiteListOf bool)
                                 (neListOf bool neBool finiteListOf)
                                 (coListOf coBool)
    , prop_SemanticEq_congruence (finiteTreeOf integer)
                                 (neTreeOf integer neInteger finiteTreeOf)
                                 (coTreeOf coInteger)
    , prop_SemanticOrd_partial_order bool neBool geBool joinBool
    , prop_SemanticOrd_partial_order integer neInteger geInteger joinInteger
    , prop_SemanticOrd_partial_order (finiteListOf bool)
                                     (neListOf bool neBool finiteListOf)
                                     (geListOf bool geBool finiteListOf)
                                     (joinListOf bool joinBool finiteListOf)
    , prop_SemanticOrd_partial_order (finiteTreeOf integer)
                                     (neTreeOf integer neInteger finiteTreeOf)
                                     (geTreeOf integer geInteger finiteTreeOf)
                                     (joinTreeOf integer joinInteger
                                                 finiteTreeOf)
    ]
