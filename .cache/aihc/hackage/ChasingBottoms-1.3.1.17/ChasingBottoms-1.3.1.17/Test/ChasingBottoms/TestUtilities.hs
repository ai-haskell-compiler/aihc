{-# LANGUAGE CPP #-}

-- | Some utilities that are part of the testing framework.

module Test.ChasingBottoms.TestUtilities
  ( -- * Batch execution of QuickCheck tests
    run
  , runQuickCheckTests
    -- * Various algebraic properties
  , isAssociative
  , isCommutative
  , isIdempotent
    -- ** Equivalence and congruence
  , isEquivalenceRelation
  , isCongruence
  , eqIsCongruence
    -- ** Partial and total orders
  , isPartialOrder
  , isTotalOrder
  , isPartialOrderOperators
  , isTotalOrderOperators
  , ordIsTotalOrder
    -- * Helper functions
  , pair
  , triple
  , pair3
  ) where

import Test.QuickCheck
import Data.List
import Control.Arrow
import Control.Monad
import Text.Show.Functions

------------------------------------------------------------------------
-- Batch execution of QuickCheck tests

-- | Runs a single test, using suitable settings.

run :: Testable p => p -> IO Result
run = quickCheckWithResult (stdArgs { maxSuccess = 1000
                                    , maxDiscardRatio = 5
                                    })

-- | Runs a bunch of QuickCheck tests, printing suitable information
-- to standard output. Returns 'True' if no tests fail.

runQuickCheckTests :: [IO Result]
                      -- ^ Create the tests in this list from ordinary
                      -- QuickCheck tests by using 'run'.
                      -> IO Bool
runQuickCheckTests tests = do
  results <- sequence tests
  mapM_ (putStrLn . showTR) results
  return $ all ok $ results
  where
  ok (Success {})           = True
  ok (GaveUp {})            = False
  ok (Failure {})           = False
  ok (NoExpectedFailure {}) = False

  showTR (Success {})              = "OK."
  showTR (GaveUp { numTests = n }) =
    "Gave up after " ++ show n ++ " tests."
  showTR (Failure {})              = "Test failed."
  showTR (NoExpectedFailure {})    =
    "Test did not fail, but it should have."

------------------------------------------------------------------------
-- Testing various algebraic properties

-- | Test for associativity.

isAssociative
  :: Show a
     => Gen (a, a, a)
     -- ^ Generator for arbitrary elements, possibly related in some
     -- way to make the test more meaningful.
     -> (a -> a -> Bool)
     -- ^ Equality test.
     -> (a -> a -> a)
     -- ^ The operation.
     -> Property
isAssociative triple (==.) (+.) =
  forAll triple $ \(x, y, z) ->
    ((x +. y) +. z) ==. (x +. (y +. z))

-- | Test for commutativity.

isCommutative
  :: Show a
     => Gen (a, a)
     -- ^ Generator for arbitrary elements, possibly related in some
     -- way to make the test more meaningful.
     -> (b -> b -> Bool)
     -- ^ Equality test.
     -> (a -> a -> b)
     -- ^ The operation.
     -> Property
isCommutative pair (==.) (+.) =
  forAll pair $ \(x, y) ->
    (x +. y) ==. (y +. x)

-- | Test for idempotence.

isIdempotent
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> a -> Bool)
     -- ^ Equality test.
     -> (a -> a -> a)
     -- ^ The operation.
     -> Property
isIdempotent element (==.) (+.) =
  forAll element $ \x ->
    (x +. x) ==. x

-- | Tests for an equivalence relation. Requires that the relation is
-- neither always false nor always true.

isEquivalenceRelation
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isEquivalenceRelation element equalTo notEqualTo (===) =
  [reflexive, symmetric1, symmetric2, transitive]
  where
  x /== y = not (x === y)

  reflexive = forAll element $ \x ->
                x === x

  symmetric1 = forAll (pair element equalTo) $ \(x, y) ->
                 x === y && y === x

  symmetric2 = forAll (pair element notEqualTo) $ \(x, y) ->
                 x /== y && y /== x

  transitive = forAll (pair element equalTo) $ \(x, y) ->
                 forAll (equalTo y) $ \z ->
                   x === z

-- | Tests for a congruence. Also tests that the negated relation is
-- the negation of the relation.

isCongruence
  :: (Show a, Eq b)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> (a -> a -> Bool)
     -- ^ The negated relation.
     -> Gen (a -> b)
     -- ^ Generator for functions.
     -> (b -> b -> Bool)
     -- ^ Equality for function result type.
     -> [Property]
isCongruence element equalTo notEqualTo (===) (/==) function (.===) =
  isEquivalenceRelation element equalTo notEqualTo (===)
  ++ [cong, eq_neq1, eq_neq2]
  where
  cong = forAll function $ \f ->
           forAll (pair element equalTo) $ \(x, y) ->
             f x .=== f y
  eq_neq1 = forAll (pair element equalTo) $ \(x, y) ->
              x === y && not (x /== y)
  eq_neq2 = forAll (pair element notEqualTo) $ \(x, y) ->
              not (x === y) && x /== y

-- | Test that an 'Eq' instance is a congruence.

eqIsCongruence
  :: (Show a, Eq a, Eq b)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> Gen (a -> b)
     -- ^ Generator for functions.
     -> [Property]
eqIsCongruence element equalTo notEqualTo function =
  isCongruence element equalTo notEqualTo (==) (/=) function (==)

-- | Tests for a partial order.

isPartialOrder
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Underlying equality relation.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isPartialOrder element equalTo differentFrom greaterThan (==.) (<=.) =
  [reflexive, antisymmetric1, antisymmetric2, transitive]
  where
  reflexive =
    forAll element $ \x ->
      x <=. x

  antisymmetric1 =
    forAll (pair element equalTo) $ \(x, y) ->
      ((x <=. y) && (y <=. x)) && x ==. y

  antisymmetric2 =
    forAll (pair element differentFrom) $ \(x, y) ->
      not ((x <=. y) && (y <=. x)) && not (x ==. y)

  transitive = forAll (pair element greaterThan) $ \(x, y) ->
                 forAll (greaterThan y) $ \z ->
                   x <=. z

-- | Tests for a total order.

isTotalOrder
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Underlying equality relation.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isTotalOrder element equalTo differentFrom greaterThan (==.) (<=.) =
  isPartialOrder element equalTo differentFrom greaterThan (==.) (<=.)
  ++ [total]
  where
  total =
    forAll element $ \x ->
    forAll element $ \y ->
      (x <=. y) || (y <=. x)

-- | Tests relating various partial order operators. Does not include
-- any tests from 'isPartialOrder'.

isPartialOrderOperators
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Equal.
     -> (a -> a -> Bool)
     -- ^ Less than or equal.
     -> (a -> a -> Bool)
     -- ^ Less than.
     -> (a -> a -> Bool)
     -- ^ Greater than or equal.
     -> (a -> a -> Bool)
     -- ^ Greater than.
     -> [Property]
isPartialOrderOperators element greaterThan (==.) (<=.) (<.) (>=.) (>.) =
  [lt_le, gt_ge, ge_le, lt_gt]
  where
  twoElems = pair3 element greaterThan

  lt_le =
    forAll twoElems $ \(x, y) ->
      (x <. y) == ((x <=. y) && not (x ==. y))

  gt_ge =
    forAll twoElems $ \(x, y) ->
      (x >. y) == ((x >=. y) && not (x ==. y))

  ge_le =
    forAll twoElems $ \(x, y) ->
      (x >=. y) == (y <=. x)

  lt_gt =
    forAll twoElems $ \(x, y) ->
      (x <. y) == (y >. x)

-- | Tests relating various total order operators and functions. Does
-- not include any tests from 'isTotalOrder'.

isTotalOrderOperators
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Equal.
     -> (a -> a -> Bool)
     -- ^ Less than or equal.
     -> (a -> a -> Bool)
     -- ^ Less than.
     -> (a -> a -> Bool)
     -- ^ Greater than or equal.
     -> (a -> a -> Bool)
     -- ^ Greater than.
     -> (a -> a -> Ordering)
     -- ^ Compare.
     -> (a -> a -> a)
     -- ^ Minimum.
     -> (a -> a -> a)
     -- ^ Maximum.
     -> [Property]
isTotalOrderOperators element greaterThan
                      (==.) (<=.) (<.) (>=.) (>.) cmp mn mx =
  isPartialOrderOperators element greaterThan (==.) (<=.) (<.) (>=.) (>.)
  ++ [compare_lt_eq_gt, compare_max, compare_min]
  where
  twoElems = pair3 element greaterThan

  compare_lt_eq_gt =
    forAll twoElems $ \(x, y) ->
      case cmp x y of
        LT -> x <. y
        EQ -> x ==. y
        GT -> x >. y

  compare_max =
    forAll twoElems $ \(x, y) ->
      case cmp x y of
        LT -> x `mx` y ==. y
        GT -> x `mx` y ==. x
        EQ -> elemBy (==.) (x `mx` y) [x, y]

  compare_min =
    forAll twoElems $ \(x, y) ->
      case cmp x y of
        LT -> x `mn` y ==. x
        GT -> x `mn` y ==. y
        EQ -> elemBy (==.) (x `mn` y) [x, y]

  elemBy op x xs = any (`op` x) xs

-- | Tests that an 'Ord' instance should satisfy to be a total order.

ordIsTotalOrder
  :: (Show a, Ord a)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> [Property]
ordIsTotalOrder element equalTo differentFrom greaterThan =
  isTotalOrderOperators element greaterThan
                        (==) (<=) (<) (>=) (>) compare min max
  ++ isTotalOrder element equalTo differentFrom greaterThan (==) (<=)

------------------------------------------------------------------------
-- Helper functions

-- | Given two generators, generates a pair where the second component
-- depends on the first.

pair :: Gen a -> (a -> Gen b) -> Gen (a, b)
pair gen1 gen2 = do
  x <- gen1
  y <- gen2 x
  return (x, y)

-- | 'triple' works like 'pair', but for triples.

triple :: Gen a -> (a -> Gen b) -> (b -> Gen c) -> Gen (a, b, c)
triple gen1 gen2 gen3 = do
  x <- gen1
  y <- gen2 x
  z <- gen3 y
  return (x, y, z)

-- | Given two generators, where the second one depends on elements
-- generated by the first one, 'pair3' generates three kinds of pairs:
--
--   1. Containing two elements from the first generator.
--
--   2. Containing one element from the first and one from the second.
--
--   3. Containing one element from the second and one from the first.

pair3 :: Gen a -> (a -> Gen a) -> Gen (a, a)
pair3 gen1 gen2 =
 oneof [ liftM2 (,) gen1 gen1
       , pair gen1 gen2
       , fmap (snd &&& fst) $ pair gen1 gen2
       ]
