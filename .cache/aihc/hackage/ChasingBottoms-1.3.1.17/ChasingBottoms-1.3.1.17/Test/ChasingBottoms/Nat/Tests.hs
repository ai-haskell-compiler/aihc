{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for "Test.ChasingBottoms.Nat".

-- The automatically derived 'Typeable' instance is not tested.

module Test.ChasingBottoms.Nat.Tests (tests) where

import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.TestUtilities
import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Ratio

default ()

-- The default versions of succ and pred go via Ints, and hence
-- perform incorrectly in the presence of large natural numbers. Hence
-- this generator is needed. Other tests can possibly also fail in the
-- presence of large natural numbers, but QuickCheck does not handle
-- large numbers very well, especially not when coarbitrary is used,
-- so we do not use this generator for all tests. Furthermore
-- defaulting is turned off here and in Test.ChasingBottoms.Nat, and
-- that should minimise surprises.

largeNat :: Gen Nat
largeNat = do
  n <- choose (0, 2 * toInteger (maxBound :: Int))
  return (fromInteger n)

-- Testing isSucc.

prop_isSucc n = isSucc n == (n > 0)

-- Testing fromSucc.

prop_fromSucc n | n == 0    = fromSucc n == Nothing
                | otherwise = fromSucc n == Just (n-1)

-- Testing natrec.

-- How do you test something as versatile as natrec? Well, at least we
-- can verify that we can use it to implement addition.

prop_natrec_add m n = natrec m (\_ o -> succ o) n == m + n

-- There is no need to test foldN, since it is specified by its
-- definition.

-- Testing Enum.

prop_Nat_Enum_succ =
  forAll largeNat $ \n ->
    succ n == n + 1
prop_Nat_Enum_pred =
  forAll largeNat $ \n ->
    n > 0 ==> pred n == n - 1

prop_Nat_Enum_toEnum (n :: Int) =
   n >= 0 ==> (toEnum n :: Nat) == fromInteger (toInteger n)
prop_Nat_Enum_fromEnum (n :: Nat) =
  n <= fromInteger (toInteger (maxBound :: Int)) ==>
  toInteger (fromEnum n) == toInteger n

-- enumFrom and friends have default definitions.

-- Testing Eq.

prop_Nat_Eq_congruence =
  eqIsCongruence arbitrary equalTo notEqualTo
                 (arbitrary :: Gen (Nat -> Integer))

equalTo (n :: Nat) = return n

notEqualTo (n :: Nat) = do
  m <- fmap succ arbitrary  -- m >= 1.
  if m <= n then
    elements [n - m, n + m]
   else
    return (n + m)

-- Testing Show.

prop_Nat_Show (m :: Nat) = show m == show (toInteger m)

-- Testing Ord.

prop_Nat_Ord_total_order = ordIsTotalOrder arbitrary
                             equalTo notEqualTo greaterThanOrEqual

greaterThanOrEqual (n :: Nat) = do
  m <- arbitrary
  return (n + m)

-- Testing Num.

prop_Nat_mul_iterated_sum (m :: Nat) n =
  m * n == foldr (+) 0 (genericReplicate m n)

prop_Nat_plus_assoc (m :: Nat) n o = m + (n + o) == (m + n) + o
prop_Nat_plus_comm (m :: Nat) n    = m + n == n + m

prop_Nat_mul_assoc (m :: Nat) n o = m * (n * o) == (m * n) * o
prop_Nat_mul_comm (m :: Nat) n    = m * n == n * m

prop_Nat_mul_plus_left_dist (m :: Nat) n o = m * (n + o) == m * n + m * o

prop_Nat_mul_plus_zero (m :: Nat) = m + 0 == m
prop_Nat_mul_mul_unit (m :: Nat)  = m * 1 == m

prop_Nat_minus n =
  forAll (greaterThanOrEqual n) $ \m ->
    (m - n) + n == m

prop_Nat_signum_abs (m :: Nat) = signum m * abs m == m
prop_Nat_signum_zero           = once ((signum 0 :: Nat) == 0)

prop_Nat_fromInteger_plus m n =
  m >= 0 && n >= 0 ==>
    fromInteger m + fromInteger n == (fromInteger (m + n) :: Nat)
prop_Nat_fromInteger_mul m n =
  m >= 0 && n >= 0 ==>
    fromInteger m * fromInteger n == (fromInteger (m * n) :: Nat)

-- negate is undefined.

-- Testing Integral.

prop_Nat_to_from (m :: Nat) = fromInteger (toInteger m) == m
prop_Nat_from_to i = i >= 0 ==> toInteger (fromInteger i :: Nat) == i

prop_Nat_quotRem (m :: Nat) n =
  n /= 0 ==> m `quotRem` n == (m `quot` n, m `rem` n)
prop_Nat_divMod (m :: Nat) n =
  n /= 0 ==> m `divMod` n == (m `div` n, m `mod` n)

prop_Nat_quot_rem (m :: Nat) n =
  n /= 0 ==> (m `quot` n) * n + m `rem` n == m
prop_Nat_div_mod (m :: Nat) n =
  n /= 0 ==> (m `div` n) * n + m `mod` n == m

-- Testing Real.

prop_Nat_toRational (m :: Nat) = toRational m == toInteger m % 1

-- Since the implementation is based on Integers I'd like to test that
-- we can't construct values of the form "Nat i" where i is a negative
-- Integer. (This can be seen as a test of the observation function
-- toInteger.)

prop_Nat_closed = [ unary (fromJust . fromSucc)
                    -- Ord
                  , binary max
                  , binary min
                    -- Enum
                    -- enumFrom and friends have default definitions.
                  , unary succ
                  , unary pred
                  , unary' toEnum
                    -- Num
                  , binary (+)
                  , binary (-)
                  , binary (*)
                  , unary negate
                  , unary abs
                  , unary signum
                  , unary' fromInteger
                    -- Integral
                  , binary quot
                  , binary rem
                  , binary div
                  , binary mod
                  , binary (fst .^^ quotRem)
                  , binary (snd .^^ quotRem)
                  , binary (fst .^^ divMod)
                  , binary (snd .^^ divMod)
                  ]
  where
  ok (n :: Nat) = (toInteger n >= 0) <=! True

  unary (f :: Nat -> Nat) = unary' f

  unary' f =
    forAll arbitrary $ \x ->
      ok (f x)

  binary f =
    forAll arbitrary $ \(m :: Nat) ->
    forAll arbitrary $ \(n :: Nat) ->
      ok (f m n)

  f .^^ g = \x y -> f (g x y)

-- | All tests collected together.

tests :: IO Bool
tests = runQuickCheckTests theTests
  where
  theTests = map run (concat testLists) ++ singleTests

  singleTests =
    [ run prop_isSucc
    , run prop_fromSucc
    , run prop_natrec_add
    , run prop_Nat_Enum_succ
    , run prop_Nat_Enum_pred
    , run prop_Nat_Enum_toEnum
    , run prop_Nat_Enum_fromEnum
    , run prop_Nat_Show
    , run prop_Nat_mul_iterated_sum
    , run prop_Nat_plus_assoc
    , run prop_Nat_plus_comm
    , run prop_Nat_mul_assoc
    , run prop_Nat_mul_comm
    , run prop_Nat_mul_plus_left_dist
    , run prop_Nat_mul_plus_zero
    , run prop_Nat_mul_mul_unit
    , run prop_Nat_minus
    , run prop_Nat_signum_abs
    , run prop_Nat_signum_zero
    , run prop_Nat_fromInteger_plus
    , run prop_Nat_fromInteger_mul
    , run prop_Nat_to_from
    , run prop_Nat_from_to
    , run prop_Nat_quotRem
    , run prop_Nat_divMod
    , run prop_Nat_quot_rem
    , run prop_Nat_div_mod
    , run prop_Nat_toRational
    ]

  testLists =
    [ prop_Nat_Eq_congruence
    , prop_Nat_Ord_total_order
    , prop_Nat_closed
    ]
