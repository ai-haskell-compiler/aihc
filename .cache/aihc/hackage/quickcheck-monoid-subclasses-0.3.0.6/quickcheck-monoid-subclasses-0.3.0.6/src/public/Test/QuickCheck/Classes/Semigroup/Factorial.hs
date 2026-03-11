{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Semigroup.Factorial".
--
module Test.QuickCheck.Classes.Semigroup.Factorial
    ( factorialLaws
    , stableFactorialLaws
    )
    where

import Data.Bool
    ( Bool (True) )
import Data.Eq
    ( Eq ((==)) )
import Data.Foldable
    ( all )
import Data.Function
    ( ($), (&) )
import Data.Functor
    ( Functor (fmap), (<$>) )
import Data.List.NonEmpty
    ( nonEmpty )
import Data.Maybe
    ( maybe )
import Data.Ord
    ( Ord ((>=)) )
import Data.Proxy
    ( Proxy )
import Data.Semigroup
    ( Semigroup (sconcat, (<>)) )
import Data.Semigroup.Factorial
    ( Factorial
    , StableFactorial
    , factors
    , foldl
    , foldl'
    , foldr
    , length
    , primePrefix
    , primeSuffix
    , reverse
    )
import Data.String
    ( String )
import Internal
    ( cover, makeLaw1, makeLaw2, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary, Gen, Property, elements, forAllBlind )
import Test.QuickCheck.Classes
    ( Laws (Laws) )
import Text.Show
    ( Show )

import qualified Data.List as L

--------------------------------------------------------------------------------
-- Factorial
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Factorial'.
--
-- Includes the following laws:
--
-- @
-- 'length' a '==' "Data.List".'Data.List.length' ('factors' a)
-- @
--
-- @
-- 'maybe' a 'sconcat' ('nonEmpty'   \ \ \     \ $ 'factors' a) '==' \       \ a
-- 'maybe' a 'sconcat' ('nonEmpty' $ 'L.reverse' $ 'factors' a) '==' 'reverse' a
-- @
--
-- @
-- 'all' (\\f -> 'factors' f '==' [f]) ('factors' a)
-- @
--
-- @
-- 'primePrefix' a '==' 'foldr' (\\x _ -> x) a a
-- 'primeSuffix' a '==' 'foldl' (\\_ x -> x) a a
-- @
--
-- @
-- 'foldl'  f x a '==' "Data.List".'Data.List.foldl'  f x ('factors' a)
-- 'foldl'' f x a '==' "Data.List".'Data.List.foldl'' f x ('factors' a)
-- 'foldr'  f x a '==' "Data.List".'Data.List.foldr'  f x ('factors' a)
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.semigroupLaws'
--
factorialLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Factorial a)
    => Proxy a
    -> Laws
factorialLaws _ = Laws "Factorial"
    [ makeLaw1 @a
        "factorialLaw_coverage"
        (factorialLaw_coverage)
    , makeLaw1 @a
        "factorialLaw_length_factors"
        (factorialLaw_length_factors)
    , makeLaw1 @a
        "factorialLaw_maybe_sconcat_nonEmpty_factors"
        (factorialLaw_maybe_sconcat_nonEmpty_factors)
    , makeLaw1 @a
        "factorialLaw_maybe_sconcat_nonEmpty_factors_reverse"
        (factorialLaw_maybe_sconcat_nonEmpty_factors_reverse)
    , makeLaw1 @a
        "factorialLaw_all_factors_prime"
        (factorialLaw_all_factors_prime)
    , makeLaw1 @a
        "factorialLaw_primePrefix_foldr"
        (factorialLaw_primePrefix_foldr)
    , makeLaw1 @a
        "factorialLaw_primeSuffix_foldl"
        (factorialLaw_primeSuffix_foldl)
    , makeLaw2 @a
        "factorialLaw_factors_foldl"
        (factorialLaw_factors_foldl)
    , makeLaw2 @a
        "factorialLaw_factors_foldl'"
        (factorialLaw_factors_foldl')
    , makeLaw2 @a
        "factorialLaw_factors_foldr"
        (factorialLaw_factors_foldr)
    ]

factorialLaw_coverage
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_coverage a =
    makeProperty
        "True"
        (True)
    & cover
        "length a == 0"
        (length a == 0)
    & cover
        "length a == 1"
        (length a == 1)
    & cover
        "length a >= 2"
        (length a >= 2)

factorialLaw_length_factors
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_length_factors a =
    makeProperty
        "length a == L.length (factors a)"
        (length a == L.length (factors a))
    & report
        "length a"
        (length a)
    & report
        "L.length (factors a)"
        (L.length (factors a))

factorialLaw_maybe_sconcat_nonEmpty_factors
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_maybe_sconcat_nonEmpty_factors a =
    makeProperty
        "maybe a sconcat (nonEmpty (factors a)) == a"
        (maybe a sconcat (nonEmpty (factors a)) == a)
    & report
        "factors a"
        (factors a)
    & report
        "nonEmpty (factors a)"
        (nonEmpty (factors a))
    & report
        "fmap sconcat (nonEmpty (factors a))"
        (fmap sconcat (nonEmpty (factors a)))

factorialLaw_maybe_sconcat_nonEmpty_factors_reverse
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_maybe_sconcat_nonEmpty_factors_reverse a =
    makeProperty
        "maybe a sconcat (nonEmpty (L.reverse (factors a))) == reverse a"
        (maybe a sconcat (nonEmpty (L.reverse (factors a))) == reverse a)
    & report
        "factors a"
        (factors a)
    & report
        "L.reverse (factors a)"
        (L.reverse (factors a))
    & report
        "nonEmpty (L.reverse (factors a))"
        (nonEmpty (L.reverse (factors a)))
    & report
        "maybe a sconcat (nonEmpty (L.reverse (factors a)))"
        (maybe a sconcat (nonEmpty (L.reverse (factors a))))
    & report
        "reverse a"
        (reverse a)

factorialLaw_all_factors_prime
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_all_factors_prime a =
    makeProperty
        "all (λf -> factors f == [f]) (factors a)"
        (all (\f -> factors f == [f]) (factors a))
    & report
        "factors a"
        (factors a)
    & report
        "factors <$> factors a"
        (factors <$> factors a)

factorialLaw_primePrefix_foldr
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_primePrefix_foldr a =
    makeProperty
        "primePrefix a == foldr (λx _ -> x) a a"
        (primePrefix a == foldr (\x _ -> x) a a)
    & report
        "factors a"
        (factors a)
    & report
        "primePrefix a"
        (primePrefix a)
    & report
        "foldr (λx _ -> x) a a"
        (foldr (\x _ -> x) a a)

factorialLaw_primeSuffix_foldl
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_primeSuffix_foldl a =
    makeProperty
        "primeSuffix a == foldl (λ_ x -> x) a a"
        (primeSuffix a == foldl (\_ x -> x) a a)
    & report
        "factors a"
        (factors a)
    & report
        "primeSuffix a"
        (primeSuffix a)
    & report
        "foldl (λ_ x -> x) a a"
        (foldl (\_ x -> x) a a)

factorialLaw_factors_foldl
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldl x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldl f x a == L.foldl f x (factors a)"
            (foldl f x a == L.foldl f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldl f x a"
            (foldl f x a)
        & report
            "L.foldl f x (factors a)"
            (L.foldl f x (factors a))

factorialLaw_factors_foldl'
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldl' x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldl' f x a == L.foldl' f x (factors a)"
            (foldl' f x a == L.foldl' f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldl' f x a"
            (foldl' f x a)
        & report
            "L.foldl' f x (factors a)"
            (L.foldl' f x (factors a))

factorialLaw_factors_foldr
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldr x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldr f x a == L.foldr f x (factors a)"
            (foldr f x a == L.foldr f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldr f x a"
            (foldr f x a)
        & report
            "L.foldr f x (factors a)"
            (L.foldr f x (factors a))

--------------------------------------------------------------------------------
-- StableFactorial
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'StableFactorial'.
--
-- Includes the following laws:
--
-- @
-- 'factors' (a '<>' b) '==' 'factors' a '<>' 'factors' b
-- @
--
-- @
-- 'factors' ('reverse' a) '==' "Data.List".'L.reverse' ('factors' a)
-- @
--
-- @
-- 'primePrefix' a '==' 'primeSuffix' ('reverse' a)
-- 'primeSuffix' a '==' 'primePrefix' ('reverse' a)
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'factorialLaws'
--
stableFactorialLaws
    :: forall a. (Arbitrary a, Show a, Eq a, StableFactorial a)
    => Proxy a
    -> Laws
stableFactorialLaws _ = Laws "StableFactorial"
    [ makeLaw2 @a
        "stableFactorialLaw_factors_mappend"
        (stableFactorialLaw_factors_mappend)
    , makeLaw1 @a
        "stableFactorialLaw_factors_reverse"
        (stableFactorialLaw_factors_reverse)
    , makeLaw1 @a
        "stableFactorialLaw_primePrefix_primeSuffix_reverse"
        (stableFactorialLaw_primePrefix_primeSuffix_reverse)
    , makeLaw1 @a
        "stableFactorialLaw_primeSuffix_primePrefix_reverse"
        (stableFactorialLaw_primeSuffix_primePrefix_reverse)
    ]

stableFactorialLaw_factors_mappend
    :: (Eq a, Show a, StableFactorial a) => a -> a -> Property
stableFactorialLaw_factors_mappend a b =
    makeProperty
        "factors (a <> b) == factors a <> factors b"
        (factors (a <> b) == factors a <> factors b)
    & report
        "a <> b"
        (a <> b)
    & report
        "factors a"
        (factors a)
    & report
        "factors b"
        (factors b)
    & report
        "factors (a <> b)"
        (factors (a <> b))
    & report
        "factors a <> factors b"
        (factors a <> factors b)

stableFactorialLaw_factors_reverse
    :: (Eq a, Show a, StableFactorial a) => a -> Property
stableFactorialLaw_factors_reverse a =
    makeProperty
        "factors (reverse a) == L.reverse (factors a)"
        (factors (reverse a) == L.reverse (factors a))
    & report
        "reverse a"
        (reverse a)
    & report
        "factors (reverse a)"
        (factors (reverse a))
    & report
        "factors a"
        (factors a)
    & report
        "L.reverse (factors a)"
        (L.reverse (factors a))

stableFactorialLaw_primePrefix_primeSuffix_reverse
    :: (Eq a, Show a, StableFactorial a) => a -> Property
stableFactorialLaw_primePrefix_primeSuffix_reverse a =
    makeProperty
        "primePrefix a == primeSuffix (reverse a)"
        (primePrefix a == primeSuffix (reverse a))
    & report
        "primePrefix a"
        (primePrefix a)
    & report
        "reverse a"
        (reverse a)
    & report
        "primeSuffix (reverse a)"
        (primeSuffix (reverse a))

stableFactorialLaw_primeSuffix_primePrefix_reverse
    :: (Eq a, Show a, StableFactorial a) => a -> Property
stableFactorialLaw_primeSuffix_primePrefix_reverse a =
    makeProperty
        "primeSuffix a == primePrefix (reverse a)"
        (primeSuffix a == primePrefix (reverse a))
    & report
        "primeSuffix a"
        (primeSuffix a)
    & report
        "reverse a"
        (reverse a)
    & report
        "primePrefix (reverse a)"
        (primePrefix (reverse a))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

genAccumulatorFn :: Semigroup a => Gen (String, a -> a -> a)
genAccumulatorFn = elements
    [ ( "λa _ -> a"
      , (\a _ -> a)
      )
    , ( "λ_ b -> b"
      , (\_ b -> b)
      )
    , ( "λa b -> a <> b"
      , (\a b -> a <> b)
      )
    , ( "λa b -> b <> a"
      , (\a b -> b <> a)
      )
    ]
