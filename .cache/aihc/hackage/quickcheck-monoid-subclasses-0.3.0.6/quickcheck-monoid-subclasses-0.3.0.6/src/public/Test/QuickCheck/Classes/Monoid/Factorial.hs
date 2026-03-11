{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Hoist not" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use const" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.Factorial".
--
module Test.QuickCheck.Classes.Monoid.Factorial
    ( factorialMonoidLaws
    )
    where

import Prelude hiding
    ( dropWhile, foldl, length, null, reverse, span, splitAt, takeWhile )

import Data.Bifunctor
    ( Bifunctor (bimap) )
import Data.Function
    ( (&) )
import Data.List
    ( unfoldr )
import Data.Monoid.Factorial
    ( FactorialMonoid
    , inits
    , span
    , split
    , splitAt
    , splitPrimePrefix
    , splitPrimeSuffix
    , tails
    )
import Data.Monoid.Null
    ( null )
import Data.Proxy
    ( Proxy )
import Data.Semigroup.Factorial
    ( factors, length, primePrefix, primeSuffix, reverse )
import Data.Tuple
    ( swap )
import Internal
    ( cover, makeLaw1, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    , Fun
    , Property
    , Testable
    , applyFun
    , chooseInt
    , elements
    , forAll
    , forAllShrink
    )
import Test.QuickCheck.Classes
    ( Laws (Laws) )

import qualified Data.List as L

--------------------------------------------------------------------------------
-- FactorialMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'FactorialMonoid'.
--
-- Includes the following laws:
--
-- @
-- 'null' a '==' 'null' ('factors' a)
-- @
--
-- @
-- 'factors' a '==' \ \ \     \ ('unfoldr'  \    \ \    \   'splitPrimePrefix'  a)
-- 'factors' a '==' 'L.reverse' ('unfoldr' ('fmap' 'swap' . 'splitPrimeSuffix') a)
-- @
--
-- @
-- 'reverse' a '==' 'mconcat' ('L.reverse' ('factors' a))
-- @
--
-- @
-- 'primePrefix' a '==' 'maybe' 'mempty' 'fst' ('splitPrimePrefix' a)
-- 'primeSuffix' a '==' 'maybe' 'mempty' 'snd' ('splitPrimeSuffix' a)
-- @
--
-- @
-- 'inits' a '==' 'fmap' 'mconcat' ('L.inits' ('factors' a))
-- 'tails' a '==' 'fmap' 'mconcat' ('L.tails' ('factors' a))
-- @
--
-- @
-- 'span' p a '==' 'bimap' 'mconcat' 'mconcat' ('L.span' p ('factors' a))
-- @
--
-- @
-- 'L.all' ('L.all' ('not' . p) . 'factors') ('split' p a)
-- @
--
-- @
-- 'mconcat' ('L.intersperse' p ('split' ('==' p) a)) '==' a
-- @
--
-- @
-- 'splitAt' i a '==' 'bimap' 'mconcat' 'mconcat' ('L.splitAt' i ('factors' a))
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Factorial.factorialLaws'
-- * 'Test.QuickCheck.Classes.Monoid.Null.monoidNullLaws'
--
factorialMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a)
    => Proxy a
    -> Laws
factorialMonoidLaws _ = Laws "FactorialMonoid"
    [ makeLaw1 @a
        "factorialMonoidLaw_coverage"
        (factorialMonoidLaw_coverage)
    , makeLaw1 @a
        "factorialMonoidLaw_null"
        (factorialMonoidLaw_null)
    , makeLaw1 @a
        "factorialMonoidLaw_splitPrimePrefix"
        (factorialMonoidLaw_splitPrimePrefix)
    , makeLaw1 @a
        "factorialMonoidLaw_splitPrimeSuffix"
        (factorialMonoidLaw_splitPrimeSuffix)
    , makeLaw1 @a
        "factorialMonoidLaw_reverse"
        (factorialMonoidLaw_reverse)
    , makeLaw1 @a
        "factorialMonoidLaw_primePrefix"
        (factorialMonoidLaw_primePrefix)
    , makeLaw1 @a
        "factorialMonoidLaw_primeSuffix"
        (factorialMonoidLaw_primeSuffix)
    , makeLaw1 @a
        "factorialMonoidLaw_inits"
        (factorialMonoidLaw_inits)
    , makeLaw1 @a
        "factorialMonoidLaw_tails"
        (factorialMonoidLaw_tails)
    , makeLaw1 @a
        "factorialMonoidLaw_span"
        (factorialMonoidLaw_span)
    , makeLaw1 @a
        "factorialMonoidLaw_split"
        (factorialMonoidLaw_split)
    , makeLaw1 @a
        "factorialMonoidLaw_split_intersperse"
        (factorialMonoidLaw_split_intersperse)
    , makeLaw1 @a
        "factorialMonoidLaw_splitAt"
        (factorialMonoidLaw_splitAt)
    ]

factorialMonoidLaw_coverage
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_coverage a =
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

factorialMonoidLaw_null
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_null a =
    makeProperty
        "null a == null (factors a)"
        (null a == null (factors a))
    & report
        "null a"
        (null a)
    & report
        "factors a"
        (factors a)
    & report
        "null (factors a)"
        (null (factors a))

factorialMonoidLaw_splitPrimePrefix
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_splitPrimePrefix a =
    makeProperty
        "factors a == unfoldr splitPrimePrefix a"
        (factors a == unfoldr splitPrimePrefix a)
    & report
        "factors a"
        (factors a)
    & report
        "unfoldr splitPrimePrefix a"
        (unfoldr splitPrimePrefix a)

factorialMonoidLaw_splitPrimeSuffix
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_splitPrimeSuffix a =
    makeProperty
        "factors a == L.reverse (unfoldr (fmap swap . splitPrimeSuffix) a)"
        (factors a == L.reverse (unfoldr (fmap swap . splitPrimeSuffix) a))
    & report
        "factors a"
        (factors a)
    & report
        "unfoldr (fmap swap . splitPrimeSuffix) a"
        (unfoldr (fmap swap . splitPrimeSuffix) a)
    & report
        "L.reverse (unfoldr (fmap swap . splitPrimeSuffix) a)"
        (L.reverse (unfoldr (fmap swap . splitPrimeSuffix) a))

factorialMonoidLaw_reverse
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_reverse a =
    makeProperty
        "reverse a == mconcat (L.reverse (factors a))"
        (reverse a == mconcat (L.reverse (factors a)))
    & report
        "reverse a"
        (reverse a)
    & report
        "factors a"
        (factors a)
    & report
        "L.reverse (factors a)"
        (L.reverse (factors a))
    & report
        "mconcat (L.reverse (factors a))"
        (mconcat (L.reverse (factors a)))

factorialMonoidLaw_primePrefix
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_primePrefix a =
    makeProperty
        "primePrefix a == maybe mempty fst (splitPrimePrefix a)"
        (primePrefix a == maybe mempty fst (splitPrimePrefix a))
    & report
        "primePrefix a"
        (primePrefix a)
    & report
        "splitPrimePrefix a"
        (splitPrimePrefix a)
    & report
        "maybe mempty fst (splitPrimePrefix a)"
        (maybe mempty fst (splitPrimePrefix a))

factorialMonoidLaw_primeSuffix
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_primeSuffix a =
    makeProperty
        "primeSuffix a == maybe mempty snd (splitPrimeSuffix a)"
        (primeSuffix a == maybe mempty snd (splitPrimeSuffix a))
    & report
        "primeSuffix a"
        (primeSuffix a)
    & report
        "splitPrimeSuffix a"
        (splitPrimeSuffix a)
    & report
        "maybe mempty snd (splitPrimeSuffix a)"
        (maybe mempty snd (splitPrimeSuffix a))

factorialMonoidLaw_inits
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_inits a =
    makeProperty
        "inits a == fmap mconcat (L.inits (factors a))"
        (inits a == fmap mconcat (L.inits (factors a)))
    & report
        "inits a"
        (inits a)
    & report
        "factors a"
        (factors a)
    & report
        "L.inits (factors a)"
        (L.inits (factors a))
    & report
        "fmap mconcat (L.inits (factors a))"
        (fmap mconcat (L.inits (factors a)))

factorialMonoidLaw_tails
    :: (Eq a, Show a, FactorialMonoid a) => a -> Property
factorialMonoidLaw_tails a =
    makeProperty
        "tails a == fmap mconcat (L.tails (factors a))"
        (tails a == fmap mconcat (L.tails (factors a)))
    & report
        "tails a"
        (tails a)
    & report
        "factors a"
        (factors a)
    & report
        "L.tails (factors a)"
        (L.tails (factors a))
    & report
        "fmap mconcat (L.tails (factors a))"
        (fmap mconcat (L.tails (factors a)))

factorialMonoidLaw_span
    :: forall a. (Eq a, Show a, FactorialMonoid a)
    => a
    -> Property
factorialMonoidLaw_span a = withShowableFn $ \p ->
    makeProperty
        "span p a == bimap mconcat mconcat (L.span p (factors a))"
        (span p a == bimap mconcat mconcat (L.span p (factors a)))
    & cover
        "any p (factors a)"
        (any p (factors a))
    & cover
        "any (not . p) (factors a)"
        (any (not . p) (factors a))
    & report
        "span p a"
        (span p a)
    & report
        "factors a"
        (factors a)
    & report
        "L.span p (factors a)"
        (L.span p (factors a))
    & report
        "bimap mconcat mconcat (L.span p (factors a))"
        (bimap mconcat mconcat (L.span p (factors a)))

factorialMonoidLaw_split
    :: forall a. (Eq a, Show a, FactorialMonoid a)
    => a
    -> Property
factorialMonoidLaw_split a = withShowableFn $ \p ->
    makeProperty
        "L.all (L.all (not . p) . factors) (split p a)"
        (L.all (L.all (not . p) . factors) (split p a))
    & cover
        "any p (factors a)"
        (any p (factors a))
    & cover
        "any (not . p) (factors a)"
        (any (not . p) (factors a))
    & report
        "split p a"
        (split p a)

factorialMonoidLaw_split_intersperse
    :: forall a. (Eq a, Show a, FactorialMonoid a)
    => a
    -> Property
factorialMonoidLaw_split_intersperse a =
    forAll (elements (factors a)) $ \p ->
        makeProperty
            "mconcat (L.intersperse p (split (== p) a)) == a"
            (mconcat (L.intersperse p (split (== p) a)) == a)
        & report
            "split (== p) a"
            (split (== p) a)
        & report
            "L.intersperse p (split (== p) a)"
            (L.intersperse p (split (== p) a))
        & report
            "mconcat (L.intersperse p (split (== p) a))"
            (mconcat (L.intersperse p (split (== p) a)))

factorialMonoidLaw_splitAt
    :: forall a. (Eq a, Show a, FactorialMonoid a)
    => a
    -> Property
factorialMonoidLaw_splitAt a =
    forAll (chooseInt (0, length a)) $ \i ->
        makeProperty
            "splitAt i a == bimap mconcat mconcat (L.splitAt i (factors a))"
            (splitAt i a == bimap mconcat mconcat (L.splitAt i (factors a)))
        & report
            "splitAt i a"
            (splitAt i a)
        & report
            "factors a"
            (factors a)
        & report
            "L.splitAt i (factors a)"
            (L.splitAt i (factors a))
        & report
            "bimap mconcat mconcat (L.splitAt i (factors a))"
            (bimap mconcat mconcat (L.splitAt i (factors a)))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

withShowableFn
    :: forall a b t. (Show a, Show b, Arbitrary b, Testable t)
    => ((a -> b) -> t)
    -> Property
withShowableFn t =
    forAllShrink (arbitrary @(Fun String b)) shrink $
        \f -> t ((applyFun f) . show)
