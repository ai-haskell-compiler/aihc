{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use infix" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Semigroup.Cancellative".
--
module Test.QuickCheck.Classes.Semigroup.Cancellative
    (
    -- * Commutative
      commutativeLaws

    -- * Reductive
    , reductiveLaws
    , leftReductiveLaws
    , rightReductiveLaws

    -- * Cancellative
    , cancellativeLaws
    , leftCancellativeLaws
    , rightCancellativeLaws
    )
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive (..)
    , Reductive (..)
    , RightCancellative
    , RightReductive (..)
    )
import Internal
    ( cover, makeLaw2, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- Cancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Cancellative'.
--
-- Includes the following laws:
--
-- @
-- (a '<>' b) '</>' a '==' 'Just' b
-- @
--
-- @
-- (a '<>' b) '</>' b '==' 'Just' a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftCancellativeLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightCancellativeLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.reductiveLaws'
--
cancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Cancellative a)
    => Proxy a
    -> Laws
cancellativeLaws _ = Laws "Cancellative"
    [ makeLaw2 @a
        "cancellativeLaw_cancellation_prefix"
        (cancellativeLaw_cancellation_prefix)
    , makeLaw2 @a
        "cancellativeLaw_cancellation_suffix"
        (cancellativeLaw_cancellation_suffix)
    ]

cancellativeLaw_cancellation_prefix
    :: (Eq a, Show a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_prefix a b =
    makeProperty
        "(a <> b) </> a == Just b"
        ((a <> b) </> a == Just b)
    & report
        "a <> b"
        (a <> b)
    & report
        "(a <> b) </> a"
        ((a <> b) </> a)

cancellativeLaw_cancellation_suffix
    :: (Eq a, Show a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b =
    makeProperty
        "(a <> b) </> b == Just a"
        ((a <> b) </> b == Just a)
    & report
        "a <> b"
        (a <> b)
    & report
        "(a <> b) </> b"
        ((a <> b) </> b)

--------------------------------------------------------------------------------
-- Commutative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Commutative'.
--
-- Includes the following law:
--
-- @
-- a '<>' b '==' b '<>' a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.semigroupLaws'
--
commutativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Commutative a)
    => Proxy a
    -> Laws
commutativeLaws _ = Laws "Commutative"
    [ makeLaw2 @a
        "commutativeLaw_basic"
        (commutativeLaw_basic)
    ]

commutativeLaw_basic
    :: (Eq a, Show a, Commutative a) => a -> a -> Property
commutativeLaw_basic a b =
    makeProperty
        "a <> b == b <> a"
        (a <> b == b <> a)
    & report
        "a <> b"
        (a <> b)
    & report
        "b <> a"
        (b <> a)
    & cover
        "(a /= b) && (a <> b /= a) && (b <> a /= b)"
        ((a /= b) && (a <> b /= a) && (b <> a /= b))

--------------------------------------------------------------------------------
-- LeftCancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftCancellative'.
--
-- Includes the following law:
--
-- @
-- 'stripPrefix' a (a '<>' b) '==' 'Just' b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
--
leftCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftCancellative a)
    => Proxy a
    -> Laws
leftCancellativeLaws _ = Laws "LeftCancellative"
    [ makeLaw2 @a
        "leftCancellativeLaw_cancellation"
        (leftCancellativeLaw_cancellation)
    ]

leftCancellativeLaw_cancellation
    :: (Eq a, Show a, LeftCancellative a) => a -> a -> Property
leftCancellativeLaw_cancellation a b =
    makeProperty
        "stripPrefix a (a <> b) == Just b"
        (stripPrefix a (a <> b) == Just b)
    & report
        "a <> b"
        (a <> b)
    & report
        "stripPrefix a (a <> b)"
        (stripPrefix a (a <> b))

--------------------------------------------------------------------------------
-- LeftReductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftReductive'.
--
-- Includes the following laws:
--
-- @
-- a '`isPrefixOf`' (a '<>' b)
-- @
--
-- @
-- 'isPrefixOf' a b '==' 'isJust' ('stripPrefix' a b)
-- @
--
-- @
-- 'maybe' b (a '<>') ('stripPrefix' a b) '==' b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.semigroupLaws'
--
leftReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftReductive a)
    => Proxy a
    -> Laws
leftReductiveLaws _ = Laws "LeftReductive"
    [ makeLaw2 @a
        "leftReductiveLaw_isPrefixOf_mappend"
        (leftReductiveLaw_isPrefixOf_mappend)
    , makeLaw2 @a
        "leftReductiveLaw_isPrefixOf_stripPrefix"
        (leftReductiveLaw_isPrefixOf_stripPrefix)
    , makeLaw2 @a
        "leftReductiveLaw_stripPrefix"
        (leftReductiveLaw_stripPrefix)
    ]

leftReductiveLaw_isPrefixOf_mappend
    :: (Eq a, Show a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefixOf_mappend a b =
    makeProperty
        "a `isPrefixOf` (a <> b)"
        (a `isPrefixOf` (a <> b))
    & report
        "a <> b"
        (a <> b)

leftReductiveLaw_isPrefixOf_stripPrefix
    :: (Eq a, Show a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefixOf_stripPrefix a b =
    makeProperty
        "isPrefixOf a b == isJust (stripPrefix a b)"
        (isPrefixOf a b == isJust (stripPrefix a b))
    & report
        "isPrefixOf a b"
        (isPrefixOf a b)
    & report
        "stripPrefix a b"
        (stripPrefix a b)

leftReductiveLaw_stripPrefix
    :: (Eq a, Show a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b =
    makeProperty
        "maybe b (a <>) (stripPrefix a b) == b"
        (maybe b (a <>) (stripPrefix a b) == b)
    & report
        "stripPrefix a b"
        (stripPrefix a b)
    & report
        "maybe b (a <>) (stripPrefix a b)"
        (maybe b (a <>) (stripPrefix a b))

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Reductive'.
--
-- Includes the following laws:
--
-- @
-- a '</>' b '==' 'stripPrefix' b a
-- @
--
-- @
-- a '</>' b '==' 'stripSuffix' b a
-- @
--
-- @
-- 'maybe' a (b '<>') (a '</>' b) '==' a
-- @
--
-- @
-- 'maybe' a ('<>' b) (a '</>' b) '==' a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.commutativeLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw2 @a
        "reductiveLaw_equivalence_prefix"
        (reductiveLaw_equivalence_prefix)
    , makeLaw2 @a
        "reductiveLaw_equivalence_suffix"
        (reductiveLaw_equivalence_suffix)
    , makeLaw2 @a
        "reductiveLaw_inversion_prefix"
        (reductiveLaw_inversion_prefix)
    , makeLaw2 @a
        "reductiveLaw_inversion_suffix"
        (reductiveLaw_inversion_suffix)
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Show a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b =
    makeProperty
        "a </> b == stripPrefix b a"
        (a </> b == stripPrefix b a)
    & report
        "a </> b"
        (a </> b)
    & report
        "stripPrefix b a"
        (stripPrefix b a)

reductiveLaw_equivalence_suffix
    :: (Eq a, Show a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b =
    makeProperty
        "a </> b == stripSuffix b a"
        (a </> b == stripSuffix b a)
    & report
        "a </> b"
        (a </> b)
    & report
        "stripSuffix b a"
        (stripSuffix b a)

reductiveLaw_inversion_prefix
    :: (Eq a, Show a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b =
    makeProperty
        "maybe a (b <>) (a </> b) == a"
        (maybe a (b <>) (a </> b) == a)
    & report
        "a </> b"
        (a </> b)
    & report
        "maybe a (b <>) (a </> b)"
        (maybe a (b <>) (a </> b))

reductiveLaw_inversion_suffix
    :: (Eq a, Show a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b =
    makeProperty
        "maybe a (<> b) (a </> b) == a"
        (maybe a (<> b) (a </> b) == a)
    & report
        "a </> b"
        (a </> b)
    & report
        "maybe a (<> b) (a </> b)"
        (maybe a (<> b) (a </> b))

--------------------------------------------------------------------------------
-- RightCancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightCancellative'.
--
-- Includes the following law:
--
-- @
-- 'stripSuffix' b (a '<>' b) '==' 'Just' a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
rightCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightCancellative a)
    => Proxy a
    -> Laws
rightCancellativeLaws _ = Laws "RightCancellative"
    [ makeLaw2 @a
        "rightCancellativeLaw_cancellation"
        (rightCancellativeLaw_cancellation)
    ]

rightCancellativeLaw_cancellation
    :: (Eq a, Show a, RightCancellative a) => a -> a -> Property
rightCancellativeLaw_cancellation a b =
    makeProperty
        "stripSuffix b (a <> b) == Just a"
        (stripSuffix b (a <> b) == Just a)
    & report
        "a <> b"
        (a <> b)
    & report
        "stripSuffix b (a <> b)"
        (stripSuffix b (a <> b))

--------------------------------------------------------------------------------
-- RightReductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightReductive'.
--
-- Includes the following laws:
--
-- @
-- b '`isSuffixOf`' (a '<>' b)
-- @
--
-- @
-- 'isSuffixOf' a b '==' 'isJust' ('stripSuffix' a b)
-- @
--
-- @
-- 'maybe' b ('<>' a) ('stripSuffix' a b) '==' b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.semigroupLaws'
--
rightReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightReductive a)
    => Proxy a
    -> Laws
rightReductiveLaws _ = Laws "RightReductive"
    [ makeLaw2 @a
        "rightReductiveLaw_isSuffixOf_mappend"
        (rightReductiveLaw_isSuffixOf_mappend)
    , makeLaw2 @a
        "rightReductiveLaw_isSuffixOf_stripSuffix"
        (rightReductiveLaw_isSuffixOf_stripSuffix)
    , makeLaw2 @a
        "rightReductiveLaw_stripSuffix"
        (rightReductiveLaw_stripSuffix)
    ]

rightReductiveLaw_isSuffixOf_mappend
    :: (Eq a, Show a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffixOf_mappend a b =
    makeProperty
        "b `isSuffixOf` (a <> b)"
        (b `isSuffixOf` (a <> b))
    & report
        "a <> b"
        (a <> b)

rightReductiveLaw_isSuffixOf_stripSuffix
    :: (Eq a, Show a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffixOf_stripSuffix a b =
    makeProperty
        "isSuffixOf a b == isJust (stripSuffix a b)"
        (isSuffixOf a b == isJust (stripSuffix a b))
    & report
        "isSuffixOf a b"
        (isSuffixOf a b)
    & report
        "stripSuffix a b"
        (stripSuffix a b)

rightReductiveLaw_stripSuffix
    :: (Eq a, Show a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b =
    makeProperty
        "maybe b (<> a) (stripSuffix a b) == b"
        (maybe b (<> a) (stripSuffix a b) == b)
    & report
        "stripSuffix a b"
        (stripSuffix a b)
    & report
        "maybe b (<> a) (stripSuffix a b)"
        (maybe b (<> a) (stripSuffix a b))
