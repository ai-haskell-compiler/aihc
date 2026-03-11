{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Group".
--
module Test.QuickCheck.Classes.Group
    (
    -- * Group
      groupLaws

    -- * Abelian
    , abelianLaws
    )
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Group
    ( Abelian, Group (..) )
import Data.Proxy
    ( Proxy (..) )
import Internal
    ( cover, makeLaw0, makeLaw1, makeLaw2, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , NonPositive (..)
    , Property
    , forAllShrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- Group
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Group'.
--
-- Includes the following laws:
--
-- __/Inversion/__
--
-- @
-- 'invert' 'mempty' '==' 'mempty'
-- @
--
-- @
-- 'invert' ('invert' a) '==' a
-- @
--
-- @
-- \      \ a '<>' 'invert' a '==' 'mempty'
-- 'invert' a '<>' \      \ a '==' 'mempty'
-- @
--
-- __/Subtraction/__
--
-- @
-- a '~~' 'mempty' '==' a
-- @
--
-- @
-- a '~~' a '==' 'mempty'
-- @
--
-- @
-- a '~~' b '==' a '<>' 'invert' b
-- @
--
-- __/Exponentiation/__
--
-- @
-- 'pow' a 0 '==' 'mempty'
-- @
--
-- @
-- n '>=' 0 ==> 'pow' a n '==' \      \  'mconcat' ('replicate'  \   \ n  a)
-- n '<=' 0 ==> 'pow' a n '==' 'invert' ('mconcat' ('replicate' ('abs' n) a))
-- @
--
-- == Superclass laws
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
--
groupLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Group a)
    => Proxy a
    -> Laws
groupLaws _ = Laws "Group"
    [ makeLaw0 @a
        "groupLaw_invert_mempty"
        (groupLaw_invert_mempty)
    , makeLaw1 @a
        "groupLaw_invert_invert"
        (groupLaw_invert_invert)
    , makeLaw1 @a
        "groupLaw_invert_mappend_1"
        (groupLaw_invert_mappend_1)
    , makeLaw1 @a
        "groupLaw_invert_mappend_2"
        (groupLaw_invert_mappend_2)
    , makeLaw1 @a
        "groupLaw_subtract_mempty"
        (groupLaw_subtract_mempty)
    , makeLaw1 @a
        "groupLaw_subtract_self"
        (groupLaw_subtract_self)
    , makeLaw2 @a
        "groupLaw_subtract_other"
        (groupLaw_subtract_other)
    , makeLaw1 @a
        "groupLaw_pow_zero"
        (groupLaw_pow_zero)
    , makeLaw1 @a
        "groupLaw_pow_nonNegative"
        (groupLaw_pow_nonNegative)
    , makeLaw1 @a
        "groupLaw_pow_nonPositive"
        (groupLaw_pow_nonPositive)
    ]

groupLaw_invert_mempty
    :: forall a. (Eq a, Show a, Group a) => Proxy a -> Property
groupLaw_invert_mempty _ =
    makeProperty
        "invert (mempty @a) == (mempty @a)"
        (invert (mempty @a) == (mempty @a))
    & report
        "mempty @a"
        (mempty @a)
    & report
        "invert (mempty @a)"
        (invert (mempty @a))

groupLaw_invert_invert
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_invert_invert a =
    makeProperty
        "invert (invert a) == a"
        (invert (invert a) == a)
    & report
        "invert a"
        (invert a)
    & report
        "invert (invert a)"
        (invert (invert a))

groupLaw_invert_mappend_1
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_invert_mappend_1 a =
    makeProperty
        "a <> invert a == mempty"
        (a <> invert a == mempty)
    & report
        "mempty @a"
        (mempty @a)
    & report
        "invert a"
        (invert a)
    & report
        "a <> invert a"
        (a <> invert a)

groupLaw_invert_mappend_2
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_invert_mappend_2 a =
    makeProperty
        "invert a <> a == mempty"
        (invert a <> a == mempty)
    & report
        "mempty @a"
        (mempty @a)
    & report
        "invert a"
        (invert a)
    & report
        "invert a <> a"
        (invert a <> a)

groupLaw_subtract_mempty
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_subtract_mempty a =
    makeProperty
        "a ~~ mempty == a"
        (a ~~ mempty == a)
    & report
        "mempty @a"
        (mempty @a)
    & report
        "a ~~ mempty"
        (a ~~ mempty)

groupLaw_subtract_self
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_subtract_self a =
    makeProperty
        "a ~~ a == mempty"
        (a ~~ a == mempty @a)
    & report
        "mempty @a"
        (mempty @a)
    & report
        "a ~~ a"
        (a ~~ a)

groupLaw_subtract_other
    :: (Eq a, Show a, Group a) => a -> a -> Property
groupLaw_subtract_other a b =
    makeProperty
        "a ~~ b == a <> invert b"
        (a ~~ b == a <> invert b)
    & report
        "a ~~ b"
        (a ~~ b)
    & report
        "invert b"
        (invert b)
    & report
        "a <> invert b"
        (a <> invert b)

groupLaw_pow_zero
    :: forall a. (Eq a, Show a, Group a) => a -> Property
groupLaw_pow_zero a =
    makeProperty
        "pow a 0 == mempty"
        (pow a 0 == mempty)
    & report
        "pow a 0"
        (pow a 0)
    & report
        "mempty @a"
        (mempty @a)

groupLaw_pow_nonNegative
    :: (Eq a, Show a, Group a) => a -> Property
groupLaw_pow_nonNegative a =
    forAllShrink (arbitrary @(NonNegative Int)) shrink $ \(NonNegative n) ->
    makeProperty
        "pow a n == mconcat (replicate n a)"
        (pow a n == mconcat (replicate n a))
    & report
        "pow a n"
        (pow a n)
    & report
        "mconcat (replicate n a)"
        (mconcat (replicate n a))
    & cover
        "n == 0"
        (n == 0)
    & cover
        "n == 1"
        (n == 1)
    & cover
        "n == 2"
        (n == 2)
    & cover
        "n == 3"
        (n == 3)
    & cover
        "n >= 4"
        (n >= 4)

groupLaw_pow_nonPositive
    :: (Eq a, Show a, Group a) => a -> Property
groupLaw_pow_nonPositive a =
    forAllShrink (arbitrary @(NonPositive Int)) shrink $ \(NonPositive n) ->
    makeProperty
        "pow a n == invert (mconcat (replicate (abs n) a))"
        (pow a n == invert (mconcat (replicate (abs n) a)))
    & report
        "pow a n"
        (pow a n)
    & report
        "mconcat (replicate (abs n) a)"
        (mconcat (replicate (abs n) a))
    & report
        "invert (mconcat (replicate (abs n) a))"
        (invert (mconcat (replicate (abs n) a)))
    & cover
        "n == -0"
        (n == -0)
    & cover
        "n == -1"
        (n == -1)
    & cover
        "n == -2"
        (n == -2)
    & cover
        "n == -3"
        (n == -3)
    & cover
        "n <= -4"
        (n <= -4)

--------------------------------------------------------------------------------
-- Abelian
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Abelian'.
--
-- Includes the following law:
--
-- __/Commutativity/__
--
-- @
-- a '<>' b '==' b '<>' a
-- @
--
-- == Superclass laws
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Group.groupLaws'
--
abelianLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Abelian a)
    => Proxy a
    -> Laws
abelianLaws _ = Laws "Abelian"
    [ makeLaw2 @a
        "abelianLaw_commutative"
        (abelianLaw_commutative)
    ]

abelianLaw_commutative
    :: (Eq a, Show a, Abelian a) => a -> a -> Property
abelianLaw_commutative a b =
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
