{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.Null".
--
module Test.QuickCheck.Classes.Monoid.Null
    (
    -- * Null
      monoidNullLaws

    -- * Positive
    , positiveMonoidLaws
    )
    where

import Prelude hiding
    ( null )

import Data.Function
    ( (&) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Proxy
    ( Proxy (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- MonoidNull
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'MonoidNull'.
--
-- Includes the following law:
--
-- @
-- 'null' a '==' (a '==' 'mempty')
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
--
monoidNullLaws
    :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a)
    => Proxy a
    -> Laws
monoidNullLaws _ = Laws "MonoidNull"
    [ makeLaw1 @a
        "monoidNullLaw_basic"
        (monoidNullLaw_basic)
    ]

monoidNullLaw_basic
    :: (Eq a, MonoidNull a) => a -> Property
monoidNullLaw_basic a =
    makeProperty
        "null a == (a == mempty)"
        (null a == (a == mempty))
    & cover
        "a == mempty"
        (a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "null a"
        (null a)
    & report
        "a == mempty"
        (a == mempty)

--------------------------------------------------------------------------------
-- PositiveMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'PositiveMonoid'.
--
-- Includes the following law:
--
-- @
-- 'null' (a '<>' b) '==' ('null' a '&&' 'null' b)
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Monoid.Null.monoidNullLaws'
--
positiveMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, PositiveMonoid a)
    => Proxy a
    -> Laws
positiveMonoidLaws _ = Laws "PositiveMonoid"
    [ makeLaw2 @a
        "positiveMonoidLaw_fundamental"
        (positiveMonoidLaw_fundamental)
    ]

positiveMonoidLaw_fundamental
    :: (Eq a, PositiveMonoid a, Show a) => a -> a -> Property
positiveMonoidLaw_fundamental a b =
    makeProperty
        "null (a <> b) == (null a && null b)"
        (null (a <> b) == (null a && null b))
    & cover
        "null (a <> b)"
        (null (a <> b))
    & cover
        "not (null (a <> b))"
        (not (null (a <> b)))
    & report
        "a <> b"
        (a <> b)
    & report
        "null (a <> b)"
        (null (a <> b))
    & report
        "null a"
        (null a)
    & report
        "null b"
        (null b)
