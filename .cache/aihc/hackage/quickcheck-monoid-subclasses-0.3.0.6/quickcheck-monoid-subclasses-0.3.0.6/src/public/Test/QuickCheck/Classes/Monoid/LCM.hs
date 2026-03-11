{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use &&" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.LCM".
--
module Test.QuickCheck.Classes.Monoid.LCM
    ( lcmMonoidLaws
    , distributiveLCMMonoidLaws
    )
    where

import Prelude hiding
    ( gcd, lcm )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.GCD
    ( GCDMonoid (..) )
import Data.Monoid.LCM
    ( DistributiveLCMMonoid, LCMMonoid (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Reductive (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeLaw3, makeProperty, report, (==>) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- LCMMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LCMMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('lcm' a b '</>' a)
-- @
-- @
-- 'isJust' ('lcm' a b '</>' b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ \   \   c '</>' a
--     , \   \   c '</>' b
--     , 'lcm' a b '</>' c
--     ]
-- ==>
--     ('lcm' a b '==' c)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'lcm' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'lcm' 'mempty' a '==' a
-- @
-- @
-- 'lcm' a 'mempty' '==' a
-- @
--
-- __/Commutativity/__
--
-- @
-- 'lcm' a b '==' 'lcm' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'lcm' ('lcm' a b) c '==' 'lcm' a ('lcm' b c)
-- @
--
-- __/Absorption/__
--
-- @
-- 'lcm' a ('gcd' a b) '==' a
-- @
-- @
-- 'gcd' a ('lcm' a b) '==' a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Monoid.GCD.gcdMonoidLaws'
--
lcmMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LCMMonoid a)
    => Proxy a
    -> Laws
lcmMonoidLaws _ = Laws "LCMMonoid"
    [ makeLaw2 @a
        "lcmMonoidLaw_reductivity_left"
        (lcmMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "lcmMonoidLaw_reductivity_right"
        (lcmMonoidLaw_reductivity_right)
    , makeLaw3 @a
        "lcmMonoidLaw_uniqueness"
        (lcmMonoidLaw_uniqueness)
    , makeLaw1 @a
        "lcmMonoidLaw_idempotence"
        (lcmMonoidLaw_idempotence)
    , makeLaw1 @a
        "lcmMonoidLaw_identity_left"
        (lcmMonoidLaw_identity_left)
    , makeLaw1 @a
        "lcmMonoidLaw_identity_right"
        (lcmMonoidLaw_identity_right)
    , makeLaw2 @a
        "lcmMonoidLaw_commutativity"
        (lcmMonoidLaw_commutativity)
    , makeLaw3 @a
        "lcmMonoidLaw_associativity"
        (lcmMonoidLaw_associativity)
    , makeLaw2 @a
        "lcmMonoidLaw_absorption_gcd_lcm"
        (lcmMonoidLaw_absorption_gcd_lcm)
    , makeLaw2 @a
        "lcmMonoidLaw_absorption_lcm_gcd"
        (lcmMonoidLaw_absorption_lcm_gcd)
    ]

lcmMonoidLaw_reductivity_left
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> Property
lcmMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (lcm a b </> a)"
        (isJust (lcm a b </> a))
    & cover
        "lcm a b /= mempty"
        (lcm a b /= mempty)
    & cover
        "(lcm a b </> a) /= Just mempty"
        ((lcm a b </> a) /= Just mempty)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm a b </> a"
        (lcm a b </> a)

lcmMonoidLaw_reductivity_right
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> Property
lcmMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (lcm a b </> b)"
        (isJust (lcm a b </> b))
    & cover
        "lcm a b /= mempty"
        (lcm a b /= mempty)
    & cover
        "(lcm a b </> b) /= Just mempty"
        ((lcm a b </> b) /= Just mempty)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm a b </> b"
        (lcm a b </> b)

lcmMonoidLaw_uniqueness
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> a -> Property
lcmMonoidLaw_uniqueness a b c =
    makeProperty
        "all isJust [c </> a, c </> b, lcm a b </> c] ==> (lcm a b == c)"
        (all isJust [c </> a, c </> b, lcm a b </> c] ==> (lcm a b == c))
    & cover
        "all isJust [c </> a, c </> b, lcm a b </> c]"
        (all isJust [c </> a, c </> b, lcm a b </> c])
    & cover
        "not (all isJust [c </> a, c </> b, lcm a b </> c])"
        (not (all isJust [c </> a, c </> b, lcm a b </> c]))
    & cover
        "c == lcm a b"
        (c == lcm a b)
    & cover
        "c /= lcm a b"
        (c /= lcm a b)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm a b </> c"
        (lcm a b </> c)
    & report
        "c </> a"
        (c </> a)
    & report
        "c </> b"
        (c </> b)

lcmMonoidLaw_idempotence
    :: (Eq a, Show a, LCMMonoid a) => a -> Property
lcmMonoidLaw_idempotence a =
    makeProperty
        "lcm a a == a"
        (lcm a a == a)
    & report
        "lcm a a"
        (lcm a a)

lcmMonoidLaw_identity_left
    :: (Eq a, Show a, LCMMonoid a) => a -> Property
lcmMonoidLaw_identity_left a =
    makeProperty
        "lcm mempty a == a"
        (lcm mempty a == a)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "lcm mempty a"
        (lcm mempty a)

lcmMonoidLaw_identity_right
    :: (Eq a, Show a, LCMMonoid a) => a -> Property
lcmMonoidLaw_identity_right a =
    makeProperty
        "lcm a mempty == a"
        (lcm a mempty == a)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "lcm a mempty"
        (lcm a mempty)

lcmMonoidLaw_commutativity
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> Property
lcmMonoidLaw_commutativity a b =
    makeProperty
        "lcm a b == lcm b a"
        (lcm a b == lcm b a)
    & cover
        "lcm a b /= mempty"
        (lcm a b /= mempty)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm b a"
        (lcm b a)

lcmMonoidLaw_associativity
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> a -> Property
lcmMonoidLaw_associativity a b c =
    makeProperty
        "lcm (lcm a b) c == lcm a (lcm b c)"
        (lcm (lcm a b) c == lcm a (lcm b c))
    & cover
        "lcm a b /= mempty"
        (lcm a b /= mempty)
    & cover
        "lcm b c /= mempty"
        (lcm b c /= mempty)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm (lcm a b) c"
        (lcm (lcm a b) c)
    & report
        "lcm b c"
        (lcm b c)
    & report
        "lcm a (lcm b c)"
        (lcm a (lcm b c))

lcmMonoidLaw_absorption_gcd_lcm
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> Property
lcmMonoidLaw_absorption_gcd_lcm a b =
    makeProperty
        "lcm a (gcd a b) == a"
        (lcm a (gcd a b) == a)
    & cover
        "gcd a b == mempty"
        (gcd a b == mempty)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "lcm a (gcd a b)"
        (lcm a (gcd a b))

lcmMonoidLaw_absorption_lcm_gcd
    :: (Eq a, Show a, LCMMonoid a) => a -> a -> Property
lcmMonoidLaw_absorption_lcm_gcd a b =
    makeProperty
        "gcd a (lcm a b) == a"
        (gcd a (lcm a b) == a)
    & cover
        "gcd a b == mempty"
        (gcd a b == mempty)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & report
        "lcm a b"
        (lcm a b)
    & report
        "gcd a (lcm a b)"
        (gcd a (lcm a b))

--------------------------------------------------------------------------------
-- DistributiveLCMMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'DistributiveLCMMonoid'.
--
-- Includes the following laws:
--
-- __/Left-distributivity/__
--
-- @
-- 'lcm' (a '<>' b) (a '<>' c) '==' a '<>' 'lcm' b c
-- @
--
-- __/Right-distributivity/__
--
-- @
-- 'lcm' (a '<>' c) (b '<>' c) '==' 'lcm' a b '<>' c
-- @
--
-- __/Lattice distributivity/__
--
-- @
-- 'lcm' a ('gcd' b c) '==' 'gcd' ('lcm' a b) ('lcm' a c)
-- @
--
-- @
-- 'gcd' a ('lcm' b c) '==' 'lcm' ('gcd' a b) ('gcd' a c)
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Monoid.GCD.distributiveGCDMonoidLaws'
-- * 'lcmMonoidLaws'
--
distributiveLCMMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, DistributiveLCMMonoid a)
    => Proxy a
    -> Laws
distributiveLCMMonoidLaws _ = Laws "DistributiveLCMMonoid"
    [ makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_left"
        (distributiveLCMMonoidLaw_distributivity_left)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_right"
        (distributiveLCMMonoidLaw_distributivity_right)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_gcd_lcm"
        (distributiveLCMMonoidLaw_distributivity_gcd_lcm)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_lcm_gcd"
        (distributiveLCMMonoidLaw_distributivity_lcm_gcd)
    ]

distributiveLCMMonoidLaw_distributivity_left
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_left a b c =
    makeProperty
        "lcm (a <> b) (a <> c) == a <> lcm b c"
        (lcm (a <> b) (a <> c) == a <> lcm b c)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "lcm (a <> b) (a <> c)"
        (lcm (a <> b) (a <> c))
    & report
        "lcm b c"
        (lcm b c)
    & report
        "a <> lcm b c"
        (a <> lcm b c)

distributiveLCMMonoidLaw_distributivity_right
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_right a b c =
    makeProperty
        "lcm (a <> c) (b <> c) == lcm a b <> c"
        (lcm (a <> c) (b <> c) == lcm a b <> c)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "lcm (a <> c) (b <> c)"
        (lcm (a <> c) (b <> c))
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm a b <> c"
        (lcm a b <> c)

distributiveLCMMonoidLaw_distributivity_gcd_lcm
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_gcd_lcm a b c =
    makeProperty
        "lcm a (gcd b c) == gcd (lcm a b) (lcm a c)"
        (lcm a (gcd b c) == gcd (lcm a b) (lcm a c))

distributiveLCMMonoidLaw_distributivity_lcm_gcd
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_lcm_gcd a b c =
    makeProperty
        "gcd a (lcm b c) == lcm (gcd a b) (gcd a c)"
        (gcd a (lcm b c) == lcm (gcd a b) (gcd a c))
