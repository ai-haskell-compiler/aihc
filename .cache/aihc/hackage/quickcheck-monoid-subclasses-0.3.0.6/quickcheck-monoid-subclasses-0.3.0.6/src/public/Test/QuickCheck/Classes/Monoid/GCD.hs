{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Redundant ==" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.GCD".
--
module Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , leftGCDMonoidLaws
    , rightGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , distributiveGCDMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
    )
    where

import Prelude hiding
    ( gcd )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.GCD
    ( DistributiveGCDMonoid
    , GCDMonoid (..)
    , LeftDistributiveGCDMonoid
    , LeftGCDMonoid (..)
    , OverlappingGCDMonoid (..)
    , RightDistributiveGCDMonoid
    , RightGCDMonoid (..)
    )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( LeftReductive (..), Reductive (..), RightReductive (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeLaw3, makeProperty, report, (==>) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- GCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'GCDMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' (a '</>' 'gcd' a b)
-- @
-- @
-- 'isJust' (b '</>' 'gcd' a b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ a '</>' c
--     , b '</>' c
--     , c '</>' 'gcd' a b
--     ]
-- ==>
--     (c '==' 'gcd' a b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'gcd' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'gcd' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'gcd' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'gcd' a b '==' 'gcd' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'gcd' ('gcd' a b) c '==' 'gcd' a ('gcd' b c)
-- @
--
-- __/Equivalences/__
--
-- @
-- 'gcd' a b '==' 'commonPrefix' a b
-- @
-- @
-- 'gcd' a b '==' 'commonSuffix' a b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.commutativeLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.reductiveLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.leftGCDMonoidLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.rightGCDMonoidLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.overlappingGCDMonoidLaws'
--
gcdMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a)
    => Proxy a
    -> Laws
gcdMonoidLaws _ = Laws "GCDMonoid"
    [ makeLaw2 @a
        "gcdMonoidLaw_reductivity_left"
        (gcdMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "gcdMonoidLaw_reductivity_right"
        (gcdMonoidLaw_reductivity_right)
    , makeLaw3 @a
        "gcdMonoidLaw_uniqueness"
        (gcdMonoidLaw_uniqueness)
    , makeLaw1 @a
        "gcdMonoidLaw_idempotence"
        (gcdMonoidLaw_idempotence)
    , makeLaw1 @a
        "gcdMonoidLaw_identity_left"
        (gcdMonoidLaw_identity_left)
    , makeLaw1 @a
        "gcdMonoidLaw_identity_right"
        (gcdMonoidLaw_identity_right)
    , makeLaw2 @a
        "gcdMonoidLaw_commutativity"
        (gcdMonoidLaw_commutativity)
    , makeLaw3 @a
        "gcdMonoidLaw_associativity"
        (gcdMonoidLaw_associativity)
    , makeLaw2 @a
        "gcdMonoidLaw_equivalence_commonPrefix"
        (gcdMonoidLaw_equivalence_commonPrefix)
    , makeLaw2 @a
        "gcdMonoidLaw_equivalence_commonSuffix"
        (gcdMonoidLaw_equivalence_commonSuffix)
    ]

gcdMonoidLaw_reductivity_left
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (a </> gcd a b)"
        (isJust (a </> gcd a b))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "(a </> gcd a b) /= mempty"
        ((a </> gcd a b) /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "a </> gcd a b"
        (a </> gcd a b)

gcdMonoidLaw_reductivity_right
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (b </> gcd a b)"
        (isJust (b </> gcd a b))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "(b </> gcd a b) /= mempty"
        ((b </> gcd a b) /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "b </> gcd a b"
        (b </> gcd a b)

gcdMonoidLaw_uniqueness
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_uniqueness a b c =
    makeProperty
        "all isJust [a </> c, b </> c, c </> gcd a b] ==> (c == gcd a b)"
        (all isJust [a </> c, b </> c, c </> gcd a b] ==> (c == gcd a b))
    & cover
        "all isJust [a </> c, b </> c, c </> gcd a b]"
        (all isJust [a </> c, b </> c, c </> gcd a b])
    & cover
        "not (all isJust [a </> c, b </> c, c </> gcd a b])"
        (not (all isJust [a </> c, b </> c, c </> gcd a b]))
    & cover
        "c == gcd a b"
        (c == gcd a b)
    & cover
        "c /= gcd a b"
        (c /= gcd a b)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "c </> gcd a b"
        (c </> gcd a b)
    & report
        "a </> c"
        (a </> c)
    & report
        "b </> c"
        (b </> c)

gcdMonoidLaw_idempotence
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_idempotence a =
    makeProperty
        "gcd a a == a"
        (gcd a a == a)
    & report
        "gcd a a"
        (gcd a a)

gcdMonoidLaw_identity_left
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_identity_left a =
    makeProperty
        "gcd mempty a == mempty"
        (gcd mempty a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "gcd mempty a"
        (gcd mempty a)

gcdMonoidLaw_identity_right
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_identity_right a =
    makeProperty
        "gcd a mempty == mempty"
        (gcd a mempty == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "gcd a mempty"
        (gcd a mempty)

gcdMonoidLaw_commutativity
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_commutativity a b =
    makeProperty
        "gcd a b == gcd b a"
        (gcd a b == gcd b a)
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
        "gcd b a"
        (gcd b a)

gcdMonoidLaw_associativity
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_associativity a b c =
    makeProperty
        "gcd (gcd a b) c == gcd a (gcd b c)"
        (gcd (gcd a b) c == gcd a (gcd b c))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "gcd b c /= mempty"
        (gcd b c /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd (gcd a b) c"
        (gcd (gcd a b) c)
    & report
        "gcd b c"
        (gcd b c)
    & report
        "gcd a (gcd b c)"
        (gcd a (gcd b c))

gcdMonoidLaw_equivalence_commonPrefix
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_equivalence_commonPrefix a b =
    makeProperty
        "gcd a b == commonPrefix a b"
        (gcd a b == commonPrefix a b)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "commonPrefix a b"
        (commonPrefix a b)

gcdMonoidLaw_equivalence_commonSuffix
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_equivalence_commonSuffix a b =
    makeProperty
        "gcd a b == commonSuffix a b"
        (gcd a b == commonSuffix a b)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "commonSuffix a b"
        (commonSuffix a b)

--------------------------------------------------------------------------------
-- LeftGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftGCDMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('stripPrefix' ('commonPrefix' a b) a)
-- @
-- @
-- 'isJust' ('stripPrefix' ('commonPrefix' a b) b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ 'stripPrefix' c a
--     , 'stripPrefix' c b
--     , 'stripPrefix' ('commonPrefix' a b) c
--     ]
-- ==>
--     (c '==' 'commonPrefix' a b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'commonPrefix' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'commonPrefix' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'commonPrefix' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'commonPrefix' a b '==' 'commonPrefix' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'commonPrefix' ('commonPrefix' a b) c
-- '=='
-- 'commonPrefix' a ('commonPrefix' b c)
-- @
--
-- __/Equivalences/__
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, _) -> p '==' 'commonPrefix' a b
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, x, _) -> p '<>' x '==' a
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, x) -> p '<>' x '==' b
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, x, _) -> 'Just' x '==' 'stripPrefix' p a
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, x) -> 'Just' x '==' 'stripPrefix' p b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
--
leftGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a)
    => Proxy a
    -> Laws
leftGCDMonoidLaws _ = Laws "LeftGCDMonoid"
    [ makeLaw2 @a
        "leftGCDMonoidLaw_reductivity_left"
        (leftGCDMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "leftGCDMonoidLaw_reductivity_right"
        (leftGCDMonoidLaw_reductivity_right)
    , makeLaw2 @a
        "leftGCDMonoidLaw_uniqueness"
        (leftGCDMonoidLaw_uniqueness)
    , makeLaw1 @a
        "leftGCDMonoidLaw_idempotence"
        (leftGCDMonoidLaw_idempotence)
    , makeLaw1 @a
        "leftGCDMonoidLaw_identity_left"
        (leftGCDMonoidLaw_identity_left)
    , makeLaw1 @a
        "leftGCDMonoidLaw_identity_right"
        (leftGCDMonoidLaw_identity_right)
    , makeLaw2 @a
        "leftGCDMonoidLaw_commutativity"
        (leftGCDMonoidLaw_commutativity)
    , makeLaw3 @a
        "leftGCDMonoidLaw_associativity"
        (leftGCDMonoidLaw_associativity)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_commonPrefix"
        (leftGCDMonoidLaw_stripCommonPrefix_commonPrefix)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_1"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_2"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_2)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2)
    ]

leftGCDMonoidLaw_reductivity_left
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (stripPrefix (commonPrefix a b) a)"
        (isJust (stripPrefix (commonPrefix a b) a))
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & cover
        "stripPrefix (commonPrefix a b) a /= mempty"
        (stripPrefix (commonPrefix a b) a /= mempty)
    & report
        "commonPrefix a b"
        (commonPrefix a b)
    & report
        "stripPrefix (commonPrefix a b) a"
        (stripPrefix (commonPrefix a b) a)

leftGCDMonoidLaw_reductivity_right
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (stripPrefix (commonPrefix a b) b)"
        (isJust (stripPrefix (commonPrefix a b) b))
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & cover
        "stripPrefix (commonPrefix a b) b /= mempty"
        (stripPrefix (commonPrefix a b) b /= mempty)
    & report
        "commonPrefix a b"
        (commonPrefix a b)
    & report
        "stripPrefix (commonPrefix a b) b"
        (stripPrefix (commonPrefix a b) b)

leftGCDMonoidLaw_uniqueness
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_uniqueness a b c =
    makeProperty
        "antecedent ==> consequent"
        (antecedent ==> consequent)
    -- Note that in the expressions below, we use '==' to compare Boolean
    -- expressions, even in cases where it is redundant, in order to make
    -- test output more readable:
    & cover
        "antecedent == True"
        (antecedent == True)
    & cover
        "antecedent == False"
        (antecedent == False)
    & cover
        "consequent == True"
        (consequent == True)
    & cover
        "consequent == False"
        (consequent == False)
    & report
        "stripPrefix c a"
        (stripPrefix c a)
    & report
        "stripPrefix c b"
        (stripPrefix c b)
    & report
        "commonPrefix a b"
        (commonPrefix a b)
    & report
        "stripPrefix (commonPrefix a b) c"
        (stripPrefix (commonPrefix a b) c)
  where
    antecedent =
        all isJust
            [ stripPrefix c a
            , stripPrefix c b
            , stripPrefix (commonPrefix a b) c
            ]
    consequent =
        c == commonPrefix a b

leftGCDMonoidLaw_idempotence
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> Property
leftGCDMonoidLaw_idempotence a =
    makeProperty
        "commonPrefix a a == a"
        (commonPrefix a a == a)
    & report
        "commonPrefix a a"
        (commonPrefix a a)

leftGCDMonoidLaw_identity_left
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> Property
leftGCDMonoidLaw_identity_left a =
    makeProperty
        "commonPrefix mempty a == mempty"
        (commonPrefix mempty a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "commonPrefix mempty a"
        (commonPrefix mempty a)

leftGCDMonoidLaw_identity_right
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> Property
leftGCDMonoidLaw_identity_right a =
    makeProperty
        "commonPrefix a mempty == mempty"
        (commonPrefix a mempty == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "commonPrefix a mempty"
        (commonPrefix a mempty)

leftGCDMonoidLaw_commutativity
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_commutativity a b =
    makeProperty
        "commonPrefix a b == commonPrefix b a"
        (commonPrefix a b == commonPrefix b a)
    & cover
        "commonPrefix a b == mempty"
        (commonPrefix a b == mempty)
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & report
        "commonPrefix a b"
        (commonPrefix a b)
    & report
        "commonPrefix b a"
        (commonPrefix b a)

leftGCDMonoidLaw_associativity
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_associativity a b c =
    makeProperty
        "commonPrefix (commonPrefix a b) c == commonPrefix a (commonPrefix b c)"
        (commonPrefix (commonPrefix a b) c == commonPrefix a (commonPrefix b c))
    & cover
        "commonPrefix (commonPrefix a b) c /= mempty"
        (commonPrefix (commonPrefix a b) c /= mempty)
    & cover
        "commonPrefix a (commonPrefix b c) /= mempty"
        (commonPrefix a (commonPrefix b c) /= mempty)
    & report
        "commonPrefix a b"
        (commonPrefix a b)
    & report
        "commonPrefix (commonPrefix a b) c"
        (commonPrefix (commonPrefix a b) c)
    & report
        "commonPrefix b c"
        (commonPrefix b c)
    & report
        "commonPrefix a (commonPrefix b c)"
        (commonPrefix a (commonPrefix b c))

leftGCDMonoidLaw_stripCommonPrefix_commonPrefix
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_commonPrefix a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, _) -> p == commonPrefix a b"
        (stripCommonPrefix a b & \(p, _, _) -> p == commonPrefix a b)
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "commonPrefix a b"
        (commonPrefix a b)

leftGCDMonoidLaw_stripCommonPrefix_mappend_1
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> p <> x == a"
        (stripCommonPrefix a b & \(p, x, _) -> p <> x == a)
    & cover
        "stripCommonPrefix a b & λ(p, x, _) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, x, _) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, x, _) -> p <> x"
        (stripCommonPrefix a b & \(p, x, _) -> p <> x)

leftGCDMonoidLaw_stripCommonPrefix_mappend_2
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> p <> x == b"
        (stripCommonPrefix a b & \(p, _, x) -> p <> x == b)
    & cover
        "stripCommonPrefix a b & λ(p, _, x) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, _, x) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, x) -> p <> x"
        (stripCommonPrefix a b & \(p, _, x) -> p <> x)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> Just x == stripPrefix p a"
        (stripCommonPrefix a b & \(p, x, _) -> Just x == stripPrefix p a)
    & cover
        "stripCommonPrefix a b & λ(p, x, _) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, x, _) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, _) -> stripPrefix p a"
        (stripCommonPrefix a b & \(p, _, _) -> stripPrefix p a)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> Just x == stripPrefix p b"
        (stripCommonPrefix a b & \(p, _, x) -> Just x == stripPrefix p b)
    & cover
        "stripCommonPrefix a b & λ(p, _, x) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, _, x) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, _) -> stripPrefix p b"
        (stripCommonPrefix a b & \(p, _, _) -> stripPrefix p b)

--------------------------------------------------------------------------------
-- OverlappingGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'OverlappingGCDMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('stripSuffix' ('overlap' a b) a)
-- @
-- @
-- 'isJust' ('stripPrefix' ('overlap' a b) b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'overlap' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'overlap' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'overlap' a 'mempty' '==' 'mempty'
-- @
--
-- __/Equivalences/__
--
-- @
-- 'overlap' a b '<>' 'stripPrefixOverlap' a b '==' b
-- @
--
-- @
-- 'stripSuffixOverlap' b a '<>' 'overlap' a b '==' a
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(_, x, _) -> x '==' 'overlap' a b
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(_, _, x) -> x '==' 'stripPrefixOverlap' a b
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(x, _, _) -> x '==' 'stripSuffixOverlap' b a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
overlappingGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, OverlappingGCDMonoid a)
    => Proxy a
    -> Laws
overlappingGCDMonoidLaws _ = Laws "OverlappingGCDMonoid"
    [ makeLaw2 @a
        "overlappingGCDMonoidLaw_reductivity_left"
        (overlappingGCDMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_reductivity_right"
        (overlappingGCDMonoidLaw_reductivity_right)
    , makeLaw1 @a
        "overlappingGCDMonoidLaw_idempotence"
        (overlappingGCDMonoidLaw_idempotence)
    , makeLaw1 @a
        "overlappingGCDMonoidLaw_identity_left"
        (overlappingGCDMonoidLaw_identity_left)
    , makeLaw1 @a
        "overlappingGCDMonoidLaw_identity_right"
        (overlappingGCDMonoidLaw_identity_right)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripSuffixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_overlap"
        (overlappingGCDMonoidLaw_stripOverlap_overlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap)
    ]

overlappingGCDMonoidLaw_reductivity_left
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (stripSuffix (overlap a b) a)"
        (isJust (stripSuffix (overlap a b) a))
    & cover
        "overlap a b /= mempty"
        (overlap a b /= mempty)
    & cover
        "stripSuffix (overlap a b) a /= mempty"
        (stripSuffix (overlap a b) a /= mempty)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripSuffix (overlap a b) a"
        (stripSuffix (overlap a b) a)

overlappingGCDMonoidLaw_reductivity_right
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (stripPrefix (overlap a b) b)"
        (isJust (stripPrefix (overlap a b) b))
    & cover
        "overlap a b /= mempty"
        (overlap a b /= mempty)
    & cover
        "stripPrefix (overlap a b) b /= mempty"
        (stripPrefix (overlap a b) b /= mempty)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripPrefix (overlap a b) b"
        (stripPrefix (overlap a b) b)

overlappingGCDMonoidLaw_idempotence
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> Property
overlappingGCDMonoidLaw_idempotence a =
    makeProperty
        "overlap a a == a"
        (overlap a a == a)
    & report
        "overlap a a"
        (overlap a a)

overlappingGCDMonoidLaw_identity_left
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> Property
overlappingGCDMonoidLaw_identity_left a =
    makeProperty
        "overlap mempty a == mempty"
        (overlap mempty a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "overlap mempty a"
        (overlap mempty a)

overlappingGCDMonoidLaw_identity_right
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> Property
overlappingGCDMonoidLaw_identity_right a =
    makeProperty
        "overlap a mempty == mempty"
        (overlap a mempty == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "overlap a mempty"
        (overlap a mempty)

overlappingGCDMonoidLaw_overlap_stripPrefixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripPrefixOverlap a b =
    makeProperty
        "overlap a b <> stripPrefixOverlap a b == b"
        (overlap a b <> stripPrefixOverlap a b == b)
    & cover
        "overlap a b /= mempty && stripPrefixOverlap a b /= mempty"
        (overlap a b /= mempty && stripPrefixOverlap a b /= mempty)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripPrefixOverlap a b"
        (stripPrefixOverlap a b)
    & report
        "overlap a b <> stripPrefixOverlap a b"
        (overlap a b <> stripPrefixOverlap a b)

overlappingGCDMonoidLaw_overlap_stripSuffixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripSuffixOverlap a b =
    makeProperty
        "stripSuffixOverlap b a <> overlap a b == a"
        (stripSuffixOverlap b a <> overlap a b == a)
    & cover
        "stripSuffixOverlap b a /= mempty && overlap a b /= mempty"
        (stripSuffixOverlap b a /= mempty && overlap a b /= mempty)
    & report
        "stripSuffixOverlap b a"
        (stripSuffixOverlap b a)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripSuffixOverlap b a <> overlap a b"
        (stripSuffixOverlap b a <> overlap a b)

overlappingGCDMonoidLaw_stripOverlap_overlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_overlap a b =
    makeProperty
        "stripOverlap a b & λ(_, x, _) -> x == overlap a b"
        (stripOverlap a b & \(_, x, _) -> x == overlap a b)
    & cover
        "overlap a b /= mempty"
        (overlap a b /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "overlap a b"
        (overlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(_, _, x) -> x == stripPrefixOverlap a b"
        (stripOverlap a b & \(_, _, x) -> x == stripPrefixOverlap a b)
    & cover
        "stripPrefixOverlap a b /= mempty"
        (stripPrefixOverlap a b /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "stripPrefixOverlap a b"
        (stripPrefixOverlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(x, _, _) -> x == stripSuffixOverlap b a"
        (stripOverlap a b & \(x, _, _) -> x == stripSuffixOverlap b a)
    & cover
        "stripSuffixOverlap b a /= mempty"
        (stripSuffixOverlap b a /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "stripSuffixOverlap b a"
        (stripSuffixOverlap b a)

--------------------------------------------------------------------------------
-- RightGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightGCDMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('stripSuffix' ('commonSuffix' a b) a)
-- @
-- @
-- 'isJust' ('stripSuffix' ('commonSuffix' a b) b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ 'stripSuffix' c a
--     , 'stripSuffix' c b
--     , 'stripSuffix' ('commonSuffix' a b) c
--     ]
-- ==>
--     (c '==' 'commonSuffix' a b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'commonSuffix' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'commonSuffix' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'commonSuffix' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'commonSuffix' a b '==' 'commonSuffix' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'commonSuffix' ('commonSuffix' a b) c
-- '=='
-- 'commonSuffix' a ('commonSuffix' b c)
-- @
--
-- __/Equivalences/__
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, _, s) -> s '==' 'commonSuffix' a b
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(x, _, s) -> x '<>' s '==' a
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, x, s) -> x '<>' s '==' b
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(x, _, s) -> 'Just' x '==' 'stripSuffix' s a
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, x, s) -> 'Just' x '==' 'stripSuffix' s b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
rightGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a)
    => Proxy a
    -> Laws
rightGCDMonoidLaws _ = Laws "RightGCDMonoid"
    [ makeLaw2 @a
        "rightGCDMonoidLaw_reductivity_left"
        (rightGCDMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "rightGCDMonoidLaw_reductivity_right"
        (rightGCDMonoidLaw_reductivity_right)
    , makeLaw2 @a
        "rightGCDMonoidLaw_uniqueness"
        (rightGCDMonoidLaw_uniqueness)
    , makeLaw1 @a
        "rightGCDMonoidLaw_idempotence"
        (rightGCDMonoidLaw_idempotence)
    , makeLaw1 @a
        "rightGCDMonoidLaw_identity_left"
        (rightGCDMonoidLaw_identity_left)
    , makeLaw1 @a
        "rightGCDMonoidLaw_identity_right"
        (rightGCDMonoidLaw_identity_right)
    , makeLaw2 @a
        "rightGCDMonoidLaw_commutativity"
        (rightGCDMonoidLaw_commutativity)
    , makeLaw3 @a
        "rightGCDMonoidLaw_associativity"
        (rightGCDMonoidLaw_associativity)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_commonSuffix"
        (rightGCDMonoidLaw_stripCommonSuffix_commonSuffix)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_1"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_2"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_2)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2)
    ]

rightGCDMonoidLaw_reductivity_left
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (stripSuffix (commonSuffix a b) a)"
        (isJust (stripSuffix (commonSuffix a b) a))
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & cover
        "stripSuffix (commonSuffix a b) a /= mempty"
        (stripSuffix (commonSuffix a b) a /= mempty)
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "stripSuffix (commonSuffix a b) a"
        (stripSuffix (commonSuffix a b) a)

rightGCDMonoidLaw_reductivity_right
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (stripSuffix (commonSuffix a b) b)"
        (isJust (stripSuffix (commonSuffix a b) b))
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & cover
        "stripSuffix (commonSuffix a b) b /= mempty"
        (stripSuffix (commonSuffix a b) b /= mempty)
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "stripSuffix (commonSuffix a b) b"
        (stripSuffix (commonSuffix a b) b)

rightGCDMonoidLaw_uniqueness
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_uniqueness a b c =
    makeProperty
        "antecedent ==> consequent"
        (antecedent ==> consequent)
    -- Note that in the expressions below, we use '==' to compare Boolean
    -- expressions, even in cases where it is redundant, in order to make
    -- test output more readable:
    & cover
        "antecedent == True"
        (antecedent == True)
    & cover
        "antecedent == False"
        (antecedent == False)
    & cover
        "consequent == True"
        (consequent == True)
    & cover
        "consequent == False"
        (consequent == False)
    & report
        "stripSuffix c a"
        (stripSuffix c a)
    & report
        "stripSuffix c b"
        (stripSuffix c b)
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "stripSuffix (commonSuffix a b) c"
        (stripSuffix (commonSuffix a b) c)
  where
    antecedent =
        all isJust
            [ stripSuffix c a
            , stripSuffix c b
            , stripSuffix (commonSuffix a b) c
            ]
    consequent =
        c == commonSuffix a b

rightGCDMonoidLaw_idempotence
    :: (Eq a, Show a, RightGCDMonoid a) => a -> Property
rightGCDMonoidLaw_idempotence a =
    makeProperty
        "commonSuffix a a == a"
        (commonSuffix a a == a)
    & report
        "commonSuffix a a"
        (commonSuffix a a)

rightGCDMonoidLaw_identity_left
    :: (Eq a, Show a, RightGCDMonoid a) => a -> Property
rightGCDMonoidLaw_identity_left a =
    makeProperty
        "commonSuffix mempty a == mempty"
        (commonSuffix mempty a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "commonSuffix mempty a"
        (commonSuffix mempty a)

rightGCDMonoidLaw_identity_right
    :: (Eq a, Show a, RightGCDMonoid a) => a -> Property
rightGCDMonoidLaw_identity_right a =
    makeProperty
        "commonSuffix a mempty == mempty"
        (commonSuffix a mempty == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "commonSuffix a mempty"
        (commonSuffix a mempty)

rightGCDMonoidLaw_commutativity
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_commutativity a b =
    makeProperty
        "commonSuffix a b == commonSuffix b a"
        (commonSuffix a b == commonSuffix b a)
    & cover
        "commonSuffix a b == mempty"
        (commonSuffix a b == mempty)
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "commonSuffix b a"
        (commonSuffix b a)

rightGCDMonoidLaw_associativity
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_associativity a b c =
    makeProperty
        "commonSuffix (commonSuffix a b) c == commonSuffix a (commonSuffix b c)"
        (commonSuffix (commonSuffix a b) c == commonSuffix a (commonSuffix b c))
    & cover
        "commonSuffix (commonSuffix a b) c /= mempty"
        (commonSuffix (commonSuffix a b) c /= mempty)
    & cover
        "commonSuffix a (commonSuffix b c) /= mempty"
        (commonSuffix a (commonSuffix b c) /= mempty)
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "commonSuffix (commonSuffix a b) c"
        (commonSuffix (commonSuffix a b) c)
    & report
        "commonSuffix b c"
        (commonSuffix b c)
    & report
        "commonSuffix a (commonSuffix b c)"
        (commonSuffix a (commonSuffix b c))

rightGCDMonoidLaw_stripCommonSuffix_commonSuffix
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_commonSuffix a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, _, s) -> s == commonSuffix a b"
        (stripCommonSuffix a b & \(_, _, s) -> s == commonSuffix a b)
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "commonSuffix a b"
        (commonSuffix a b)

rightGCDMonoidLaw_stripCommonSuffix_mappend_1
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> x <> s == a"
        (stripCommonSuffix a b & \(x, _, s) -> x <> s == a)
    & cover
        "stripCommonSuffix a b & λ(x, _, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(x, _, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(x, _, s) -> x <> s"
        (stripCommonSuffix a b & \(x, _, s) -> x <> s)

rightGCDMonoidLaw_stripCommonSuffix_mappend_2
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> x <> s == b"
        (stripCommonSuffix a b & \(_, x, s) -> x <> s == b)
    & cover
        "stripCommonSuffix a b & λ(_, x, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(_, x, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, x, s) -> x <> s"
        (stripCommonSuffix a b & \(_, x, s) -> x <> s)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> Just x == stripSuffix s a"
        (stripCommonSuffix a b & \(x, _, s) -> Just x == stripSuffix s a)
    & cover
        "stripCommonSuffix a b & λ(x, _, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(x, _, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, _, s) -> stripSuffix s a"
        (stripCommonSuffix a b & \(_, _, s) -> stripSuffix s a)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> Just x == stripSuffix s b"
        (stripCommonSuffix a b & \(_, x, s) -> Just x == stripSuffix s b)
    & cover
        "stripCommonSuffix a b & λ(_, x, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(_, x, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, _, s) -> stripSuffix s b"
        (stripCommonSuffix a b & \(_, _, s) -> stripSuffix s b)

--------------------------------------------------------------------------------
-- DistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'DistributiveGCDMonoid'.
--
-- Includes the following laws:
--
-- __/Left-distributivity/__
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
--
-- __/Right-distributivity/__
--
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'gcdMonoidLaws'
-- * 'leftDistributiveGCDMonoidLaws'
-- * 'rightDistributiveGCDMonoidLaws'
--
distributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, DistributiveGCDMonoid a)
    => Proxy a
    -> Laws
distributiveGCDMonoidLaws _ = Laws "DistributiveGCDMonoid"
    [ makeLaw3 @a
        "distributiveGCDMonoidLaw_distributivity_left"
        (distributiveGCDMonoidLaw_distributivity_left)
    , makeLaw3 @a
        "distributiveGCDMonoidLaw_distributivity_right"
        (distributiveGCDMonoidLaw_distributivity_right)
    ]

distributiveGCDMonoidLaw_distributivity_left
    :: (Eq a, Show a, DistributiveGCDMonoid a) => a -> a -> a -> Property
distributiveGCDMonoidLaw_distributivity_left a b c =
    makeProperty
        "gcd (a <> b) (a <> c) == a <> gcd b c"
        (gcd (a <> b) (a <> c) == a <> gcd b c)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "gcd (a <> b) (a <> c)"
        (gcd (a <> b) (a <> c))
    & report
        "gcd b c"
        (gcd b c)
    & report
        "a <> gcd b c"
        (a <> gcd b c)

distributiveGCDMonoidLaw_distributivity_right
    :: (Eq a, Show a, DistributiveGCDMonoid a) => a -> a -> a -> Property
distributiveGCDMonoidLaw_distributivity_right a b c =
    makeProperty
        "gcd (a <> c) (b <> c) == gcd a b <> c"
        (gcd (a <> c) (b <> c) == gcd a b <> c)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "gcd (a <> c) (b <> c)"
        (gcd (a <> c) (b <> c))
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd a b <> c"
        (gcd a b <> c)

--------------------------------------------------------------------------------
-- LeftDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftDistributiveGCDMonoid'.
--
-- Includes the following law:
--
-- __/Left-distributivity/__
--
-- @
-- 'commonPrefix' (a '<>' b) (a '<>' c) '==' a '<>' 'commonPrefix' b c
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'leftGCDMonoidLaws'
--
leftDistributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftDistributiveGCDMonoid a)
    => Proxy a
    -> Laws
leftDistributiveGCDMonoidLaws _ = Laws "LeftDistributiveGCDMonoid"
    [ makeLaw3 @a
        "leftDistributiveGCDMonoidLaw_distributivity"
        (leftDistributiveGCDMonoidLaw_distributivity)
    ]

leftDistributiveGCDMonoidLaw_distributivity
    :: (Eq a, Show a, LeftDistributiveGCDMonoid a) => a -> a -> a -> Property
leftDistributiveGCDMonoidLaw_distributivity a b c =
    makeProperty
        "commonPrefix (a <> b) (a <> c) == a <> commonPrefix b c"
        (commonPrefix (a <> b) (a <> c) == a <> commonPrefix b c)
    & cover
        "commonPrefix b c /= mempty && a /= mempty"
        (commonPrefix b c /= mempty && a /= mempty)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "commonPrefix (a <> b) (a <> c)"
        (commonPrefix (a <> b) (a <> c))
    & report
        "commonPrefix b c"
        (commonPrefix b c)
    & report
        "a <> commonPrefix b c"
        (a <> commonPrefix b c)

--------------------------------------------------------------------------------
-- RightDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightDistributiveGCDMonoid'.
--
-- Includes the following law:
--
-- __/Right-distributivity/__
--
-- @
-- 'commonSuffix' (a '<>' c) (b '<>' c) '==' 'commonSuffix' a b '<>' c
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'rightGCDMonoidLaws'
--
rightDistributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightDistributiveGCDMonoid a)
    => Proxy a
    -> Laws
rightDistributiveGCDMonoidLaws _ = Laws "RightDistributiveGCDMonoid"
    [ makeLaw3 @a
        "rightDistributiveGCDMonoidLaw_distributivity"
        (rightDistributiveGCDMonoidLaw_distributivity)
    ]

rightDistributiveGCDMonoidLaw_distributivity
    :: (Eq a, Show a, RightDistributiveGCDMonoid a) => a -> a -> a -> Property
rightDistributiveGCDMonoidLaw_distributivity a b c =
    makeProperty
        "commonSuffix (a <> c) (b <> c) == commonSuffix a b <> c"
        (commonSuffix (a <> c) (b <> c) == commonSuffix a b <> c)
    & cover
        "commonSuffix a b /= mempty && c /= mempty"
        (commonSuffix a b /= mempty && c /= mempty)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "commonSuffix (a <> c) (b <> c)"
        (commonSuffix (a <> c) (b <> c))
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "commonSuffix a b <> c"
        (commonSuffix a b <> c)
