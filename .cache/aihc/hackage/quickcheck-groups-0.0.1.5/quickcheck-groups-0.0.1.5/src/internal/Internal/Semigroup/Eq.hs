-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Internal.Semigroup.Eq
    ( allNonNull
    , allUnique
    , allUniqueNonNull
    )
    where

import Data.List
    ( nub )

import Data.Foldable as F

allNonNull :: (Eq a, Semigroup a, Foldable f) => f a -> Bool
allNonNull as = F.all (as `canVerifyNonNull`) as

allUnique :: (Eq a, Foldable f) => f a -> Bool
allUnique as = length (nub xs) == length xs
  where
    xs = F.toList as

allUniqueNonNull :: (Eq a, Foldable f, Semigroup a) => f a -> Bool
allUniqueNonNull as = allUnique as && allNonNull as

canModify :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModify` b = (||)
    (a `canModifyL` b)
    (a `canModifyR` b)

canModifyL :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModifyL` b = b /= a <> b

canModifyR :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModifyR` b = b /= b <> a

canVerifyNonNull :: (Eq a, Semigroup a, Foldable f) => f a -> a -> Bool
canVerifyNonNull as a = F.any (a `canModify`) as
