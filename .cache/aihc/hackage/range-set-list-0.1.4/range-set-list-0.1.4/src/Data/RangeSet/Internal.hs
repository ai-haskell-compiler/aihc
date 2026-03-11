{- |
Module      :  Data.RangeSet.Internal
Description :  Support functions for dealing with distinct ordered range lists
Copyright   :  (c) Dylan Simon 2015
License     :  MIT

Maintainer  :  oleg.grenrus@iki.fi
Stability   :  experimental
Portability :  non-portable (tested with GHC only)

Most functions in this module deal with normalized (closed, fst <= snd,
non-overlapping, non-adjacent, ordered) ranges, but do not check this
assumption.  Most users should use a higher-level interface.
-}
{-# LANGUAGE Safe #-}
module Data.RangeSet.Internal
  ( rangeSize
  , rangeIsSubsetList
  , isSubsetRangeList
  , insertRangeList
  , deleteRangeList
  , unionRangeList
  , differenceRangeList
  , intersectRangeList
  , complementRangeList
  , fromAscElemList
  , fromElemList
  , normalizeRangeList
  , validRangeList
  ) where

import Data.List   (sort)
import Data.Monoid (Sum (..))

-- | Determine the number of items in an 'Enum' range as a 'Sum'
rangeSize :: Enum a => a -> a -> Sum Int
rangeSize a b = Sum $ succ $ fromEnum b - fromEnum a

-- | Determine if @[x,y]@ is a subset of the list, returning the list right of
-- @y@ if so.
rangeIsSubsetList :: Ord a => a -> a -> [(a, a)] -> Maybe [(a, a)]
rangeIsSubsetList x y ((u,v):s)
  | x < u = Nothing
  | y <= v = Just ((y,v):s)
  | otherwise = rangeIsSubsetList x y s
rangeIsSubsetList _ _ [] = Nothing

-- | Determine if the first list is a subset of the second.
isSubsetRangeList :: Ord a => [(a, a)] -> [(a, a)] -> Bool
isSubsetRangeList ((x,y):as) bs = maybe False (isSubsetRangeList as) $ rangeIsSubsetList x y bs
isSubsetRangeList [] _ = True

-- | Add @[x,y]@.
--
-- There are three possibilities we consider, when inserting into non-empty set:
--
-- * discretely after: continue
-- * discretely before: prepend
-- * overlapping: union and prepend
insertRangeList :: (Ord a, Enum a) => a -> a -> [(a, a)] -> [(a, a)]
insertRangeList x y set@(uv@(u,v) : xs)
  | v < x && succ v /= x = uv : insertRangeList x y xs
  | y < u && succ y /= u = (x,y) : set
  | otherwise            = prependRangeList (min x u) (max y v) xs
insertRangeList x y [] = [(x,y)]

-- | Add @[x,y]@ to the beginning (assuming @x <= u@).
prependRangeList :: (Ord a, Enum a) => a -> a -> [(a, a)] -> [(a, a)]
prependRangeList x y set@((u,v) : xs)
  | y < u && succ y /= u = (x,y) : set
  | otherwise            = prependRangeList x (max y v) xs
prependRangeList x y [] = [(x,y)]

-- | Union two range lists.
unionRangeList :: (Ord a, Enum a) => [(a, a)] -> [(a, a)] -> [(a, a)]
unionRangeList aset@(xy@(x,y):as) bset@(uv@(u,v):bs)
  | y < u && succ y /= u = xy : unionRangeList as bset
  | v < x && succ v /= x = uv : unionRangeList aset bs
  | otherwise = prependRangeList (min x u) (max y v) $ unionRangeList as bs
unionRangeList s [] = s
unionRangeList [] s = s

-- | Remove a range from a range list.
--
-- There are 6 possibilities we consider, when deleting from non-empty set:
--
-- * more
-- * less
-- * strictly inside (splits)
-- * overlapping less-edge
-- * overlapping more-edge
-- * stricly larger
deleteRangeList :: (Ord a, Enum a) => a -> a -> [(a, a)] -> [(a, a)]
deleteRangeList x y set@(s@(u,v) : xs)
  | v < x = s : deleteRangeList x y xs
  | y < u = set
  | u < x = (u, pred x) : t
  | otherwise = t where
  t = trimRangeList' y v xs
deleteRangeList _ _ [] = []

-- | Remove @(,y]@ while (re-)adding @(y,v]@ if valid
trimRangeList' :: (Ord a, Enum a) => a -> a -> [(a, a)] -> [(a, a)]
trimRangeList' y v xs
  | y < v = (succ y, v) : xs
  | otherwise = trimRangeList y xs

-- | Remove @(,y]@
trimRangeList :: (Ord a, Enum a) => a -> [(a, a)] -> [(a, a)]
trimRangeList y set@((u,v) : xs)
  | y < u = set
  | otherwise = trimRangeList' y v xs
trimRangeList _ [] = []

-- | Compute the set difference, removing each range in the second list from
-- the first.
differenceRangeList :: (Ord a, Enum a) => [(a, a)] -> [(a, a)] -> [(a, a)]
differenceRangeList aset@(xy@(x,y):as) bset@((u,v):bs)
  | y < u = xy : differenceRangeList as bset
  | v < x = differenceRangeList aset bs
  | x < u = (x, pred u) : t
  | otherwise = t where
  t = differenceRangeList (trimRangeList' v y as) bs
differenceRangeList s [] = s
differenceRangeList [] _ = []

-- | Compute the intersection.
intersectRangeList :: Ord a => [(a, a)] -> [(a, a)] -> [(a, a)]
intersectRangeList aset@((x,y):as) bset@((u,v):bs)
  | y < u = intersectRangeList as bset
  | v < x = intersectRangeList aset bs
  | y < v = (max x u, y) : intersectRangeList as bset
  | otherwise = (max x u, v) : intersectRangeList aset bs
intersectRangeList _ [] = []
intersectRangeList [] _ = []

-- | Compute the complement intersected with @[x,)@ assuming @x<u@.
complementRangeList' :: (Ord a, Enum a, Bounded a) => a -> [(a, a)] -> [(a, a)]
complementRangeList' x ((u,v):s) = (x,pred u) : complementRangeList'' v s
complementRangeList' x [] = [(x,maxBound)]

-- | Compute the complement intersected with @(x,)@.
complementRangeList'' :: (Ord a, Enum a, Bounded a) => a -> [(a, a)] -> [(a, a)]
complementRangeList'' x s
  | x == maxBound = []
  | otherwise = complementRangeList' (succ x) s

-- | Compute the complement.
complementRangeList :: (Ord a, Enum a, Bounded a) => [(a, a)] -> [(a, a)]
complementRangeList s@((x,y):s')
  | x == minBound = complementRangeList'' y s'
  | otherwise = complementRangeList' minBound s
complementRangeList [] = [(minBound, maxBound)]

-- | Take elements off the beginning of the list while they are equal or
-- adjacent to the given item, and return the last removed item and remaining
-- list.
takeWhileAdj :: (Eq a, Enum a) => a -> [a] -> (a, [a])
takeWhileAdj x yl@(y:l)
  | x == y || succ x == y = takeWhileAdj y l
  | otherwise = (x, yl)
takeWhileAdj x [] = (x, [])

-- | Take ranges off the beginning of a unnormalized but sorted and valid range
-- list while they are overlapping or adjacent to the given value, and return
-- the last removed item and remaining list.
takeWhileRangeAdj :: (Ord a, Enum a) => a -> [(a,a)] -> (a, [(a,a)])
takeWhileRangeAdj x yzl@((y,z):l)
  | x >= y || succ x == y = takeWhileRangeAdj (max x z) l
  | otherwise = (x, yzl)
takeWhileRangeAdj x [] = (x, [])

-- | Normalize a sorted list of elements to a range list.
fromAscElemList :: (Eq a, Enum a) => [a] -> [(a, a)]
fromAscElemList (x:l) = (x, y) : fromAscElemList l' where
  (y, l') = takeWhileAdj x l
fromAscElemList [] = []

-- | Normalize an arbitrary list of elements to a range list.
fromElemList :: (Ord a, Enum a) => [a] -> [(a, a)]
fromElemList = fromAscElemList . sort

-- | Normalize a sorted list of valid ranges.
mergeRangeList :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
mergeRangeList ((x,y):l) = (x,y') : mergeRangeList l' where
  (y', l') = takeWhileRangeAdj y l
mergeRangeList [] = []

-- | Normalize an arbitrary list of ranges.
normalizeRangeList :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
normalizeRangeList = mergeRangeList . sort . filter valid where
  valid (x,y) = x <= y

-- | Check if a list is normalized and strictly above @b@.
validRangeList' :: (Ord a, Enum a, Bounded a) => a -> [(a, a)] -> Bool
validRangeList' b ((x,y):s) = b < maxBound && succ b < x && x <= y && validRangeList' y s
validRangeList' _ [] = True

-- | Check if a list is normalized.
validRangeList :: (Ord a, Enum a, Bounded a) => [(a, a)] -> Bool
validRangeList ((x,y):s) = x <= y && validRangeList' y s
validRangeList [] = True
