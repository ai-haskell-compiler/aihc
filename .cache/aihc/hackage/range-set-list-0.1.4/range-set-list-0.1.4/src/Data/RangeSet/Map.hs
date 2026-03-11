{- |
Module      :  Data.RangeSet.Map
Description :  A slightly less trivial implementation of range sets
Copyright   :  (c) Dylan Simon, 2015
License     :  MIT

A slightly less trivial implementation of range sets.

This is nearly identical to "Data.RangeSet.List" except for some important
performance differences:

* Most query functions in this module are /O(log n)/ rather than /O(n)/, so may
  be much faster.
* Most composition functions have the same time complexity but a higher
  constant, so may be somewhat slower.

If you're mainly calling 'member', you should consider using this module, but
if you're calling 'union', 'deleteRange', and other range manipulation
functions as often as querying, you might stick with the list implementation.

This module is intended to be imported qualified, to avoid name
clashes with Prelude functions, e.g.

>  import Data.RangeSet.Map (RSet)
>  import qualified Data.RangeSet.Map as RSet

The implementation of 'RSet' is based on "Data.Map.Strict".

-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe               #-}
module Data.RangeSet.Map (
  -- * Range set type
  RSet

  -- * Operators
  , (\\)

  -- * Query
  , null
  , isFull
  , size
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , containsRange
  , isSubsetOf
  , valid

  -- * Construction
  , empty
  , full
  , singleton
  , singletonRange
  , insert
  , insertRange
  , delete
  , deleteRange

  -- * Combine
  , union
  , difference
  , intersection

  -- * Filter
  , split
  , splitMember

  -- * Min/Max
  , findMin
  , findMax

  -- * Complement
  , complement

  -- * Conversion
  , elems
  , toList
  , fromList
  , fromAscList
  , toAscList
  , toRangeList
  , fromRangeList
  , fromRList
  , toRList
  , fromNormalizedRangeList

  ) where

import Prelude hiding (filter, foldl, foldr, map, null)

import           Control.DeepSeq (NFData (..))
import qualified Data.Foldable   as Fold
import           Data.Functor    ((<$>))
import qualified Data.Map.Strict as Map
import           Data.Monoid     (Monoid (..), getSum)
import           Data.Semigroup  (Semigroup (..))
import           Data.Typeable   (Typeable)

import           Data.RangeSet.Internal
import qualified Data.RangeSet.List     as RList

-- | Internally set is represented as sorted list of distinct inclusive ranges.
newtype RSet a = RSet (Map.Map a a)
  deriving (Eq, Ord, Typeable)

instance Show a => Show (RSet a) where
  showsPrec d x = showParen (d > 10)
    $ showString "fromRangeList "
    . showsPrec 11 (toRangeList x)

instance (Ord a, Enum a) => Semigroup (RSet a) where
  (<>) = union

instance (Ord a, Enum a) => Monoid (RSet a) where
  mempty  = empty
  mappend = union

instance NFData a => NFData (RSet a) where
  rnf (RSet xs) = rnf xs

{- Operators -}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
m1 \\ m2 = difference m1 m2

{- Query -}

-- | /O(1)/. Is this the empty set?
null :: RSet a -> Bool
null (RSet m) = Map.null m

-- | /O(1)/. Is this the empty set?
isFull :: (Eq a, Bounded a) => RSet a -> Bool
isFull = (==) full

-- | /O(n)/. The number of the elements in the set.
size :: Enum a => RSet a -> Int
size (RSet xm) = getSum $ Map.foldMapWithKey rangeSize xm

contains' :: Ord a => a -> a -> RSet a -> Bool
contains' x y (RSet xm) = Fold.any ((y <=) . snd) $ Map.lookupLE x xm

-- | /O(log n)/. Is the element in the set?
member :: Ord a => a -> RSet a -> Bool
member x = contains' x x

-- | /O(log n)/. Is the element not in the set?
notMember :: Ord a => a -> RSet a -> Bool
notMember a r = not $ member a r

-- | /O(log n)/. Find largest element smaller than the given one.
lookupLT :: (Ord a, Enum a) => a -> RSet a -> Maybe a
lookupLT x (RSet xm) = min (pred x) . snd <$> Map.lookupLT x xm

-- | /O(log n)/. Find smallest element greater than the given one.
lookupGT :: (Ord a, Enum a) => a -> RSet a -> Maybe a
lookupGT x (RSet xm)
  | Just (_, b) <- Map.lookupLE x xm, x < b = Just (succ x)
  | otherwise = fst <$> Map.lookupGT x xm

-- | /O(log n)/. Find largest element smaller or equal to than the given one.
lookupLE :: Ord a => a -> RSet a -> Maybe a
lookupLE x (RSet xm) = min x . snd <$> Map.lookupLE x xm

-- | /O(log n)/. Find smallest element greater or equal to than the given one.
lookupGE :: Ord a => a -> RSet a -> Maybe a
lookupGE x (RSet xm)
  | Just (_, b) <- Map.lookupLE x xm, x <= b = Just x
  | otherwise = fst <$> Map.lookupGT x xm

-- | /O(log n)/. Is the entire range contained within the set?
containsRange :: Ord a => (a, a) -> RSet a -> Bool
containsRange (x,y) s
  | x <= y = contains' x y s
  | otherwise = True

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Ord a => RSet a -> RSet a -> Bool
isSubsetOf x y = isSubsetRangeList (toRangeList x) (toRangeList y)

{- Construction -}

-- | /O(1)/. The empty set.
empty :: RSet a
empty = RSet Map.empty

-- | /O(1)/. The full set.
full :: Bounded a => RSet a
full = singletonRange' minBound maxBound

singletonRange' :: a -> a -> RSet a
singletonRange' x y = RSet $ Map.singleton x y

-- | /O(1)/. Create a singleton set.
singleton :: a -> RSet a
singleton x = singletonRange' x x

-- | /O(1)/. Create a continuos range set.
singletonRange :: Ord a => (a, a) -> RSet a
singletonRange (x, y) | x > y     = empty
                      | otherwise = singletonRange' x y

{- Construction -}

insertRange' :: (Ord a, Enum a) => a -> a -> RSet a -> RSet a
insertRange' x y s = unRangeList $ insertRangeList x y $ toRangeList s

-- | /O(n)/. Insert an element in a set.
insert :: (Ord a, Enum a) => a -> RSet a -> RSet a
insert x = insertRange' x x

-- | /O(n)/. Insert a continuos range in a set.
insertRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
insertRange (x, y) set
  | x > y      = set
  | otherwise  = insertRange' x y set

deleteRange' :: (Ord a, Enum a) => a -> a -> RSet a -> RSet a
deleteRange' x y = unRangeList . deleteRangeList x y . toRangeList

-- | /O(n). Delete an element from a set.
delete :: (Ord a, Enum a) => a -> RSet a -> RSet a
delete x = deleteRange' x x

-- | /O(n). Delete a continuos range from a set.
deleteRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
deleteRange (x, y) set
  | x > y      = set
  | otherwise  = deleteRange' x y set

{- Combination -}

-- | /O(n*m)/. The union of two sets.
union :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
union x y = unRangeList $ unionRangeList (toRangeList x) (toRangeList y)

-- | /O(n*m)/. Difference of two sets.
difference :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
difference x y = unRangeList $ differenceRangeList (toRangeList x) (toRangeList y)

-- | /O(n*m)/. The intersection of two sets.
intersection :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
intersection x y = unRangeList $ intersectRangeList (toRangeList x) (toRangeList y)

{- Complement -}

-- | /O(n)/. Complement of the set.
complement :: (Ord a, Enum a, Bounded a) => RSet a -> RSet a
complement = unRangeList . complementRangeList . toRangeList

{- Filter -}

-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: (Ord a, Enum a) => a -> RSet a -> (RSet a, RSet a)
split x s = (l, r) where (l, _, r) = splitMember x s

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: (Ord a, Enum a) => a -> RSet a -> (RSet a, Bool, RSet a)
splitMember x (RSet xm)
  | Just y <- xv = (RSet ml, True, RSet $ insertIf (x < y) (succ x) y mr)
  | Just ((u,v), ml') <- Map.maxViewWithKey ml =
    if v < x
      then (RSet ml, False, RSet mr)
      else (RSet $ insertIf (u < x) u (pred x) ml', True, RSet $ insertIf (x < v) (succ x) v mr)
  | otherwise = (RSet ml {- empty -}, False, RSet {- mr -} xm)
  where
  (ml, xv, mr) = Map.splitLookup x xm
  insertIf False _ _ = id
  insertIf True a b = Map.insert a b

{- Min/Max -}

-- | /O(log n)/. The minimal element of a set.
findMin :: RSet a -> a
findMin (RSet m) = fst $ Map.findMin m

-- | /O(log n)/. The maximal element of a set.
findMax :: RSet a -> a
findMax (RSet m) = snd $ Map.findMax m

{- Conversion -}

unRangeList :: [(a, a)] -> RSet a
unRangeList = RSet . Map.fromDistinctAscList

-- | /O(n*r)/. An alias of 'toAscList'. The elements of a set in ascending order. /r/ is the size of longest range.
elems :: Enum a => RSet a -> [a]
elems = toAscList

-- | /O(n*r)/. Convert the set to a list of elements (in arbitrary order). /r/ is the size of longest range.
toList :: Enum a => RSet a -> [a]
toList (RSet xm) = Map.foldMapWithKey enumFromTo xm

-- | /O(n*log n)/. Create a set from a list of elements.
-- Note that unlike "Data.Set" and other binary trees, this always requires a full sort and traversal to create distinct, disjoint ranges before constructing the tree.
fromList :: (Ord a, Enum a) => [a] -> RSet a
fromList = unRangeList . fromElemList

-- | /O(n)/. Create a set from a list of ascending elements.
-- /The precondition is not checked./  You may use 'valid' to check the result.
-- Note that unlike "Data.Set" and other binary trees, this always requires a full traversal to create distinct, disjoint ranges before constructing the tree.
fromAscList :: (Ord a, Enum a) => [a] -> RSet a
fromAscList = unRangeList . fromAscElemList

-- | /O(n*r)/. Convert the set to an ascending list of elements.
toAscList :: Enum a => RSet a -> [a]
toAscList (RSet xm) = Map.foldrWithKey (\a -> (++) . enumFromTo a) [] xm

-- | /O(n)/. Convert the set to a list of range pairs.
toRangeList :: RSet a -> [(a, a)]
toRangeList (RSet xs) = Map.toAscList xs

-- | /O(n*log n)/. Create a set from a list of range pairs.
-- Note that unlike "Data.Set" and other binary trees, this always requires a full sort and traversal to create distinct, disjoint ranges before constructing the tree.
fromRangeList :: (Ord a, Enum a) => [(a, a)] -> RSet a
fromRangeList = unRangeList . normalizeRangeList

-- | /O(n)/. Convert a list-based 'RList.RSet' to a map-based 'RSet'.
fromRList :: RList.RSet a -> RSet a
fromRList = fromNormalizedRangeList . RList.toRangeList

-- | /O(n)/. Convert a map-based 'RSet' to a list-based 'RList.RSet'.
toRList :: RSet a -> RList.RSet a
toRList = RList.fromNormalizedRangeList . toRangeList

-- | /O(n)/. Convert a normalized, non-adjacent, ascending list of ranges to a set.
-- /The precondition is not checked./  In general you should only use this function on the result of 'toRangeList' or ensure 'valid' on the result.
fromNormalizedRangeList :: [(a, a)] -> RSet a
fromNormalizedRangeList = RSet . Map.fromDistinctAscList

-- | /O(n)/. Ensure that a set is valid. All functions should return valid sets except those with unchecked preconditions: 'fromAscList', 'fromNormalizedRangeList'
valid :: (Ord a, Enum a, Bounded a) => RSet a -> Bool
valid (RSet xm) = Map.valid xm && validRangeList (Map.toAscList xm)

