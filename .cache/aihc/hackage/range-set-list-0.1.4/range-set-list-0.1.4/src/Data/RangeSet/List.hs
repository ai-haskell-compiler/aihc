{- |
Module      :  Data.RangeSet.List
Description :  A trivial implementation of range sets
Copyright   :  (c) Oleg Grenrus 2014
License     :  MIT

Maintainer  :  oleg.grenrus@iki.fi
Stability   :  experimental
Portability :  non-portable (tested with GHC only)

A trivial implementation of range sets.

This module is intended to be imported qualified, to avoid name
clashes with Prelude functions, e.g.

>  import Data.RangeSet.List (RSet)
>  import qualified Data.RangeSet.List as RSet

The implementation of 'RSet' is based on /list/.

Compared to 'Data.Set', this module imposes also 'Enum' restriction for many
functions.  We must be able to identify consecutive elements to be able to
/glue/ and /split/ ranges properly.

The implementation assumes that

> x < succ x
> pred x < x

and there aren't elements in between (not true for 'Float' and 'Double').  Also
'succ' and 'pred' are never called for largest or smallest value respectively.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe               #-}
module Data.RangeSet.List (
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
  , fromNormalizedRangeList
  , toSet

  ) where

import           Prelude hiding (filter, foldl, foldr, map, null)
import qualified Prelude

import           Control.DeepSeq (NFData (..))
import           Data.Foldable   (foldMap)
import           Data.Hashable   (Hashable (..))
import           Data.Maybe      (isJust)
import           Data.Monoid     (Monoid (..), getSum)
import           Data.Semigroup  (Semigroup (..))
import qualified Data.Set        as Set
import           Data.Typeable   (Typeable)

import Data.RangeSet.Internal

-- | Internally set is represented as sorted list of distinct inclusive ranges.
newtype RSet a = RSet [(a, a)]
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

instance Hashable a => Hashable (RSet a) where
    hashWithSalt salt (RSet xs) = hashWithSalt salt xs

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
null = Prelude.null . toRangeList

-- | /O(1)/. Is this the full set?
isFull :: (Eq a, Bounded a) => RSet a -> Bool
isFull = (==) full

-- | /O(n)/. The number of the elements in the set.
size :: Enum a => RSet a -> Int
size (RSet xs) = getSum $ foldMap (uncurry rangeSize) xs

-- | /O(n)/. Is the element in the set?
member :: Ord a => a -> RSet a -> Bool
member x (RSet xs) = f xs where
  f ((a,b):s)
    | x < a = False
    | x <= b = True
    | otherwise = f s
  f [] = False

-- | /O(n)/. Is the element not in the set?
notMember :: Ord a => a -> RSet a -> Bool
notMember a r = not $ member a r

-- | /O(n)/. Find largest element smaller than the given one.
lookupLT :: (Ord a, Enum a) => a -> RSet a -> Maybe a
lookupLT x (RSet xs) = f Nothing xs where
  f l ((a,b):s)
    | x <= a = l
    | x <= b || pred x == b = Just (pred x)
    | otherwise = f (Just b) s
  f l [] = l

-- | /O(n)/. Find smallest element greater than the given one.
lookupGT :: (Ord a, Enum a) => a -> RSet a -> Maybe a
lookupGT x (RSet xs) = f xs where
  f ((a,b):s)
    | x < a = Just a
    | x < b = Just (succ x)
    | otherwise = f s
  f [] = Nothing

-- | /O(n)/. Find largest element smaller or equal to than the given one.
lookupLE :: Ord a => a -> RSet a -> Maybe a
lookupLE x (RSet xs) = f Nothing xs where
  f l ((a,b):s)
    | x < a = l
    | x <= b = Just x
    | otherwise = f (Just b) s
  f l [] = l

-- | /O(n)/. Find smallest element greater or equal to than the given one.
lookupGE :: Ord a => a -> RSet a -> Maybe a
lookupGE x (RSet xs) = f xs where
  f ((a,b):s)
    | x <= a = Just a
    | x <= b = Just x
    | otherwise = f s
  f [] = Nothing

-- | /O(n)/. Is the entire range contained within the set?
containsRange :: Ord a => (a, a) -> RSet a -> Bool
containsRange (x,y) (RSet xs)
  | x <= y = isJust $ rangeIsSubsetList x y xs
  | otherwise = True

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Ord a => RSet a -> RSet a -> Bool
isSubsetOf (RSet xs) (RSet ys) = isSubsetRangeList xs ys

-- MISSING: isProperSubsetOf isRangeProperSubsetOf? overlapsRange?

{- Construction -}

-- | /O(1)/. The empty set.
empty :: RSet a
empty = RSet []

-- | /O(1)/. The full set.
full :: Bounded a => RSet a
full = singletonRange' minBound maxBound

singletonRange' :: a -> a -> RSet a
singletonRange' x y = RSet [(x, y)]

-- | /O(1)/. Create a singleton set.
singleton :: a -> RSet a
singleton x = singletonRange' x x

-- | /O(1)/. Create a continuos range set.
singletonRange :: Ord a => (a, a) -> RSet a
singletonRange (x, y) | x > y     = empty
                      | otherwise = singletonRange' x y

{- Construction -}

-- | /O(n)/. Insert an element in a set.
insert :: (Ord a, Enum a) => a -> RSet a -> RSet a
insert x (RSet xs) = RSet $ insertRangeList x x xs

-- | /O(n)/. Insert a continuos range in a set.
insertRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
insertRange (x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ insertRangeList x y xs

-- | /O(n). Delete an element from a set.
delete :: (Ord a, Enum a) => a -> RSet a -> RSet a
delete x (RSet xs) = RSet $ deleteRangeList x x xs

-- | /O(n). Delete a continuos range from a set.
deleteRange :: (Ord a, Enum a) => (a, a) -> RSet a -> RSet a
deleteRange (x, y) set@(RSet xs)
  | x > y      = set
  | otherwise  = RSet $ deleteRangeList x y xs

{- Combination -}

-- | /O(n+m)/. The union of two sets.
union :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
union (RSet xs) (RSet ys) = RSet $ unionRangeList xs ys

-- | /O(n+m)/. Difference of two sets.
difference :: (Ord a, Enum a) => RSet a -> RSet a -> RSet a
difference (RSet xs) (RSet ys) = RSet $ differenceRangeList xs ys

-- | /O(n+m)/. The intersection of two sets.
intersection :: (Ord a) => RSet a -> RSet a -> RSet a
intersection (RSet xs) (RSet ys) = RSet $ intersectRangeList xs ys

{- Complement -}

-- | /O(n)/. Complement of the set.
complement :: (Ord a, Enum a, Bounded a) => RSet a -> RSet a
complement (RSet xs) = RSet $ complementRangeList xs

{- Filter -}

-- MISSING: filter partition filterRanges? partitionRanges?

-- | /O(n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: (Ord a, Enum a) => a -> RSet a -> (RSet a, RSet a)
split x s = (l, r) where (l, _, r) = splitMember x s

-- | /O(n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: (Ord a, Enum a) => a -> RSet a -> (RSet a, Bool, RSet a)
splitMember x (RSet xs) = f xs where
  f s@(r@(a,b):s') = case compare x a of
    LT -> (empty, False, RSet s)
    EQ -> (empty, True, RSet xs')
    GT
      | x <= b -> (RSet [(a, pred x)], True, RSet xs')
      | otherwise -> push r $ f s'
    where
    xs'
      | x < b = (succ x,b):s'
      | otherwise = s'
  f [] = (empty, False, empty)
  push r (RSet ls, b, RSet rs) = (RSet (r:ls), b, RSet rs)

-- MISSING: lookupIndex findIndex elemAt deleteAt map mapMonotonic fold*
-- mapMonotonic may be reasonable as just need to map range endpoints and check adjacency

{- Min/Max -}

-- | /O(1)/. The minimal element of a set.
findMin :: RSet a -> a
findMin (RSet ((x, _) : _))  = x
findMin _                    = error "RangeSet.List.findMin: empty set"

-- | /O(n)/. The maximal element of a set.
findMax :: RSet a -> a
findMax (RSet rs) = findMax' rs
  where findMax' [(_, x)]  = x
        findMax' (_:xs)    = findMax' xs
        findMax' _         = error "RangeSet.List.findMax: empty set"

-- MISSING: deleteMin deleteMax deleteFindMin deleteFindMax minView maxView

{- Conversion -}

-- | /O(n*r)/. An alias of 'toAscList'. The elements of a set in ascending
-- order. /r/ is the size of longest range.
elems :: Enum a => RSet a -> [a]
elems = toAscList

-- | /O(n*r)/. Convert the set to a list of elements. /r/ is the size of
-- longest range.
toList :: Enum a => RSet a -> [a]
toList (RSet xs) = concatMap (uncurry enumFromTo) xs

-- | /O(n*log n)/. Create a set from a list of elements.
fromList :: (Ord a, Enum a) => [a] -> RSet a
fromList = RSet . fromElemList

-- | /O(n)/. Create a set from a list of ascending elements.
--
-- /The precondition is not checked./  You may use 'valid' to check the result.
fromAscList :: (Ord a, Enum a) => [a] -> RSet a
fromAscList = RSet . fromAscElemList

-- | /O(n*r)/. Convert the set to an ascending list of elements.
toAscList :: Enum a => RSet a -> [a]
toAscList = toList

-- | /O(1)/. Convert the set to a list of range pairs.
toRangeList :: RSet a -> [(a, a)]
toRangeList (RSet xs) = xs

-- | /O(n*log n)/. Create a set from a list of range pairs.
fromRangeList :: (Ord a, Enum a) => [(a, a)] -> RSet a
fromRangeList = RSet . normalizeRangeList

-- | /O(n*r)/. Convert the set to a 'Set.Set' of elements. /r/ is the size of
-- longest range.
toSet :: Enum a => RSet a -> Set.Set a
toSet = Set.fromDistinctAscList . toAscList

-- | /O(1)/. Convert a normalized, non-adjacent, ascending list of ranges to a
-- set.
--
-- /The precondition is not checked./  In general you should only use this
-- function on the result of 'toRangeList' or ensure 'valid' on the result.
fromNormalizedRangeList :: [(a, a)] -> RSet a
fromNormalizedRangeList = RSet

-- | /O(n)/. Ensure that a set is valid. All functions should return valid sets
-- except those with unchecked preconditions: 'fromAscList',
-- 'fromNormalizedRangeList'
valid :: (Ord a, Enum a, Bounded a) => RSet a -> Bool
valid (RSet xs) = validRangeList xs

-- MISSING: fromDistinctAscList fromAscRangeList
