{- |
Module      :  Data.RangeSet.IntMap
Description :  Specialization of Data.RangeSet.Map to Ints
Copyright   :  (c) Dylan Simon, 2015
License     :  MIT

This is simply a specialization of "Data.RangeSet.Map" to 'Int'.

The implementation of 'RIntSet' is based on "Data.IntMap.Strict".
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe               #-}
module Data.RangeSet.IntMap (
  -- * Range set type
  RIntSet

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

import           Control.DeepSeq    (NFData (..))
import qualified Data.Foldable      as Fold
import           Data.Functor       ((<$>))
import qualified Data.IntMap.Strict as Map
import           Data.Monoid        (Monoid (..), getSum)
import           Data.Semigroup     (Semigroup (..))
import           Data.Typeable      (Typeable)

import           Data.RangeSet.Internal
import qualified Data.RangeSet.List     as RList

-- | Internally set is represented as sorted list of distinct inclusive ranges.
newtype RIntSet = RSet (Map.IntMap Int)
  deriving (Eq, Ord, Typeable)

instance Show RIntSet where
  showsPrec d x = showParen (d > 10)
    $ showString "fromRangeList "
    . showsPrec 11 (toRangeList x)

instance Semigroup RIntSet where
  (<>) = union

instance Monoid RIntSet where
  mempty  = empty
  mappend = union

instance NFData RIntSet where
  rnf (RSet xs) = rnf xs

{- Operators -}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: RIntSet -> RIntSet -> RIntSet
m1 \\ m2 = difference m1 m2

{- Query -}

-- | /O(1)/. Is this the empty set?
null :: RIntSet -> Bool
null (RSet m) = Map.null m

-- | /O(1)/. Is this the empty set?
isFull :: RIntSet -> Bool
isFull = (==) full

-- | /O(n)/. The number of the elements in the set.
size :: RIntSet -> Int
size (RSet xm) = getSum $ Map.foldMapWithKey rangeSize xm

contains' :: Int -> Int -> RIntSet -> Bool
contains' x y (RSet xm) = Fold.any ((y <=) . snd) $ Map.lookupLE x xm

-- | /O(log n)/. Is the element in the set?
member :: Int -> RIntSet -> Bool
member x = contains' x x

-- | /O(log n)/. Is the element not in the set?
notMember :: Int -> RIntSet -> Bool
notMember a r = not $ member a r

-- | /O(log n)/. Find largest element smaller than the given one.
lookupLT :: Int -> RIntSet -> Maybe Int
lookupLT x (RSet xm) = min (pred x) . snd <$> Map.lookupLT x xm

-- | /O(log n)/. Find smallest element greater than the given one.
lookupGT :: Int -> RIntSet -> Maybe Int
lookupGT x (RSet xm)
  | Just (_, b) <- Map.lookupLE x xm, x < b = Just (succ x)
  | otherwise = fst <$> Map.lookupGT x xm

-- | /O(log n)/. Find largest element smaller or equal to than the given one.
lookupLE :: Int -> RIntSet -> Maybe Int
lookupLE x (RSet xm) = min x . snd <$> Map.lookupLE x xm

-- | /O(log n)/. Find smallest element greater or equal to than the given one.
lookupGE :: Int -> RIntSet -> Maybe Int
lookupGE x (RSet xm)
  | Just (_, b) <- Map.lookupLE x xm, x <= b = Just x
  | otherwise = fst <$> Map.lookupGT x xm

-- | /O(log n)/. Is the entire range contained within the set?
containsRange :: (Int, Int) -> RIntSet -> Bool
containsRange (x,y) s
  | x <= y = contains' x y s
  | otherwise = True

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: RIntSet -> RIntSet -> Bool
isSubsetOf x y = isSubsetRangeList (toRangeList x) (toRangeList y)

{- Construction -}

-- | /O(1)/. The empty set.
empty :: RIntSet
empty = RSet Map.empty

-- | /O(1)/. The full set.
full :: RIntSet
full = singletonRange' minBound maxBound

singletonRange' :: Int -> Int -> RIntSet
singletonRange' x y = RSet $ Map.singleton x y

-- | /O(1)/. Create a singleton set.
singleton :: Int -> RIntSet
singleton x = singletonRange' x x

-- | /O(1)/. Create a continuos range set.
singletonRange :: (Int, Int) -> RIntSet
singletonRange (x, y) | x > y     = empty
                      | otherwise = singletonRange' x y

{- Construction -}

insertRange' :: Int -> Int -> RIntSet -> RIntSet
insertRange' x y s = unRangeList $ insertRangeList x y $ toRangeList s

-- | /O(n)/. Insert an element in a set.
insert :: Int -> RIntSet -> RIntSet
insert x = insertRange' x x

-- | /O(n)/. Insert a continuos range in a set.
insertRange :: (Int, Int) -> RIntSet -> RIntSet
insertRange (x, y) set
  | x > y      = set
  | otherwise  = insertRange' x y set

deleteRange' :: Int -> Int -> RIntSet -> RIntSet
deleteRange' x y s = unRangeList $ deleteRangeList x y $ toRangeList s

-- | /O(n). Delete an element from a set.
delete :: Int -> RIntSet -> RIntSet
delete x = deleteRange' x x

-- | /O(n). Delete a continuos range from a set.
deleteRange :: (Int, Int) -> RIntSet -> RIntSet
deleteRange (x, y) set
  | x > y      = set
  | otherwise  = deleteRange' x y set

{- Combination -}

-- | /O(n*m)/. The union of two sets.
union :: RIntSet -> RIntSet -> RIntSet
union x y = unRangeList $ unionRangeList (toRangeList x) (toRangeList y)

-- | /O(n*m)/. Difference of two sets.
difference :: RIntSet -> RIntSet -> RIntSet
difference x y = unRangeList $ differenceRangeList (toRangeList x) (toRangeList y)

-- | /O(n*m)/. The intersection of two sets.
intersection :: RIntSet -> RIntSet -> RIntSet
intersection x y = unRangeList $ intersectRangeList (toRangeList x) (toRangeList y)

{- Complement -}

-- | /O(n)/. Complement of the set.
complement :: RIntSet -> RIntSet
complement = unRangeList . complementRangeList . toRangeList

{- Filter -}

-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: Int -> RIntSet -> (RIntSet, RIntSet)
split x s = (l, r) where (l, _, r) = splitMember x s

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Int -> RIntSet -> (RIntSet, Bool, RIntSet)
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
findMin :: RIntSet -> Int
findMin (RSet m) = fst $ Map.findMin m

-- | /O(log n)/. The maximal element of a set.
findMax :: RIntSet -> Int
findMax (RSet m) = snd $ Map.findMax m

{- Conversion -}

unRangeList :: [(Int, Int)] -> RIntSet
unRangeList = RSet . Map.fromDistinctAscList

-- | /O(n*r)/. An alias of 'toAscList'. The elements of a set in ascending
-- order. /r/ is the size of longest range.
elems :: RIntSet -> [Int]
elems = toAscList

-- | /O(n*r)/. Convert the set to a list of elements (in arbitrary order). /r/
-- is the size of longest range.
toList :: RIntSet -> [Int]
toList (RSet xm) = Map.foldMapWithKey enumFromTo xm

-- | /O(n*log n)/. Create a set from a list of elements.
--
-- Note that unlike "Data.Set" and other binary trees, this always requires a
-- full sort and traversal to create distinct, disjoint ranges before
-- constructing the tree.
fromList :: [Int] -> RIntSet
fromList = unRangeList . fromElemList

-- | /O(n)/. Create a set from a list of ascending elements.
--
-- /The precondition is not checked./  You may use 'valid' to check the result.
-- Note that unlike "Data.Set" and other binary trees, this always requires a
-- full traversal to create distinct, disjoint ranges before constructing the
-- tree.
fromAscList :: [Int] -> RIntSet
fromAscList = unRangeList . fromAscElemList

-- | /O(n*r)/. Convert the set to an ascending list of elements.
toAscList :: RIntSet -> [Int]
toAscList (RSet xm) = Map.foldrWithKey (\a -> (++) . enumFromTo a) [] xm

-- | /O(n)/. Convert the set to a list of range pairs.
toRangeList :: RIntSet -> [(Int, Int)]
toRangeList (RSet xs) = Map.toAscList xs

-- | /O(n*log n)/. Create a set from a list of range pairs.
--
-- Note that unlike "Data.Set" and other binary trees, this always requires a
-- full sort and traversal to create distinct, disjoint ranges before
-- constructing the tree.
fromRangeList :: [(Int, Int)] -> RIntSet
fromRangeList = unRangeList . normalizeRangeList

-- | /O(n)/. Convert a list-based 'RList.RSet' to a map-based 'RIntSet'.
fromRList :: RList.RSet Int -> RIntSet
fromRList = fromNormalizedRangeList . RList.toRangeList

-- | /O(n)/. Convert a map-based 'RIntSet' to a list-based 'RList.RSet'.
toRList :: RIntSet -> RList.RSet Int
toRList = RList.fromNormalizedRangeList . toRangeList

-- | /O(n)/. Convert a normalized, non-adjacent, ascending list of ranges to a set.
--
-- /The precondition is not checked./  In general you should only use this
-- function on the result of 'toRangeList' or ensure 'valid' on the result.
fromNormalizedRangeList :: [(Int, Int)] -> RIntSet
fromNormalizedRangeList = RSet . Map.fromDistinctAscList

-- | /O(n)/. Ensure that a set is valid. All functions should return valid sets
-- except those with unchecked preconditions: 'fromAscList',
-- 'fromNormalizedRangeList'
valid :: RIntSet -> Bool
valid = validRangeList . toRangeList

