
{-# LANGUAGE CPP, TypeFamilies #-}

-- | This module defines a type for sorted lists, together
--   with several functions to create and use values of that
--   type. Many operations are optimized to take advantage
--   of the list being sorted.
module Data.SortedList (
    -- * Type
    SortedList
    -- * List conversions
  , toSortedList
  , fromSortedList
    -- * Construction
  , singleton
  , repeat
  , replicate
  , iterate
    -- * Deconstruction
  , uncons
    -- * Inserting
  , insert
    -- * Deleting
  , delete
  , deleteAll
    -- * Sublists
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , span
    -- ** Filtering
  , partition
  , filter
  , filterLT
  , filterGT
  , filterLE
  , filterGE
    -- * Queries
#if !MIN_VERSION_base(4,8,0)
  , null
#endif
  , elemOrd
  , findIndices
    -- * @map@ function
  , map
  , mapDec
    -- * @traverse@ function
  , traverse
    -- * Unfolding
  , unfoldr
    -- * Others
#if MIN_VERSION_base(4,6,0)
  , reverse, reverseDown
#endif
    -- * Set operations
  , nub
  , intersect
  , union
  ) where

import Prelude hiding
  ( take, drop, splitAt, filter
  , repeat, replicate, iterate
  , null, map, reverse, traverse
  , span, takeWhile, dropWhile
#if !MIN_VERSION_base(4,8,0)
  , foldr, foldl
#endif
    )
import qualified Data.List as List
import Control.DeepSeq (NFData (..))
import Data.Foldable (Foldable (..), foldrM)
--
#if MIN_VERSION_base(4,5,0) && !MIN_VERSION_base(4,9,0)
import Data.Monoid ((<>))
#endif
--
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down (..))
#endif
--
#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts as Exts
#endif
--
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
#endif
--
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup (..))
#endif
--
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

-- | Type of sorted lists. Any (non-bottom) value of this type
--   is a sorted list. Use the 'Monoid' instance to merge sorted
--   lists.
newtype SortedList a = SortedList [a] deriving (Eq, Ord)

instance Show a => Show (SortedList a) where
  show = show . fromSortedList

instance NFData a => NFData (SortedList a) where
  {-# INLINE rnf #-}
  rnf (SortedList xs) = rnf xs

#if MIN_VERSION_base(4,7,0)
instance Ord a => Exts.IsList (SortedList a) where
  type (Item (SortedList a)) = a
  fromList = toSortedList
  toList = fromSortedList
#endif

#if !MIN_VERSION_base(4,8,0)
-- | Check if a sorted list is empty.
--
--   /This function dissappears in @base@ version 4.8.0.0 in favor of @null@/
--   /from "Data.Foldable"./
null :: SortedList a -> Bool
null = List.null . fromSortedList
#endif

-- | /O(1)/. Decompose a sorted list into its minimal element and the rest.
--   If the list is empty, it returns 'Nothing'.
uncons :: SortedList a -> Maybe (a, SortedList a)
uncons (SortedList []) = Nothing
uncons (SortedList (x:xs)) = Just (x, SortedList xs)

-- | Create a 'SortedList' by sorting a regular list.
toSortedList :: Ord a => [a] -> SortedList a
toSortedList = SortedList . List.sort

-- | /O(1)/. Create a list from a 'SortedList'. The returned list
--   is guaranteed to be sorted.
fromSortedList :: SortedList a -> [a]
fromSortedList (SortedList xs) = xs

-- | Merge two sorted lists. This assumes that both input lists
--   are sorted.
mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys) =
  if x <= y
     then x : mergeSortedLists xs (y:ys)
     else y : mergeSortedLists (x:xs) ys

#if MIN_VERSION_base(4,9,0)
instance Ord a => Semigroup (SortedList a) where
  SortedList xs <> SortedList ys = SortedList $ mergeSortedLists xs ys
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []
  mappend = (<>)
#else
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []
  mappend (SortedList xs) (SortedList ys) = SortedList $ mergeSortedLists xs ys
#endif

-- | /O(1)/. Create a sorted list with only one element.
singleton :: a -> SortedList a
singleton x = SortedList [x]

-- | An infinite list with all its elements equal to the given
--   argument.
repeat :: a -> SortedList a
repeat = SortedList . List.repeat

-- | Replicate a given number of times a single element.
replicate :: Int -> a -> SortedList a
replicate n = SortedList . List.replicate n

-- | Dual (sort of) to 'foldr' for sorted lists. It builds a sorted list from
--   a generator function and an initial element. The generator function is
--   applied to the initial element, and then it will produce either 'Nothing'
--   - meaning that the list building must stop - or 'Just' applied to the
--   value that is going to be added to the list, and a new accumulator to be fed
--   to the generator function. The list building will stop prematurely if the
--   generator function happens to create an element for the list that is strictly
--   smaller than the previous value.
unfoldr :: Ord a => (b -> Maybe (a,b)) -> b -> SortedList a
unfoldr f e = SortedList $
  let g (prev,acc) = do
        (curr,acc') <- f acc
        if prev <= curr
           then Just (curr, (curr, acc'))
           else Nothing
  in  case f e of
        Just (x0,e') -> x0 : List.unfoldr g (x0,e')
        _ -> []

-- | Create a sorted list by repeatedly applying the same
--   function to an element, until the image by that function
--   is stricly less than its argument. In other words:
--
-- > iterate f x = [x, f x, f (f x), ... ]
--
--   With the list ending whenever
--   @f (f (... (f (f x)) ...)) < f (... (f (f x)) ...)@.
--   If this never happens, the list will be infinite.
--
--   By definition:
--
-- > iterate f = unfoldr (\x -> Just (x, f x))
--
iterate :: Ord a => (a -> a) -> a -> SortedList a
iterate f = unfoldr $ \x -> Just (x, f x)

-- | /O(n)/. Insert a new element in a sorted list.
insert :: Ord a => a -> SortedList a -> SortedList a
#if MIN_VERSION_base(4,5,0)
insert x xs = singleton x <> xs
#else
insert x xs = mappend (singleton x) xs
#endif

-- | Delete the first occurrence of the given element.
delete :: Ord a => a -> SortedList a -> SortedList a
{-# INLINE delete #-}
delete a (SortedList l) = SortedList $ go l
  where
    go (x:xs) =
      case x `compare` a of
        LT -> x : go xs
        GT -> x : xs
        EQ -> xs
    go [] = []

-- | Delete /all/ occurrences of the given element.
deleteAll :: Ord a => a -> SortedList a -> SortedList a
deleteAll a (SortedList l) = SortedList $ go l
  where
    go (x:xs) =
      case x `compare` a of
        LT -> x : go xs
        GT -> x : xs
        EQ -> go xs
    go [] = []

-- | Extract the prefix with the given length from a sorted list.
take :: Int -> SortedList a -> SortedList a
take n = fst . splitAt n

-- | Drop the given number of elements from a sorted list, starting
--   from the smallest and following ascending order.
drop :: Int -> SortedList a -> SortedList a
drop n = snd . splitAt n

-- | Split a sorted list in two sublists, with the first one having
--   length equal to the given argument, except when the length of the
--   list is less than that.
splitAt :: Int -> SortedList a -> (SortedList a, SortedList a)
splitAt n (SortedList xs) =
  let (ys,zs) = List.splitAt n xs
  in  (SortedList ys, SortedList zs)

-- | /O(n)/. Divide a sorted list into two lists, one with all the elements
--   that satisfy the given predicate, and another list with the rest of
--   elements.
partition :: (a -> Bool) -> SortedList a -> (SortedList a, SortedList a)
partition f (SortedList xs) =
  let (ys,zs) = List.partition f xs
  in  (SortedList ys, SortedList zs)

-- | /O(n)/. Extract the elements of a list that satisfy the predicate.
filter :: (a -> Bool) -> SortedList a -> SortedList a
filter f = fst . partition f

-- | /O(n)/. Select only elements that are strictly less than the argument.
filterLT :: Ord a => a -> SortedList a -> SortedList a
filterLT a (SortedList l) = SortedList $ go l
  where
    go (x:xs) = if x < a then x : go xs else []
    go [] = []

-- | /O(n)/. Select only elements that are strictly greater than the argument.
filterGT :: Ord a => a -> SortedList a -> SortedList a
filterGT a (SortedList l) = SortedList $ go l
  where
    go (x:xs) = if a < x then x : xs else go xs
    go [] = []

-- | /O(n)/. Select only elements less or equal to the argument.
filterLE :: Ord a => a -> SortedList a -> SortedList a
filterLE a (SortedList l) = SortedList $ go l
  where
    go (x:xs) = if x <= a then x : go xs else []
    go [] = []

-- | /O(n)/. Select only elements greater or equal to the argument.
filterGE :: Ord a => a -> SortedList a -> SortedList a
filterGE a (SortedList l) = SortedList $ go l
  where
    go (x:xs) = if a <= x then x : xs else go xs
    go [] = []

-- | /O(n)/. An efficient implementation of 'elem', using the 'Ord'
--   instance of the elements in a sorted list. It only traverses
--   the whole list if the requested element is greater than all
--   the elements in the sorted list.
elemOrd :: Ord a => a -> SortedList a -> Bool
elemOrd a (SortedList l) = go l
    where
      go (x:xs) =
        case compare a x of
          GT -> go xs
          EQ -> True
          _  -> False
      go _ = False

-- | /O(n)/. Remove duplicate elements from a sorted list.
nub :: Eq a => SortedList a -> SortedList a
nub (SortedList l) = SortedList $ go l
  where
    go (x:y:xs) = if x == y then go (x:xs) else x : go (y:xs)
    go xs = xs

instance Foldable SortedList where
  {-# INLINE foldr #-}
  foldr f e (SortedList xs) = foldr f e xs
#if MIN_VERSION_base(4,8,0)
  {-# INLINE toList #-}
  toList = fromSortedList
  minimum (SortedList xs) =
    case xs of
      x : _ -> x
      _ -> error "SortedList.minimum: empty list"
  maximum (SortedList xs) =
    case xs of
      [] -> error "SortedList.maximum: empty list"
      _ -> last xs
#endif

-- | Map a function over all the elements of a sorted list.
--   Note that 'map' will hang if the argument is an infinite list.
--
--   Even though 'SortedList' can't be made an instance of 'Functor',
--   'map' /does/ hold the 'Functor' laws (for finite lists).
--   We can't however write an instance because of the 'Ord' instance requirement on the type of
--   the elements of the result list. Therefore, while 'SortedList'
--   is not a functor type in general, it is when restricted to elements of
--   orderable types (for finite lists).
--
--   The complexity range goes from /O(n)/ (if the function is monotonically increasing)
--   to /O(n²)/ (if the function is monotonically decreasing). These are the best
--   and worst case scenarios. We provide an alternative ('mapDec') where monotonically
--   decreasing functions are the best case scenario.
map :: Ord b => (a -> b) -> SortedList a -> SortedList b
{-# INLINE[1] map #-}
map f = foldr (insert . f) mempty

-- | Just like 'map', but favoring functions that are monotonically decreasing instead
--   of those that are monotonically increasing.
mapDec :: Ord b => (a -> b) -> SortedList a -> SortedList b
{-# INLINE[1] mapDec #-}
mapDec f = foldl (\xs x -> insert (f x) xs) mempty

{-# RULES
"SortedList:map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"SortedList:map/id"  forall xs.     map id xs = xs

"SortedList:mapDec/mapDec" forall f g xs. mapDec f (map g xs) = mapDec (f . g) xs
"SortedList:mapDec/map" forall f g xs. mapDec f (map g xs) = map (f . g) xs
"SortedList:map/mapDec" forall f g xs. map f (mapDec g xs) = map (f . g) xs
"SortedList:mapDec/id"  forall xs.     mapDec id xs = xs
  #-}

-- | Traverse a sorted list with a function that returns in a monad.
--   The same performance observations apply here as in 'map', as
--   changing the values may involve reordering the list.
--
--   @since 0.2.3.0
traverse :: (Monad m, Ord b) => (a -> m b) -> SortedList a -> m (SortedList b)
traverse f = foldrM (\x xs -> liftA2 insert (f x) $ pure xs) mempty

#if MIN_VERSION_base(4,6,0)

-- | /O(n)/. Reverse a sorted list. The result uses 'Down', thus it is a sorted
--   list as well. The following equality holds for any sorted list @xs@:
--
-- > map Down xs = reverse xs
--
--   /Only available from @base@ version 4.6.0.0./
reverse :: SortedList a -> SortedList (Down a)
{-# INLINE[2] reverse #-}
reverse = SortedList . List.reverse . fmap Down . fromSortedList

{-# RULES
"SortedList:map/Down" forall xs. map Down xs = reverse xs
  #-}

-- | /O(n)/. Reverse a sorted list with elements embedded in the 'Down' type.
--
--   /Only available from @base@ version 4.6.0.0./
reverseDown :: SortedList (Down a) -> SortedList a
{-# INLINE[2] reverseDown #-}
reverseDown = SortedList . List.reverse . fmap unDown . fromSortedList
  where
    unDown (Down a) = a

#endif

-- | Return the longest prefix of a sorted list of elements that satisfy the given condition,
--   and the rest of the list.
span :: (a -> Bool) -> SortedList a -> (SortedList a, SortedList a)
span f (SortedList xs) =
  let (ys,zs) = List.span f xs
  in  (SortedList ys, SortedList zs)

-- | Return the longest prefix of a sorted list of elements that satisfy the given condition.
takeWhile :: (a -> Bool) -> SortedList a -> SortedList a
takeWhile f = fst . span f

-- | Return the suffix remaining after dropping the longest prefix of elements that satisfy
--   the given condition.
dropWhile :: (a -> Bool) -> SortedList a -> SortedList a
dropWhile f = snd . span f

-- | /O(n)/. Return the indices of all elements in a sorted list that satisfy the given condition.
findIndices :: (a -> Bool) -> SortedList a -> SortedList Int
findIndices f (SortedList xs) = SortedList $ List.findIndices f xs

-- | /O(n+m)/. Intersection of sorted lists. If the first list contains duplicates, so will the result.
intersect :: Ord a => SortedList a -> SortedList a -> SortedList a
intersect xs ys =
  let SortedList xs' = xs
      SortedList ys' = nub ys
      go [] _  = []
      go _  [] = []
      go pp@(p:ps) qq@(q:qs) =
        case p `compare` q of
          LT ->     go ps qq
          EQ -> p : go ps qq
          GT ->     go pp qs
  in  SortedList $ go xs' ys'

-- | Union of sorted lists.
--   Duplicates, and elements of the first list, are removed from the the second list,
--   but if the first list contains duplicates, so will the result.
union :: Ord a => SortedList a -> SortedList a -> SortedList a
union xs ys = xs `mappend` foldl (flip delete) (nub ys) xs
