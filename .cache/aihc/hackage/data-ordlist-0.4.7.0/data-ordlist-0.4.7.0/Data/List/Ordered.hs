{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Ordered
-- Copyright   :  (c) 2009-2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements bag and set operations on ordered lists.  For the
-- purposes of this module,  a \"bag\" (or \"multiset\") is a non-decreasing
-- list, whereas a \"set\" is a strictly ascending list.  Bags are sorted
-- lists that may contain duplicates,  whereas sets are sorted lists that
-- do not contain duplicates.
--
-- Except for the  'nub', 'sort', 'nubSort', and 'isSorted' families of
-- functions, every function assumes that any list arguments are sorted
-- lists. Assuming this precondition is met,  every resulting list is also
-- sorted.
--
-- Because 'isect' handles multisets correctly, it does not return results
-- comparable to @Data.List.'Data.List.intersect'@ on them.  Thus @isect@
-- is more than just a more efficient @intersect@ on ordered lists. Similar
-- statements apply to other associations between functions this module and
-- functions in @Data.List@,  such as 'union' and @Data.List.'union'@.
--
-- All functions in this module are left biased.  Elements that appear in
-- earlier arguments have priority over equal elements that appear in later
-- arguments,  and elements that appear earlier in a single list have
-- priority over equal elements that appear later in that list.
--
-----------------------------------------------------------------------------

module  Data.List.Ordered
     (
        -- * Predicates
        member, memberBy, has, hasBy
     ,  subset, subsetBy
     ,  isSorted, isSortedBy

        -- * Insertion Functions
     ,  insertBag, insertBagBy
     ,  insertSet, insertSetBy

        -- * Set-like operations
     ,  isect, isectBy
     ,  union, unionBy
     ,  minus, minusBy
     ,  minus', minusBy'
     ,  xunion, xunionBy
     ,  merge, mergeBy
     ,  mergeAll, mergeAllBy
     ,  unionAll, unionAllBy

        -- * Lists to Ordered Lists
     ,  nub, nubBy
     ,  sort, sortBy
     ,  sortOn, sortOn'
     ,  nubSort, nubSortBy
     ,  nubSortOn, nubSortOn'

        -- * Miscellaneous folds
     ,  foldt, foldt'

     )  where

import Data.List(sort,sortBy,intersect)
#if  MIN_VERSION_base(4,7,1)
import Data.List(sortOn)
#endif

-- |  The 'isSorted' predicate returns 'True' if the elements of a list occur
-- in non-descending order,  equivalent to @'isSortedBy' ('<=')@.
isSorted :: Ord a => [a] -> Bool
isSorted = isSortedBy (<=)

-- |  The 'isSortedBy' function returns 'True' iff the predicate returns true
-- for all adjacent pairs of elements in the list.
isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy lte = loop
  where
    loop []       = True
    loop [_]      = True
    loop (x:y:zs) = (x `lte` y) && loop (y:zs)

-- |  The 'member' function returns 'True' if the element appears in the
-- ordered list.
member :: Ord a => a -> [a] -> Bool
member = memberBy compare

-- |  The 'memberBy' function is the non-overloaded version of 'member'.
memberBy :: (a -> a -> Ordering) -> a -> [a] -> Bool
memberBy cmp x = loop
  where
    loop []     = False
    loop (y:ys) = case cmp x y of
                    LT -> False
                    EQ -> True
                    GT -> loop ys

-- |  The 'has' function returns 'True' if the element appears in the list;
-- it is equivalent to 'member' except the order of the arguments is reversed,
-- making it a function from an ordered list to its characteristic function.
has :: Ord a => [a] -> a -> Bool
has xs y = memberBy compare y xs

-- |  The 'hasBy' function is the non-overloaded version of 'has'.
hasBy :: (a -> a -> Ordering) -> [a] -> a -> Bool
hasBy cmp xs y = memberBy cmp y xs

-- |  The 'insertBag' function inserts an element into a list.  If the element
-- is already there,  then another copy of the element is inserted.
insertBag :: Ord a => a -> [a] -> [a]
insertBag = insertBagBy compare

-- |  The 'insertBagBy' function is the non-overloaded version of 'insertBag'.
insertBagBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBagBy cmp = loop
  where
    loop x [] = [x]
    loop x (y:ys)
      = case cmp x y of
         GT -> y:loop x ys
         _  -> x:y:ys

-- |  The 'insertSet' function inserts an element into an ordered list.
-- If the element is already there,  then the element replaces the existing
-- element.
insertSet :: Ord a => a -> [a] -> [a]
insertSet = insertSetBy compare

-- |  The 'insertSetBy' function is the non-overloaded version of 'insertSet'.
insertSetBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertSetBy cmp = loop
  where
    loop x [] = [x]
    loop x (y:ys) = case cmp x y of
            LT -> x:y:ys
            EQ -> x:ys
            GT -> y:loop x ys

{-
-- This function is moderately interesting,  as it encompasses all the
-- "Venn diagram" functions on two sets. (though not merge;  which isn't
-- a set function)

-- However, it doesn't seem that useful,  considering that of the 8 possible
-- functions,  there are only 4 interesting variations:  isect, union, minus,
-- and xunion.  Due to interactions with GHC's optimizer,  coded separately,
-- these have a smaller combined object code size than the object code size
-- for genSectBy.  (Or,  turn off certain optimizations and lose speed.)

-- Each individual object code can be recovered from genSectBy via GHC's
-- inliner and constant propagation;  but this doesn't save much in terms
-- of source code size and reduces portability.

-- Note that the Static Argument Transformation is necessary for this to work
-- correctly;  inlining genSectBy allows for cmp and p to be inlined as well,
-- or at least eliminate some indirect jumps.  All of the *By functions in
-- this module follow this idiom for this reason.

genSectBy :: (a -> a -> Ordering)
          -> (Bool -> Bool -> Bool)
          -> [a] -> [a] -> [a]
genSectBy cmp p = loop
  where
    loop [] ys | p False True = ys
               | otherwise    = []
    loop xs [] | p True False = xs
               | otherwise    = []
    loop (x:xs) (y:ys)
      = case cmp x y of
          LT | p True False -> x : loop xs (y:ys)
             | otherwise    ->     loop xs (y:ys)
          EQ | p True True  -> x : loop xs ys
             | otherwise    ->     loop xs ys
          GT | p False True -> y : loop (x:xs) ys
             | otherwise    ->     loop (x:xs) ys

-- Here's another variation that was suggested to me.  It is more general
-- than genSectBy, as it can implement a merge; but it cannot implement
-- a left-biased merge

foldrMergeBy :: (a -> b -> Ordering)
             -> (a -> c -> c) -> (b -> c -> c) -> (a -> b -> c -> c) -> c
             -> [a] -> [b] -> c
foldrMergeBy cmp addA addB unify z = loop
  where
    loop xs [] = foldr addA z xs
    loop [] ys = foldr addB z ys
    loop (x:xs) (y:ys)
      = case cmp x y of
          LT -> x `addA` loop  xs (y:ys)
          EQ -> unify x y (loop xs ys)
          GT -> y `addB` loop (x:xs) ys
-}

-- |  The 'isect' function computes the intersection of two ordered lists.
-- An element occurs in the output as many times as the minimum number of
-- occurrences in either input.  If either input is a set,  then the output
-- is a set.
--
-- > isect [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 3,4 ]
-- > isect [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1, 2,2 ]
isect :: Ord a => [a] -> [a] -> [a]
isect = isectBy compare

-- |  The 'isectBy' function is the non-overloaded version of 'isect'.
isectBy :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
isectBy cmp = loop
  where
     loop [] _ys  = []
     loop _xs []  = []
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT ->     loop xs (y:ys)
          EQ -> x : loop xs ys
          GT ->     loop (x:xs) ys

-- |  The 'union' function computes the union of two ordered lists.
-- An element occurs in the output as many times as the maximum number
-- of occurrences in either input.  The output is a set if and only if
-- both inputs are sets.
--
-- > union [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2, 3,4, 5,6 ]
-- > union [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1,1, 2,2,2 ]
union :: Ord a => [a] -> [a] -> [a]
union = unionBy compare

-- |  The 'unionBy' function is the non-overloaded version of 'union'.
unionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unionBy cmp = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys

-- |  The 'minus' function computes the difference of two ordered lists.
-- An element occurs in the output as many times as it occurs in
-- the first input, minus the number of occurrences in the second input.
-- If the first input is a set,  then the output is a set.
--
-- > minus [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2 ]
-- > minus [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 2 ]
minus :: Ord a => [a] -> [a] -> [a]
minus = minusBy compare

-- |  The 'minusBy' function is the non-overloaded version of 'minus'.
minusBy :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
minusBy cmp = loop
  where
     loop [] _ys = []
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT ->     loop (x:xs) ys

-- |  The 'minus'' function computes the difference of two ordered lists.
-- The result consists of elements from the first list that do not appear
-- in the second list.  If the first input is a set, then the output is
-- a set.
--
-- > minus' [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2 ]
-- > minus' [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == []
-- > minus' [ 1,1, 2,2 ] [ 2 ]          == [ 1,1 ]
minus' :: Ord a => [a] -> [a] -> [a]
minus' = minusBy' compare

-- |  The 'minusBy'' function is the non-overloaded version of 'minus''.
minusBy' :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
minusBy' cmp = loop
  where
     loop [] _ys = []
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs (y:ys)
          GT ->     loop (x:xs) ys

-- |  The 'xunion' function computes the exclusive union of two ordered lists.
-- An element occurs in the output as many times as the absolute difference
-- between the number of occurrences in the inputs.  If both inputs
-- are sets,  then the output is a set.
--
-- > xunion [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2, 5,6 ]
-- > xunion [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1, 2 ]
xunion :: Ord a => [a] -> [a] -> [a]
xunion = xunionBy compare

-- |  The 'xunionBy' function is the non-overloaded version of 'xunion'.
xunionBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
xunionBy cmp = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT -> y : loop (x:xs) ys

-- |  The 'merge' function combines all elements of two ordered lists.
-- An element occurs in the output as many times as the sum of the
-- occurrences in both lists.   The output is a set if and only if
-- the inputs are disjoint sets.
--
-- > merge [ 1,2, 3,4 ] [ 3,4, 5,6 ]   == [ 1,2,  3,3,4,4,  5,6 ]
-- > merge [ 1, 2,2,2 ] [ 1,1,1, 2,2 ] == [ 1,1,1,1,  2,2,2,2,2 ]
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- |  The 'mergeBy' function is the non-overloaded version of 'merge'.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = loop
  where
    loop [] ys  = ys
    loop xs []  = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
         GT -> y : loop (x:xs) ys
         _  -> x : loop xs (y:ys)

-- |  The 'subset' function returns true if the first list is a sub-list
-- of the second.
subset :: Ord a => [a] -> [a] -> Bool
subset = subsetBy compare

-- |  The 'subsetBy' function is the non-overloaded version of 'subset'.
subsetBy :: (a -> a -> Ordering) -> [a] -> [a] -> Bool
subsetBy cmp = loop
  where
    loop [] _ys = True
    loop _xs [] = False
    loop (x:xs) (y:ys)
      = case cmp x y of
         LT -> False
         EQ -> loop xs ys
         GT -> loop (x:xs) ys

{-
-- This is Ian Lynagh's mergesort implementation,  which appeared as
-- Data.List.sort, with the static argument transformation applied.
-- It's not clear whether this modification is truly worthwhile or not.

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldt (mergeBy cmp) [] . map (\x -> [x])
-}

#if !MIN_VERSION_base(4,7,1)
-- |  The 'sortOn' function provides the decorate-sort-undecorate idiom,
-- also known as the \"Schwartzian transform\".
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> let y = f x in y `seq` (y, x))
#endif

-- |  This variant of 'sortOn' recomputes the sorting key every comparison.
-- This can be better for functions that are cheap to compute.
-- This is definitely better for projections,  as the decorate-sort-undecorate
-- saves nothing and adds two traversals of the list and extra memory
-- allocation.
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))

-- |  The 'nubSort' function is equivalent to @'nub' '.' 'sort'@,  except
-- that duplicates are removed as it sorts. It is essentially the same
-- implementation as @Data.List.sort@, with 'merge' replaced by 'union'.
-- Thus the performance of 'nubSort' should better than or nearly equal
-- to 'sort' alone.  It is faster than both 'sort' and @'nub' '.' 'sort'@
-- when the input contains significant quantities of duplicated elements.
nubSort :: Ord a => [a] -> [a]
nubSort = nubSortBy compare

-- |  The 'nubSortBy' function is the non-overloaded version of 'nubSort'.
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = foldt' (unionBy cmp) [] . runs
  where
    -- 'runs' partitions the input into sublists that are monotonic,
    -- contiguous,  and non-overlapping.   Descending runs are reversed
    -- and adjacent duplicates are eliminated,  so every run returned is
    -- strictly ascending.

    runs (a:b:xs)
      = case cmp a b of
          LT -> asc b (a:) xs
          EQ -> runs (a:xs)
          GT -> desc b [a] xs
    runs xs = [xs]

    desc a as []  = [a:as]
    desc a as (b:bs)
      = case cmp a b of
          LT -> (a:as) : runs (b:bs)
          EQ -> desc a as bs
          GT -> desc b (a:as) bs

    asc a as [] = [as [a]]
    asc a as (b:bs)
      = case cmp a b of
         LT -> asc b (\ys -> as (a:ys)) bs
         EQ -> asc a as bs
         GT -> as [a] : runs (b:bs)

-- |  The 'nubSortOn' function provides decorate-sort-undecorate for 'nubSort'.
nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn f = map snd . nubSortOn' fst . map (\x -> let y = f x in y `seq` (y, x))

-- |  This variant of 'nubSortOn' recomputes the sorting key for each comparison
nubSortOn' :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn' f = nubSortBy (\x y -> compare (f x) (f y))

-- | On ordered lists,  'nub' is equivalent to 'Data.List.nub', except that
-- it runs in linear time instead of quadratic.   On unordered lists it also
-- removes elements that are smaller than any preceding element.
--
-- > nub [1,1,1,2,2] == [1,2]
-- > nub [2,0,1,3,3] == [2,3]
-- > nub = nubBy (<)
nub :: Ord a => [a] -> [a]
nub = nubBy (<)

-- | The 'nubBy' function is the greedy algorithm that returns a
-- sublist of its input such that:
--
-- > isSortedBy pred (nubBy pred xs) == True
--
-- This is true for all lists,  not just ordered lists,  and all binary
-- predicates,  not just total orders.   On infinite lists,  this statement
-- is true in a certain mathematical sense,  but not a computational one.
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy p []     = []
nubBy p (x:xs) = x : loop x xs
  where
    loop _ [] = []
    loop x (y:ys)
       | p x y     = y : loop y ys
       | otherwise = loop x ys

-- | The function @'foldt'' plus zero@ computes the sum of a list
-- using a balanced tree of operations.  'foldt'' necessarily diverges
-- on infinite lists, hence it is a stricter variant of 'foldt'.
-- 'foldt'' is used in the implementation of 'sort' and 'nubSort'.
foldt' :: (a -> a -> a) -> a -> [a] -> a
foldt' plus zero xs
  = case xs of
      []    -> zero
      (_:_) -> loop xs
  where
    loop [x] = x
    loop xs  = loop (pairs xs)

    pairs (x:y:zs) = plus x y : pairs zs
    pairs zs       = zs

-- | The function @'foldt' plus zero@ computes the sum of a list using
-- a sequence of balanced trees of operations.   Given an appropriate @plus@
-- operator,  this function can be productive on an infinite list, hence it
-- is lazier than 'foldt''.   'foldt' is used in the implementation of
-- 'mergeAll' and 'unionAll'.
foldt :: (a -> a -> a) -> a -> [a] -> a
foldt plus zero = loop
  where
    loop []     = zero
    loop (x:xs) = x `plus` loop (pairs xs)

    pairs (x:y:zs) = plus x y : pairs zs
    pairs zs       = zs

-- helper functions used in 'mergeAll' and 'unionAll'

data People a = VIP a (People a) | Crowd [a]

serve (VIP x xs) = x:serve xs
serve (Crowd xs) = xs

vips xss = [ VIP x (Crowd xs) | (x:xs) <- xss ]

-- | The 'mergeAll' function merges a (potentially) infinite number of
-- ordered lists, under the assumption that the heads of the inner lists
-- are sorted.  An element is duplicated in the result as many times as
-- the total number of occurrences in all inner lists.
--
-- The 'mergeAll' function is closely related to @'foldr' 'merge' []@.
-- The former does not assume that the outer list is finite, whereas
-- the latter does not assume that the heads of the inner lists are sorted.
-- When both sets of assumptions are met,  these two functions are
-- equivalent.
--
-- This implementation of 'mergeAll'  uses a tree of comparisons, and is
-- based on input from Dave Bayer, Heinrich Apfelmus, Omar Antolin Camarena,
-- and Will Ness.  See @CHANGES@ for details.
mergeAll :: Ord a => [[a]] -> [a]
mergeAll = mergeAllBy compare

-- | The 'mergeAllBy' function is the non-overloaded variant of the 'mergeAll'
-- function.
mergeAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy cmp = serve . foldt merge' (Crowd []) . vips
  where
    merge' (VIP x xs) ys = VIP x (merge' xs ys)
    merge' (Crowd []) ys = ys
    merge' (Crowd xs) (Crowd ys) = Crowd (mergeBy cmp xs ys)
    merge' xs@(Crowd (x:xt)) ys@(VIP y yt)
      = case cmp x y of
         GT -> VIP y (merge' xs yt)
         _  -> VIP x (merge' (Crowd xt) ys)

-- | The 'unionAll' computes the union of a (potentially) infinite number
-- of lists,  under the assumption that the heads of the inner lists
-- are sorted.  The result will duplicate an element as many times as
-- the maximum number of occurrences in any single list.  Thus, the result
-- is a set if and only if every inner list is a set.
--
-- The 'unionAll' function is closely related to @'foldr' 'union' []@.
-- The former does not assume that the outer list is finite, whereas
-- the latter does not assume that the heads of the inner lists are sorted.
-- When both sets of assumptions are met,  these two functions are
-- equivalent.
--
-- Note that there is no simple way to express 'unionAll' in terms of
-- 'mergeAll' or vice versa on arbitrary valid inputs.  They are related
-- via 'nub' however,  as @'nub' . 'mergeAll' == 'unionAll' . 'map' 'nub'@.
-- If every list is a set,  then @map nub == id@,  and in this special case
-- (and only in this special case) does @nub . mergeAll == unionAll@.
--
-- This implementation of 'unionAll'  uses a tree of comparisons, and is
-- based on input from Dave Bayer, Heinrich Apfelmus, Omar Antolin Camarena,
-- and Will Ness.  See @CHANGES@ for details.
unionAll :: Ord a => [[a]] -> [a]
unionAll = unionAllBy compare

-- | The 'unionAllBy' function is the non-overloaded variant of the 'unionAll'
-- function.
unionAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
unionAllBy cmp = serve . foldt union' (Crowd []) . vips
  where
    msg = "Data.List.Ordered.unionAllBy:  the heads of the lists are not sorted"

    union' (VIP x xs) ys
       = VIP x $ case ys of
                  Crowd _ -> union' xs ys
                  VIP y yt -> case cmp x y of
                               LT -> union' xs ys
                               EQ -> union' xs yt
                               GT -> error msg
    union' (Crowd []) ys = ys
    union' (Crowd xs) (Crowd ys) = Crowd (unionBy cmp xs ys)
    union' xs@(Crowd (x:xt)) ys@(VIP y yt)
       = case cmp x y of
           LT -> VIP x (union' (Crowd xt) ys)
           EQ -> VIP x (union' (Crowd xt) yt)
           GT -> VIP y (union' xs yt)
