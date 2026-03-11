{-| Module      : Data.List.Predicate
    Description : Predicates on lists.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Predicates (@True@/@False@ queries) on lists.

The functions in this module are as lazy as possible. For example,
@'sortedBy' undefined [undefined] == True@, since a list of one element must be
sorted, no matter the comparison function, or the value of the element.
-}
module Data.List.Predicate
  ( -- * All equal
    allEqual
  , allEqualBy

   -- * Sortedness
  , sorted
  , sortedBy

  -- * All unique
  , allUnique
  , allUniqueBy
  , allAdjUnique
  , allAdjUniqueBy

  -- * Sequential
  , ascSequential
  , descSequential

  -- * Miscellaneous
  , palindrome
  ) where

import Data.List (sort, sortBy)

{-| /O(n)./ Whether the elements are all equal.

    >>> allEqual [1..]
    False
    >>> allEqual [3, 3, 3, 3]
    True
    >>> allEqual []
    True
    >>> allEqual [1]
    True
-}
allEqual :: (Eq a) => [a] -> Bool
allEqual = allEqualBy (==)

{-| /O(n)./ Like 'allEqual', with a custom equality test.

    >>> allEqualBy ((==) `on` (`mod` 10)) [3, 13, 23]
    True
    >>> allEqualBy ((==) `on` (`mod` 10)) [3, 13, 24]
    False
-}
allEqualBy :: (a -> a -> Bool) -> [a] -> Bool
allEqualBy _  []       = True
allEqualBy eq (x : xs) = all (eq x) xs

{-| /O(n)./ Whether the elements are in sorted order.

    >>> sorted [1, 2, 3, 3]
    True
    >>> sorted [1, 2, 3, 2]
    False
    >>> sorted []
    True
    >>> sorted [1]
    True
-}
sorted :: (Ord a) => [a] -> Bool
sorted = sortedBy compare

{-| /O(n)./ Like 'sorted', with a custom comparison test.

    >>> sortedBy (comparing Down) [3, 2, 1]
    True
    >>> sortedBy (comparing Down) [3, 2, 1, 2]
    False
-}
sortedBy :: (a -> a -> Ordering) -> [a] -> Bool
sortedBy _   []  = True
sortedBy _   [_] = True
sortedBy cmp xs  = and $ zipWith (\a b -> cmp a b <= EQ) xs (tail xs)

{-| /O(n log(n))./ Whether the elements are all unique.

    >>> allUnique [1, 2, 5, 7]
    True
    >>> allUnique [1, 2, 5, 2]
    False
    >>> allUnique []
    True
    >>> allUnique [1]
    True
-}
allUnique :: (Ord a) => [a] -> Bool
allUnique = allAdjUnique . sort

{-| /O(n log(n))./ Like 'allUnique', with a custom comparison test.

    >>> allUniqueBy (comparing head) ["apple", "bow", "cat"]
    True
    >>> allUniqueBy (comparing head) ["apple", "bow", "ant"]
    False
-}
allUniqueBy :: (a -> a -> Ordering) -> [a] -> Bool
allUniqueBy cmp = allAdjUniqueBy eq . sortBy cmp
  where
    eq a b = cmp a b == EQ

{-| /O(n)./ Whether all adjacent pairs of elements are different. Useful for
    determining whether a sorted list is all unique.

    >>> allAdjUnique [1, 2, 3, 2]
    True
    >>> allAdjUnique [1, 2, 2, 3]
    False
    >>> allAdjUnique []
    True
    >>> allAdjUnique [1]
    True
-}
allAdjUnique :: (Eq a) => [a] -> Bool
allAdjUnique = allAdjUniqueBy (==)

{-| /O(n)./ Like 'allAdjUnique', with a custom equality test.

    >>> allAdjUniqueBy ((==) `on` head) ["apple", "bow", "cat", "ant"]
    True
    >>> allAdjUniqueBy ((==) `on` head) ["apple", "ant", "bow", "cat"]
    False
-}
allAdjUniqueBy :: (a -> a -> Bool) -> [a] -> Bool
allAdjUniqueBy eq xs = (not . or) $ zipWith eq xs (tail xs)

{-| /O(n)./ Whether the list is increasing sequentially (one-by-one).

    >>> ascSequential [1, 2, 3, 4, 5]
    True
    >>> ascSequential [1, 2, 3, 4, 8]
    False
    >>> ascSequential ([] :: [Int])
    True
    >>> ascSequential [1]
    True
-}
ascSequential :: (Enum a) => [a] -> Bool
ascSequential []  = True
ascSequential [_] = True
ascSequential xs  = and $ zipWith (==) xs' [head xs' ..]
  where
    xs' = map fromEnum xs

{-| /O(n)./ Whether the list is descending sequentially (one-by-one).

    >>> descSequential [5, 4, 3, 2, 1]
    True
    >>> descSequential [5, 4, 3, 3, 1]
    False
    >>> descSequential ([] :: [Int])
    True
    >>> descSequential [1]
    True
-}
descSequential :: (Enum a) => [a] -> Bool
descSequential []  = True
descSequential [_] = True
descSequential xs  = and $ zipWith (==) xs' [x, x - 1 ..]
  where
    xs' = map fromEnum xs
    x   = head xs'

{-| /O(n)./ Whether the input is a palindrome, i.e., the same forwards and
    backwards.

    >>> palindrome "rotor"
    True
    >>> palindrome "rover"
    False
    >>> palindrome ""
    True
    >>> palindrome "a"
    True
-}
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = and $ take (len `div` 2) $ zipWith (==) xs rev
  where
    (rev, len) = reverseLength xs

-- Get the reverse and the length of a list in one pass.
reverseLength :: [a] -> ([a], Int)
reverseLength = reverseLengthWith [] 0
  where
    -- Accumulate the reverse and the length.
    reverseLengthWith :: [a] -> Int -> [a] -> ([a], Int)
    reverseLengthWith ys n [] = (ys, n)
    reverseLengthWith ys n (x : xs) =
        let n' = n + 1
         in seq n' (reverseLengthWith (x : ys) n' xs)
