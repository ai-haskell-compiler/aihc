{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsArrMini1
Copyright   :  (c) OleksandrZhabenko 2022-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Special permutations functions for the phonetic-languages and phladiprelio series of packages. This
module uses no vectors, but instead uses arrays.
-}
module Aftovolio.PermutationsArrMini1 (
    genElementaryPermutations1,
    pairsSwapP1,
    genElementaryPermutationsArrN1,
    genElementaryPermutationsArr1,
    genElementaryPermutationsLN1,
    genElementaryPermutationsL1,
    genElementaryPermutationsArrLN1,
    genElementaryPermutationsArrL1,
) where

import GHC.Arr
import GHC.Base
import GHC.Enum
import GHC.List
import GHC.Num (Num, abs, (+), (-))

genElementaryPermutations1 :: (Ord a, Enum a, Num a) => Int -> Array Int [a]
genElementaryPermutations1 n = listArray (0, l - 1) xs
  where
    xs = pairsSwapP1 . take n $ [0 ..]
    l = length xs
{-# INLINE genElementaryPermutations1 #-}
{-# SPECIALIZE genElementaryPermutations1 :: Int -> Array Int [Int] #-}

pairsSwapP1 :: (Ord a, Num a) => [a] -> [[a]]
pairsSwapP1 xs =
    xs
        : [swap2Ls1 k m xs | k <- xs, m <- xs, abs (k - m) > 1]
            `mappend` [swap2Ls1 k (k - 1) xs | k <- drop 1 xs]
{-# SPECIALIZE pairsSwapP1 :: [Int] -> [[Int]] #-}

-- | The first two arguments are considered not equal and all three of the arguments are considered greater or equal to 0, though it is not checked.
swap2ns1 :: (Ord a, Num a) => a -> a -> a -> a
swap2ns1 k n m
    | n > k =
        if
            | m < k -> m
            | m > n -> m
            | m < n -> m + 1
            | otherwise -> k
    | otherwise =
        if
            | m > k -> m
            | m < n -> m
            | m > n -> m - 1
            | otherwise -> k
{-# INLINE swap2ns1 #-}
{-# SPECIALIZE swap2ns1 :: Int -> Int -> Int -> Int #-}

swap2Ls1 :: (Ord a, Num a) => a -> a -> [a] -> [a]
swap2Ls1 k m = map (swap2ns1 k m)
{-# INLINE swap2Ls1 #-}
{-# SPECIALIZE swap2Ls1 :: Int -> Int -> [Int] -> [Int] #-}

genElementaryPermutationsArrN1 ::
    (Ord a, Enum a, Num a) => Int -> Array Int (Array Int [a])
genElementaryPermutationsArrN1 n = amap genElementaryPermutations1 . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genElementaryPermutationsArrN1 #-}
{-# SPECIALIZE genElementaryPermutationsArrN1 ::
    Int -> Array Int (Array Int [Int])
    #-}

genElementaryPermutationsArr1 ::
    (Ord a, Enum a, Num a) => Array Int (Array Int [a])
genElementaryPermutationsArr1 = genElementaryPermutationsArrN1 10
{-# INLINE genElementaryPermutationsArr1 #-}
{-# SPECIALIZE genElementaryPermutationsArr1 :: Array Int (Array Int [Int]) #-}

genElementaryPermutationsLN1 :: (Ord a, Enum a, Num a) => Int -> [Array Int a]
genElementaryPermutationsLN1 n = map (\xs -> listArray (0, n - 1) xs) . pairsSwapP1 . take n $ [0 ..]
{-# INLINE genElementaryPermutationsLN1 #-}
{-# SPECIALIZE genElementaryPermutationsLN1 :: Int -> [Array Int Int] #-}

genElementaryPermutationsL1 :: (Ord a, Enum a, Num a) => [Array Int a]
genElementaryPermutationsL1 = genElementaryPermutationsLN1 10
{-# INLINE genElementaryPermutationsL1 #-}
{-# SPECIALIZE genElementaryPermutationsL1 :: [Array Int Int] #-}

genElementaryPermutationsArrLN1 ::
    (Ord a, Enum a, Num a) => Int -> Array Int [Array Int a]
genElementaryPermutationsArrLN1 n = amap genElementaryPermutationsLN1 . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genElementaryPermutationsArrLN1 #-}
{-# SPECIALIZE genElementaryPermutationsArrLN1 ::
    Int -> Array Int [Array Int Int]
    #-}

genElementaryPermutationsArrL1 ::
    (Ord a, Enum a, Num a) => Array Int [Array Int a]
genElementaryPermutationsArrL1 = genElementaryPermutationsArrLN1 10
{-# INLINE genElementaryPermutationsArrL1 #-}
{-# SPECIALIZE genElementaryPermutationsArrL1 :: Array Int [Array Int Int] #-}
