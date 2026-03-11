{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsArrMini
Copyright   :  (c) OleksandrZhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Special permutations functions for the phonetic-languages and phladiprelio series of packages. This
module uses no vectors, but instead uses arrays.
-}
module Aftovolio.PermutationsArrMini (
    genPairwisePermutations,
    pairsSwapP,
    genPairwisePermutationsArrN,
    genPairwisePermutationsArr,
    genPairwisePermutationsLN,
    genPairwisePermutationsL,
    genPairwisePermutationsArrLN,
    genPairwisePermutationsArrL,
) where

import Data.Bits (shiftR)
import GHC.Arr
import GHC.Base
import GHC.Enum
import GHC.List
import GHC.Num

genPairwisePermutations :: (Ord a, Enum a, Num a) => Int -> Array Int [a]
genPairwisePermutations n = listArray (0, shiftR (n * (n - 1)) 1) . pairsSwapP . take n $ [0 ..]
{-# INLINE genPairwisePermutations #-}
{-# SPECIALIZE genPairwisePermutations :: Int -> Array Int [Int] #-}

pairsSwapP :: (Ord a, Enum a, Num a) => [a] -> [[a]]
pairsSwapP xs = xs : [swap2Ls k m xs | k <- xs, m <- xs, k < m]
{-# SPECIALIZE pairsSwapP :: [Int] -> [[Int]] #-}

-- | The first two arguments are considered not equal, though it is not checked.
swap2ns :: (Eq a) => a -> a -> a -> a
swap2ns k m n
    | n /= k = if n /= m then n else k
    | otherwise = m
{-# INLINE swap2ns #-}
{-# SPECIALIZE swap2ns :: Int -> Int -> Int -> Int #-}

swap2Ls :: (Eq a) => a -> a -> [a] -> [a]
swap2Ls k m = map (swap2ns k m)
{-# INLINE swap2Ls #-}
{-# SPECIALIZE swap2Ls :: Int -> Int -> [Int] -> [Int] #-}

genPairwisePermutationsArrN ::
    (Ord a, Enum a, Num a) => Int -> Array Int (Array Int [a])
genPairwisePermutationsArrN n = amap genPairwisePermutations . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genPairwisePermutationsArrN #-}
{-# SPECIALIZE genPairwisePermutationsArrN ::
    Int -> Array Int (Array Int [Int])
    #-}

genPairwisePermutationsArr ::
    (Ord a, Enum a, Num a) => Array Int (Array Int [a])
genPairwisePermutationsArr = genPairwisePermutationsArrN 10
{-# INLINE genPairwisePermutationsArr #-}
{-# SPECIALIZE genPairwisePermutationsArr :: Array Int (Array Int [Int]) #-}

genPairwisePermutationsLN :: (Ord a, Enum a, Num a) => Int -> [Array Int a]
genPairwisePermutationsLN n = map (\xs -> listArray (0, n - 1) xs) . pairsSwapP . take n $ [0 ..]
{-# INLINE genPairwisePermutationsLN #-}
{-# SPECIALIZE genPairwisePermutationsLN :: Int -> [Array Int Int] #-}

genPairwisePermutationsL :: (Ord a, Enum a, Num a) => [Array Int a]
genPairwisePermutationsL = genPairwisePermutationsLN 10
{-# INLINE genPairwisePermutationsL #-}
{-# SPECIALIZE genPairwisePermutationsL :: [Array Int Int] #-}

genPairwisePermutationsArrLN ::
    (Ord a, Enum a, Num a) => Int -> Array Int [Array Int a]
genPairwisePermutationsArrLN n = amap genPairwisePermutationsLN . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genPairwisePermutationsArrLN #-}
{-# SPECIALIZE genPairwisePermutationsArrLN ::
    Int -> Array Int [Array Int Int]
    #-}

genPairwisePermutationsArrL ::
    (Ord a, Enum a, Num a) => Array Int [Array Int a]
genPairwisePermutationsArrL = genPairwisePermutationsArrLN 10
{-# INLINE genPairwisePermutationsArrL #-}
{-# SPECIALIZE genPairwisePermutationsArrL :: Array Int [Array Int Int] #-}
