{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsArrMini2
Copyright   :  (c) OleksandrZhabenko 2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Special permutations functions for the AFTOVolio. This
module uses no vectors, but instead uses arrays.
-}
module Aftovolio.PermutationsArrMini2 (
    genDoublePermutations2,
    pairsSwapP2,
    genDoublePermutationsArrN2,
    genDoublePermutationsArr2,
    genDoublePermutationsLN2,
    genDoublePermutationsL2,
    genDoublePermutationsArrLN2,
    genDoublePermutationsArrL2,
) where

import GHC.Arr
import GHC.Base
import GHC.Enum
import GHC.List
import Data.List ((\\),nub)
import GHC.Num (Num, abs, (+), (-), (*))

genDoublePermutations2 :: Int -> Array Int [Int]
genDoublePermutations2 n = listArray (0, l - 1) xs
  where
    xs = pairsSwapP2 . take n $ [0 ..]
    l = length xs
{-# INLINE genDoublePermutations2 #-}
--{-# SPECIALIZE genDoublePermutations2 :: Int -> Array Int [Int] #-}

pairsSwapP2 :: [Int] -> [[Int]]
pairsSwapP2 xs = nub $
    xs
        : cuts
              where l = length xs
                    indices = [(i,j) | i <- xs, j <- xs, j /= i]
                    cuts = concatMap (\(i, j) -> 
                               let idxs = filter (\x -> x /= i && x /= j) xs
                                   in map (\(i', j') -> 
                                          let (mn, mx) = if j' > i' then (i', j') else (j', i')
                                              (ks, us) = splitAt mn idxs 
                                              idx1 = ks ++ (i : us) 
                                              (rs, qs) = splitAt mx idx1
                                              in rs ++ (j : qs)) . filter (/= (i, j)) $ indices) indices
--{-# SPECIALIZE pairsSwapP2 :: [Int] -> [[Int]] #-}

-- | The second argument is greater than the first, the third is not equal to the first, the fourth is not equal to the second, and all five of the arguments are considered greater or equal to 0, though it is not checked.
swap2ns2 :: (Ord a, Num a) => a -> a -> a -> a -> a -> a
swap2ns2 k n m j i
    | q > 0 && p > 0 = i
    | i == j = n
    | i == m = k --
    | q > 0 && p < 0 = if j > n then i + 1 else i - 1
    | q < 0 && p > 0 = if m > k then i + 1 else i - 1
    | q < 0 && p < 0 = if (k - m)*(n - j) > 0 
                           then if m > k then i + 2 else i - 2
                           else i
    | i == n = if m == n then k else if j > n then n + 1 else n - 1
    | i == k = if j == k then n else if m > k then k + 1 else k - 1
       where  q = (i - k)*(i - m)
              p = (i - n)*(i - j)
{-# INLINE swap2ns2 #-}
{-# SPECIALIZE swap2ns2 :: Int -> Int -> Int -> Int -> Int -> Int #-}

swap2Ls2 :: (Ord a, Num a) => a -> a -> a -> a -> [a] -> [a]
swap2Ls2 k n m j = map (swap2ns2 k n m j)
{-# INLINE swap2Ls2 #-}
{-# SPECIALIZE swap2Ls2 :: Int -> Int -> Int -> Int -> [Int] -> [Int] #-}

genDoublePermutationsArrN2 ::
    Int -> Array Int (Array Int [Int])
genDoublePermutationsArrN2 n = amap genDoublePermutations2 . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genDoublePermutationsArrN2 #-}
--{-# SPECIALIZE genDoublePermutationsArrN2 ::
--    Int -> Array Int (Array Int [Int])
--    #-}

genDoublePermutationsArr2 ::
    Array Int (Array Int [Int])
genDoublePermutationsArr2 = genDoublePermutationsArrN2 10
{-# INLINE genDoublePermutationsArr2 #-}
--{-# SPECIALIZE genDoublePermutationsArr2 :: Array Int (Array Int [Int]) #-}

genDoublePermutationsLN2 :: Int -> [Array Int Int]
genDoublePermutationsLN2 n = map (\xs -> listArray (0, n - 1) xs) . pairsSwapP2 . take n $ [0 ..]
{-# INLINE genDoublePermutationsLN2 #-}
--{-# SPECIALIZE genDoublePermutationsLN2 :: Int -> [Array Int Int] #-}

genDoublePermutationsL2 :: [Array Int Int]
genDoublePermutationsL2 = genDoublePermutationsLN2 10
{-# INLINE genDoublePermutationsL2 #-}
--{-# SPECIALIZE genDoublePermutationsL2 :: [Array Int Int] #-}

genDoublePermutationsArrLN2 ::
    Int -> Array Int [Array Int Int]
genDoublePermutationsArrLN2 n = amap genDoublePermutationsLN2 . listArray (0, n - 2) $ [2 .. n]
{-# INLINE genDoublePermutationsArrLN2 #-}
--{-# SPECIALIZE genDoublePermutationsArrLN2 ::
--    Int -> Array Int [Array Int Int]
--    #-}

genDoublePermutationsArrL2 ::
    Array Int [Array Int Int]
genDoublePermutationsArrL2 = genDoublePermutationsArrLN2 10
{-# INLINE genDoublePermutationsArrL2 #-}
--{-# SPECIALIZE genDoublePermutationsArrL2 :: Array Int [Array Int Int] #-}
