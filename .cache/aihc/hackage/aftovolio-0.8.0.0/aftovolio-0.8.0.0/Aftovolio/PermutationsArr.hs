{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsArr
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Permutations and universal set functions for the phonetic-languages series and phladiprelio of packages
(AFTOVolio-related). This module uses no vectors, but instead uses arrays.
-}
module Aftovolio.PermutationsArr (
    universalSetGL,
    genPermutations,
    genPermutationsArr,
    genPermutationsL,
    genPermutationsArrL,
) where

import qualified Data.Foldable as F (Foldable, concat, foldl', foldr')
import Data.InsertLeft
import qualified Data.List as L (permutations, product)
import Data.Monoid
import GHC.Arr
import GHC.Base
import GHC.Enum
import GHC.List
import GHC.Num (Num, (*), (-))

-- | A key point of the evaluation -- the universal set of the task represented as a @[[a]]@.
universalSetGL ::
    (Eq a, F.Foldable t, InsertLeft t a, Monoid (t a), Monoid (t (t a))) =>
    t a ->
    t (t a) ->
    -- | The function that is used internally to convert to the @[a]@ so that the function can process further the permutations
    (t a -> [a]) ->
    -- | The function that is used internally to convert to the needed representation so that the function can process further
    ((t (t a)) -> [[a]]) ->
    -- | The list of permutations of 'Int' indices starting from 0 and up to n (n is probably less than 7).
    [Array Int Int] ->
    Array Int [a] ->
    [[a]]
universalSetGL ts uss f1 f2 permsL baseArr =
    map
        ( F.concat
            . F.foldr' (:) []
            . (f1 ts :)
            . (`mappend` f2 uss)
            . elems
            . amap (unsafeAt baseArr)
        )
        permsL
{-# INLINE universalSetGL #-}
{-# SPECIALIZE universalSetGL ::
    String ->
    [String] ->
    (String -> String) ->
    ([String] -> [String]) ->
    [Array Int Int] ->
    Array Int String ->
    [String]
    #-}

genPermutations :: (Ord a, Enum a, Num a) => Int -> Array Int [a]
genPermutations n = listArray (0, L.product [1 .. n] - 1) . L.permutations . take n $ [0 ..]
{-# INLINE genPermutations #-}
{-# SPECIALIZE genPermutations :: Int -> Array Int [Int] #-}

genPermutationsArr :: (Ord a, Enum a, Num a) => Array Int (Array Int [a])
genPermutationsArr = amap genPermutations . listArray (0, 5) $ [2 .. 7]
{-# INLINE genPermutationsArr #-}
{-# SPECIALIZE genPermutationsArr :: Array Int (Array Int [Int]) #-}

genPermutationsL :: (Ord a, Enum a, Num a) => Int -> [Array Int a]
genPermutationsL n = map (\xs -> listArray (0, n - 1) xs) . L.permutations . take n $ [0 ..]
{-# INLINE genPermutationsL #-}
{-# SPECIALIZE genPermutationsL :: Int -> [Array Int Int] #-}

genPermutationsArrL :: (Ord a, Enum a, Num a) => Array Int [Array Int a]
genPermutationsArrL = amap genPermutationsL . listArray (0, 5) $ [2 .. 7]
{-# INLINE genPermutationsArrL #-}
{-# SPECIALIZE genPermutationsArrL :: Array Int [Array Int Int] #-}
