{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.UniquenessPeriodsG
Copyright   :  (c) OleksandrZhabenko 2020-2023
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Generalization of the uniqueness-periods and uniqueness-periods-general
packages functionality for small 'F.Foldable' data structures. Uses less dependencies.
-}
module Aftovolio.UniquenessPeriodsG (
    -- * List functions
    uniquenessPeriodsGG,
    uniquenessPeriodsG,
    uniquenessPeriodsGI8,
    diverse2GGL,
    diverse2GL,
    diverse2GLInt8,
) where

import qualified Data.Foldable as F
import Data.List hiding (foldr)
import Data.Maybe (mapMaybe)
import Data.Tuple
import GHC.Base
import GHC.Int
import GHC.Num ((-))

-- | A generalization of the uniquenessPeriods function of the @uniqueness-periods@ package.
uniquenessPeriodsGG ::
    (F.Foldable t1, F.Foldable t2, F.Foldable t3, Ord a) =>
    t3 a ->
    t1 a ->
    t2 a ->
    [Int16]
uniquenessPeriodsGG sels whspss ws
    | F.null ws = []
    | otherwise = mapMaybe (helpGSel sels F.sum whspss) . unfoldr f $ ks
  where
    !ks = indexedL ws
    !vs = mapMaybe g ks
    g (x, y)
        | y `F.elem` whspss = Just x
        | otherwise = Nothing
    {-# INLINE g #-}
    f ys@(y : _) =
        let !idX0 = snd y
         in Just
                . ( \(v2, v3) ->
                        (
                            ( helpUPV3 vs []
                                . map fst
                                $ v2
                            , snd . head $ v2
                            )
                        , v3
                        )
                  )
                . partition (\(_, xs) -> xs == idX0)
                $ ys
    f _ = Nothing
{-# INLINE uniquenessPeriodsGG #-}

-- | A general variant of the diversity property. Use it in general case.
diverse2GGL ::
    (F.Foldable t1, F.Foldable t2, F.Foldable t3, Ord a) =>
    t3 a ->
    t1 a ->
    t2 a ->
    Int16
diverse2GGL sels whspss = sum . uniquenessPeriodsGG sels whspss
{-# INLINE diverse2GGL #-}

-- | A variant of the 'diverse2GGL' function for 'Char'.
diverse2GL :: (F.Foldable t1, F.Foldable t2) => t1 Char -> t2 Char -> Int16
diverse2GL = diverse2GGL []
{-# INLINE diverse2GL #-}

--

-- | A variant for the 'uniquenessPeriodsGG' function for 'Char'.
uniquenessPeriodsG ::
    (F.Foldable t1, F.Foldable t2) => t1 Char -> t2 Char -> [Int16]
uniquenessPeriodsG = uniquenessPeriodsGG []
{-# INLINE uniquenessPeriodsG #-}

-- | A variant of the 'diverse2GGL' function for 'Int8'.
diverse2GLInt8 :: (F.Foldable t1, F.Foldable t2) => t1 Int8 -> t2 Int8 -> Int16
diverse2GLInt8 = diverse2GGL []
{-# INLINE diverse2GLInt8 #-}

-- | A variant for the 'uniquenessPeriodsGG' function for 'Int8'.
uniquenessPeriodsGI8 ::
    (F.Foldable t1, F.Foldable t2) => t1 Int8 -> t2 Int8 -> [Int16]
uniquenessPeriodsGI8 = uniquenessPeriodsGG []
{-# INLINE uniquenessPeriodsGI8 #-}

-- | The first and the third list arguments of numbers (if not empty) must be sorted in the ascending order.
helpUPV3 :: [Int16] -> [Int16] -> [Int16] -> [Int16]
helpUPV3 ks@(!z : zs) !acc ps@(!x : qs@(!y : _))
    | z < y && z > x = helpUPV3 zs ((y - x) : acc) qs
    | z < y = helpUPV3 zs acc ps
    | otherwise = helpUPV3 ks acc qs
helpUPV3 _ !acc _ = acc

indexedL :: (F.Foldable t) => t b -> [(Int16, b)]
indexedL = F.foldr f []
  where
    f x ((j, z) : ys) = (j - 1, x) : (j, z) : ys
    f x _ = [(1, x)]

helpG ::
    (Eq a, F.Foldable t1, F.Foldable t2) =>
    (t1 b -> b) ->
    t2 a ->
    (t1 b, a) ->
    Maybe b
helpG h xs (ts, x)
    | F.null ts = Nothing
    | x `F.elem` xs = Nothing
    | otherwise = Just (h ts)
{-# INLINE helpG #-}

helpGSel ::
    (Eq a, F.Foldable t1, F.Foldable t2, F.Foldable t3) =>
    t3 a ->
    (t1 b -> b) ->
    t2 a ->
    (t1 b, a) ->
    Maybe b
helpGSel sels h xs (ts, x)
    | F.null sels = helpG h xs (ts, x)
    | F.null ts = Nothing
    | x `F.elem` xs = Nothing
    | x `F.elem` sels = Just (h ts)
    | otherwise = Nothing
{-# INLINE helpGSel #-}
