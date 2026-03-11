{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.SyllableWord8
Copyright   :  (c) Oleksandr Zhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

This module works with syllable segmentation in Ukrainian. Uses 'Word8' whenever possible.
Is inspired by the DobutokO.Sound.DIS5G6G module from @dobutokO2@ package.
See: 'https://hackage.haskell.org/package/dobutokO2-0.43.0.0/docs/DobutokO-Sound-DIS5G6G.html'.
The initial 'Word8' data are gotten from there.
-}
module Aftovolio.Ukrainian.SyllableWord8 where

import Aftovolio.Ukrainian.Melodics (Sound8)
import CaseBi.Arr
import Data.Foldable (foldl')
import GHC.Base
import GHC.Int
import GHC.Num ((+))
import GHC.Word

s0DuratD1 :: Sound8 -> Word8
s0DuratD1 =
    getBFstLSorted'
        5
        [ (1, 15)
        , (2, 10)
        , (3, 13)
        , (4, 12)
        , (5, 11)
        , (6, 10)
        , (7, 1)
        , (8, 6)
        , (9, 6)
        , (10, 7)
        , (11, 7)
        , (15, 6)
        , (16, 6)
        , (17, 8)
        , (18, 8)
        , (19, 7)
        , (20, 7)
        , (21, 8)
        , (22, 8)
        , (23, 5)
        , (24, 5)
        , (25, 6)
        , (26, 6)
        , (27, 6)
        , (28, 7)
        , (29, 7)
        , (30, 8)
        , (31, 8)
        , (32, 8)
        , (33, 8)
        , (34, 5)
        , (35, 5)
        , (36, 9)
        , (37, 9)
        , (38, 5)
        , (39, 6)
        , (40, 6)
        , (41, 7)
        , (42, 7)
        , (43, 6)
        , (44, 6)
        , (45, 4)
        , (46, 4)
        , (47, 15)
        , (48, 15)
        , (49, 8)
        , (50, 12)
        , (51, 12)
        , (52, 8)
        , (53, 8)
        , (54, 8)
        , (66, 10)
        ]
{-# INLINE s0DuratD1 #-}

s0DuratD2 :: Sound8 -> Word8
s0DuratD2 =
    getBFstLSorted'
        5
        [ (1, 15)
        , (2, 15)
        , (3, 15)
        , (4, 14)
        , (5, 11)
        , (6, 11)
        , (7, 1)
        , (8, 4)
        , (9, 4)
        , (10, 5)
        , (11, 5)
        , (15, 6)
        , (16, 6)
        , (17, 4)
        , (18, 4)
        , (19, 4)
        , (20, 4)
        , (21, 3)
        , (22, 3)
        , (23, 5)
        , (24, 5)
        , (25, 4)
        , (26, 4)
        , (27, 5)
        , (28, 6)
        , (29, 6)
        , (30, 8)
        , (31, 8)
        , (32, 3)
        , (33, 3)
        , (34, 4)
        , (35, 4)
        , (36, 5)
        , (37, 5)
        , (38, 3)
        , (39, 7)
        , (40, 7)
        , (41, 7)
        , (42, 7)
        , (43, 9)
        , (44, 9)
        , (45, 3)
        , (46, 3)
        , (47, 5)
        , (48, 5)
        , (49, 3)
        , (50, 3)
        , (51, 3)
        , (52, 5)
        , (53, 5)
        , (54, 4)
        , (66, 4)
        ]
{-# INLINE s0DuratD2 #-}

s0DuratD3 :: Sound8 -> Word8
s0DuratD3 =
    getBFstLSorted'
        5
        [ (1, 15)
        , (2, 14)
        , (3, 15)
        , (4, 12)
        , (5, 12)
        , (6, 12)
        , (7, 1)
        , (8, 5)
        , (9, 5)
        , (10, 6)
        , (11, 6)
        , (15, 8)
        , (16, 8)
        , (17, 5)
        , (18, 5)
        , (19, 5)
        , (20, 5)
        , (21, 6)
        , (22, 6)
        , (23, 6)
        , (24, 6)
        , (25, 4)
        , (26, 4)
        , (27, 7)
        , (28, 7)
        , (29, 7)
        , (30, 9)
        , (31, 9)
        , (32, 4)
        , (33, 4)
        , (34, 4)
        , (35, 4)
        , (36, 5)
        , (37, 5)
        , (38, 4)
        , (39, 7)
        , (40, 7)
        , (41, 9)
        , (42, 9)
        , (43, 10)
        , (44, 10)
        , (45, 4)
        , (46, 4)
        , (47, 7)
        , (48, 7)
        , (49, 4)
        , (50, 4)
        , (51, 4)
        , (52, 8)
        , (53, 8)
        , (54, 5)
        , (66, 5)
        ]
{-# INLINE s0DuratD3 #-}

s0DuratD4 :: Sound8 -> Word8
s0DuratD4 =
    getBFstLSorted'
        5
        [ (1, 12)
        , (2, 12)
        , (3, 12)
        , (4, 12)
        , (5, 12)
        , (6, 12)
        , (7, 1)
        , (8, 5)
        , (9, 5)
        , (10, 10)
        , (11, 10)
        , (15, 5)
        , (16, 5)
        , (17, 8)
        , (18, 8)
        , (19, 8)
        , (20, 8)
        , (21, 13)
        , (22, 13)
        , (23, 10)
        , (24, 10)
        , (25, 5)
        , (26, 5)
        , (27, 4)
        , (28, 10)
        , (29, 10)
        , (30, 15)
        , (31, 15)
        , (32, 8)
        , (33, 8)
        , (34, 4)
        , (35, 4)
        , (36, 6)
        , (37, 6)
        , (38, 4)
        , (39, 12)
        , (40, 12)
        , (41, 14)
        , (42, 14)
        , (43, 11)
        , (44, 11)
        , (45, 6)
        , (46, 6)
        , (47, 4)
        , (48, 4)
        , (49, 6)
        , (50, 11)
        , (51, 11)
        , (52, 15)
        , (53, 15)
        , (54, 7)
        , (66, 5)
        ]
{-# INLINE s0DuratD4 #-}

class (Eq a) => SyllableDurations4 a where
    sDuratsD :: a -> Word8
    sDuratsD2 :: a -> Word8
    sDuratsD3 :: a -> Word8
    sDuratsD4 :: a -> Word8
    syllableDurationsGDc :: (a -> Word8) -> [[[a]]] -> [[Word8]]
    syllableDurationsGDc g = map (map (k g))
      where
        k f = foldl' (\y x -> f x + y) 0
    {-# INLINE syllableDurationsGDc #-}

instance SyllableDurations4 Sound8 where
    sDuratsD = s0DuratD1
    sDuratsD2 = s0DuratD2
    sDuratsD3 = s0DuratD3
    sDuratsD4 = s0DuratD4

-- | General variant of the 'syllableDurationsD' function.
syllableDurationsGD :: (Sound8 -> Word8) -> [[[Sound8]]] -> [[Word8]]
syllableDurationsGD g = syllableDurationsGDc g
{-# INLINE syllableDurationsGD #-}

syllableDurationsD :: [[[Sound8]]] -> [[Word8]]
syllableDurationsD = syllableDurationsGDc s0DuratD1
{-# INLINE syllableDurationsD #-}

syllableDurationsD2 :: [[[Sound8]]] -> [[Word8]]
syllableDurationsD2 = syllableDurationsGDc s0DuratD2
{-# INLINE syllableDurationsD2 #-}

syllableDurationsD3 :: [[[Sound8]]] -> [[Word8]]
syllableDurationsD3 = syllableDurationsGDc s0DuratD3
{-# INLINE syllableDurationsD3 #-}

syllableDurationsD4 :: [[[Sound8]]] -> [[Word8]]
syllableDurationsD4 = syllableDurationsGDc s0DuratD4
{-# INLINE syllableDurationsD4 #-}
