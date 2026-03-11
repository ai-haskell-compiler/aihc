{-# LANGUAGE NoImplicitPrelude, Strict #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Rhythmicity.BasicF
-- Copyright   :  (c) Oleksandr Zhabenko 2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Functions for Rhythmicity.MarkerSeqs module that additionally \"catches\" some hashing logics 
-- so that they can influence in the predictable way the peculiarities of the results 
-- for PhLADiPreLiO.

module Rhythmicity.BasicF where

import Data.Bits
import GHC.Num
import GHC.Base
import GHC.Real
import GHC.List
import GHC.Int
import GHC.Enum (fromEnum)

hashPosLF2 :: Int8 -> [Integer] -> Integer
hashPosLF2 i ns = shift (sum ns) . fromEnum $ i
{-# INLINE hashPosLF2 #-}

hashBalancingLF2 :: Int8 -> [Integer] -> Integer
hashBalancingLF2 = hashBalancingLF2G 20
{-# INLINE hashBalancingLF2 #-}

hashBasicLF2 ::  Int8 -> [Integer] -> Integer
hashBasicLF2 _ = sum
{-# INLINE hashBasicLF2 #-}

-- | Here semantically the first argument must be greater than at least 2. But this is not checked for
-- performance reasons.
hashBalancingLF2G :: Int -> Int8 -> [Integer] -> Integer
hashBalancingLF2G k _ = sum . zipWith (\pos n -> shiftR n (k*pos)) [6,5..0]
{-# INLINE hashBalancingLF2G #-}

