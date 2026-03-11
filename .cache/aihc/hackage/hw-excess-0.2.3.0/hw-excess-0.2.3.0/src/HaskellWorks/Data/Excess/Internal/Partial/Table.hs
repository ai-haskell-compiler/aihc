module HaskellWorks.Data.Excess.Internal.Partial.Table
  ( genWord8Excess0
  , genWord8Excess1

  , tableWord8Excess0Lo
  , tableWord8Excess0Ex
  , tableWord8Excess0Hi
  , tableWord8Excess1Lo
  , tableWord8Excess1Ex
  , tableWord8Excess1Hi
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Internal.Triplet8 (Triplet8 (Triplet8))

import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.Excess.Internal.Triplet8 as T8

tableWord8Excess0Lo :: DVS.Vector Int8
tableWord8Excess0Lo = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Lo (genWord8Excess0 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess0Lo #-}

tableWord8Excess0Ex :: DVS.Vector Int8
tableWord8Excess0Ex = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Ex (genWord8Excess0 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess0Ex #-}

tableWord8Excess0Hi :: DVS.Vector Int8
tableWord8Excess0Hi = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Hi (genWord8Excess0 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess0Hi #-}

tableWord8Excess1Lo :: DVS.Vector Int8
tableWord8Excess1Lo = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Lo (genWord8Excess1 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess1Lo #-}

tableWord8Excess1Ex :: DVS.Vector Int8
tableWord8Excess1Ex = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Ex (genWord8Excess1 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess1Ex #-}

tableWord8Excess1Hi :: DVS.Vector Int8
tableWord8Excess1Hi = DVS.generate (256 * 9) go
  where go :: Int -> Int8
        go i = T8.triplet8Hi (genWord8Excess1 (i `div` 256) (fromIntegral i))
{-# NOINLINE tableWord8Excess1Hi #-}

genWord8Excess0 :: Int -> Word8 -> Triplet8
genWord8Excess0 = go 0 0 0
  where go :: Int8 -> Int8 -> Int8 -> Int -> Word8 -> Triplet8
        go lo ex hi n w = if n > 0
          then if w .&. 1 /= 1
            then let fx = ex + 1 in go      lo     fx (max hi fx) (n - 1) (w .>. 1)
            else let fx = ex - 1 in go (min lo fx) fx      hi     (n - 1) (w .>. 1)
          else Triplet8 lo ex hi
{-# INLINE genWord8Excess0 #-}

genWord8Excess1 :: Int -> Word8 -> Triplet8
genWord8Excess1 = go 0 0 0
  where go :: Int8 -> Int8 -> Int8 -> Int -> Word8 -> Triplet8
        go lo ex hi n w = if n > 0
          then if w .&. 1 /= 0
            then let fx = ex + 1 in go      lo     fx (max hi fx) (n - 1) (w .>. 1)
            else let fx = ex - 1 in go (min lo fx) fx      hi     (n - 1) (w .>. 1)
          else Triplet8 lo ex hi
{-# INLINE genWord8Excess1 #-}
