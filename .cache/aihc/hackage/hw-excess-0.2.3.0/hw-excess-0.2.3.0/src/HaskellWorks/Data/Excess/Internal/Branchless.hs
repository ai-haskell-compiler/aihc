module HaskellWorks.Data.Excess.Internal.Branchless
  ( maxInt64
  , maxInt
  , minInt64
  , minInt
  ) where

import Data.Int
import HaskellWorks.Data.Bits.BitWise

import qualified HaskellWorks.Data.Branchless as BL

minInt64 :: Int64 -> Int64 -> Int64
minInt64 a b = fromIntegral ((m .&. fromIntegral a) .|. (comp m .&. fromIntegral b))
    where t = BL.ltWord64 (fromIntegral (a - minBound)) (fromIntegral (b - minBound))
          m = 0 - t
{-# INLINE minInt64 #-}

minInt :: Int -> Int -> Int
minInt a b = fromIntegral (minInt64 (fromIntegral a) (fromIntegral b))
{-# INLINE minInt #-}

maxInt64 :: Int64 -> Int64 -> Int64
maxInt64 a b = fromIntegral ((m .&. fromIntegral a) .|. (comp m .&. fromIntegral b))
    where t = BL.gtWord64 (fromIntegral (a - minBound)) (fromIntegral (b - minBound))
          m = 0 - t
{-# INLINE maxInt64 #-}

maxInt :: Int -> Int -> Int
maxInt a b = fromIntegral (maxInt64 (fromIntegral a) (fromIntegral b))
{-# INLINE maxInt #-}
