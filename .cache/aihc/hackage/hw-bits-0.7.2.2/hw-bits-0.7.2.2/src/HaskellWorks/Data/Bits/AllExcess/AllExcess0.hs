{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.AllExcess.AllExcess0 where

import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

class AllExcess0 a where
  -- | Number of 0-bits minues the number of 1-bits.
  allExcess0 :: a -> Int

instance AllExcess0 [Bool] where
  allExcess0 = go 0
    where go n (False:ys) = go (n + 1) ys
          go n (True :ys) = go (n - 1) ys
          go n _          = n
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word8 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word16 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word32 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word64 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word8) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word16) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word32) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word64) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVU.Vector Bit.Bit) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVU.Vector BitTS.Bit) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}
