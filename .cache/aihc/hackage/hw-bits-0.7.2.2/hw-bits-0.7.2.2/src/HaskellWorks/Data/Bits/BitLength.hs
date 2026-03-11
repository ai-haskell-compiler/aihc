{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Bits.BitLength
    ( -- * Bit map
      BitLength(..)
    , elemBitLength
    , elemBitEnd
    ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning
import Prelude                       hiding (length)

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

class BitLength v where
  -- | Number of bits in a value including ones and zeros.
  bitLength :: v -> Count

  -- | Number of bits in a value including ones and zeros as a position.
  endPosition :: v -> Position
  endPosition = toPosition . bitLength
  {-# INLINE endPosition #-}

--------------------------------------------------------------------------------
-- Functions

elemBitLength :: (AtIndex v, BitLength (Elem v)) => v -> Count
elemBitLength v = bitLength (v !!! 0)
{-# INLINE elemBitLength #-}

elemBitEnd :: (AtIndex v, BitLength (Elem v)) => v -> Position
elemBitEnd v = endPosition (v !!! 0)
{-# INLINE elemBitEnd #-}

--------------------------------------------------------------------------------
-- Instances

instance BitLength Bool where
  bitLength _ = 1
  {-# INLINE bitLength #-}

instance BitLength [Bool] where
  bitLength = fromIntegral . length
  {-# INLINE bitLength #-}

instance BitLength Word8 where
  bitLength _ = 8
  {-# INLINE bitLength #-}

instance BitLength Word16 where
  bitLength _ = 16
  {-# INLINE bitLength #-}

instance BitLength Word32 where
  bitLength _ = 32
  {-# INLINE bitLength #-}

instance BitLength Word64 where
  bitLength _ = 64
  {-# INLINE bitLength #-}

instance BitLength (Naive Word8) where
  bitLength _ = 8
  {-# INLINE bitLength #-}

instance BitLength (Naive Word16) where
  bitLength _ = 16
  {-# INLINE bitLength #-}

instance BitLength (Naive Word32) where
  bitLength _ = 32
  {-# INLINE bitLength #-}

instance BitLength (Naive Word64) where
  bitLength _ = 64
  {-# INLINE bitLength #-}

instance BitLength [Word8] where
  bitLength v = fromIntegral (length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word16] where
  bitLength v = fromIntegral (length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word32] where
  bitLength v = fromIntegral (length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word64] where
  bitLength v = fromIntegral (length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word8) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word16) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word32) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word64) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word8) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word16) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word32) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word64) where
  bitLength v = length v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVU.Vector Bit.Bit) where
  bitLength = fromIntegral . DVU.length
  {-# INLINE bitLength #-}

instance BitLength (DVU.Vector BitTS.Bit) where
  bitLength = fromIntegral . DVU.length
  {-# INLINE bitLength #-}
