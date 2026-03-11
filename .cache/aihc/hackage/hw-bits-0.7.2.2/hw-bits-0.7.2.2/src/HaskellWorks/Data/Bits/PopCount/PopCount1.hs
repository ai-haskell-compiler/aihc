{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.PopCount.PopCount1
    ( PopCount1(..)
    ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Types.Broadword
import HaskellWorks.Data.Bits.Types.Builtin
import HaskellWorks.Data.Int.Widen.Widen64
import HaskellWorks.Data.Positioning
import Prelude                                as P

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Bits            as DB
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

type FastWord a = Builtin a

fastWord :: a -> FastWord a
fastWord = Builtin
{-# INLINE fastWord #-}

class PopCount1 v where
  -- | The number of 1-bits in the value.
  popCount1 :: v -> Count

instance PopCount1 Bool where
  popCount1 True  = 1
  popCount1 False = 0
  {-# INLINE popCount1 #-}

instance PopCount1 (Broadword Word8) where
  popCount1 (Broadword x0) = widen64 x3
    where
      x1 = x0 - ((x0 .&. 0xaa) .>. 1)
      x2 = (x1 .&. 0x33) + ((x1 .>. 2) .&. 0x33)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f
  {-# INLINE popCount1 #-}

instance PopCount1 (Broadword Word16) where
  popCount1 (Broadword x0) = widen64 ((x3 * 0x0101) .>. 8)
    where
      x1 = x0 - ((x0 .&. 0xaaaa) .>. 1)
      x2 = (x1 .&. 0x3333) + ((x1 .>. 2) .&. 0x3333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f
  {-# INLINE popCount1 #-}

instance PopCount1 (Broadword Word32) where
  popCount1 (Broadword x0) = widen64 ((x3 * 0x01010101) .>. 24)
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x33333333) + ((x1 .>. 2) .&. 0x33333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f
  {-# INLINE popCount1 #-}

instance PopCount1 (Broadword Word64) where
  popCount1 (Broadword x0) = widen64 (x3 * 0x0101010101010101) .>. 56
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaaaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x3333333333333333) + ((x1 .>. 2) .&. 0x3333333333333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f
  {-# INLINE popCount1 #-}

instance PopCount1 (Builtin Word8) where
  popCount1 (Builtin x0) = fromIntegral (DB.popCount x0)
  {-# INLINE popCount1 #-}

instance PopCount1 (Builtin Word16) where
  popCount1 (Builtin x0) = fromIntegral (DB.popCount x0)
  {-# INLINE popCount1 #-}

instance PopCount1 (Builtin Word32) where
  popCount1 (Builtin x0) = fromIntegral (DB.popCount x0)
  {-# INLINE popCount1 #-}

instance PopCount1 (Builtin Word64) where
  popCount1 (Builtin x0) = fromIntegral (DB.popCount x0)
  {-# INLINE popCount1 #-}

instance PopCount1 Word8 where
  popCount1 = fromIntegral . popCount1 . fastWord
  {-# INLINE popCount1 #-}

instance PopCount1 Word16 where
  popCount1 = fromIntegral . popCount1 . fastWord
  {-# INLINE popCount1 #-}

instance PopCount1 Word32 where
  popCount1 = fromIntegral . popCount1 . fastWord
  {-# INLINE popCount1 #-}

instance PopCount1 Word64 where
  popCount1 = fromIntegral . popCount1 . fastWord
  {-# INLINE popCount1 #-}

instance PopCount1 a => PopCount1 [a] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector Word8) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector Word16) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector Word32) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector Word64) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector Word8) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector Word16) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector Word32) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector Word64) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

-- Vector of Builtin instances

instance PopCount1 (DV.Vector (Builtin Word8)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Builtin Word16)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Builtin Word32)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Builtin Word64)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Builtin Word8)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Builtin Word16)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Builtin Word32)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Builtin Word64)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

-- Vector of Broadword instances

instance PopCount1 (DV.Vector (Broadword Word8)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Broadword Word16)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Broadword Word32)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DV.Vector (Broadword Word64)) where
  popCount1 = DV.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Broadword Word8)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Broadword Word16)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Broadword Word32)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVS.Vector (Broadword Word64)) where
  popCount1 = DVS.foldl' (\c -> (c +) . popCount1) 0
  {-# INLINE popCount1 #-}

instance PopCount1 (DVU.Vector Bit.Bit) where
  popCount1 = fromIntegral . Bit.countBits
  {-# INLINE popCount1 #-}

instance PopCount1 (DVU.Vector BitTS.Bit) where
  popCount1 = fromIntegral . BitTS.countBits
  {-# INLINE popCount1 #-}
