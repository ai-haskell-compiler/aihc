{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.PopCount.PopCount0
    ( PopCount0(..)
    ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Bits.Types.Broadword
import HaskellWorks.Data.Bits.Types.Builtin
import HaskellWorks.Data.Positioning
import Prelude                                   as P

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

class PopCount0 v where
  -- | The number of 0-bits in the value.
  popCount0 :: v -> Count

instance PopCount0 Bool where
  popCount0 True  = 0
  popCount0 False = 1
  {-# INLINE popCount0 #-}

instance PopCount0 Word8 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word16 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word32 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 Word64 where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word8) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word16) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word32) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Broadword Word64) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word8) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word16) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word32) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 (Builtin Word64) where
  popCount0 = popCount1 . comp
  {-# INLINE popCount0 #-}

instance PopCount0 a => PopCount0 [a] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector Word8) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector Word16) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector Word32) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector Word64) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector Word8) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector Word16) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector Word32) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector Word64) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

-- Vector of Builtin instances

instance PopCount0 (DV.Vector (Builtin Word8)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Builtin Word16)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Builtin Word32)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Builtin Word64)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Builtin Word8)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Builtin Word16)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Builtin Word32)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Builtin Word64)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

-- Vector of Broadword instances

instance PopCount0 (DV.Vector (Broadword Word8)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Broadword Word16)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Broadword Word32)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DV.Vector (Broadword Word64)) where
  popCount0 = DV.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Broadword Word8)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Broadword Word16)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Broadword Word32)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVS.Vector (Broadword Word64)) where
  popCount0 = DVS.foldl' (\c -> (c +) . popCount0) 0
  {-# INLINE popCount0 #-}

instance PopCount0 (DVU.Vector Bit.Bit) where
  popCount0 v = fromIntegral $ DVU.length v - Bit.countBits v
  {-# INLINE popCount0 #-}

instance PopCount0 (DVU.Vector BitTS.Bit) where
  popCount0 v = fromIntegral $ DVU.length v - BitTS.countBits v
  {-# INLINE popCount0 #-}
