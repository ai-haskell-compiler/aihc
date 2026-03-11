{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bits.BitSize where

import Data.Functor.Identity
import Data.Int
import Data.Word

import qualified Data.Bits            as B
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class BitSize a where
  bitSize :: a -> Int
  bitCount :: a -> Word64
  bitCount = fromIntegral . bitSize
  {-# INLINE bitCount #-}

instance BitSize () where
  bitSize _ = 0
  {-# INLINE bitSize #-}

instance BitSize Bool where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Word where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Word64 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Word32 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Word16 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Word8 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Int where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Int64 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Int32 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Int16 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize Int8 where
  bitSize = B.finiteBitSize
  {-# INLINE bitSize #-}

instance BitSize a => BitSize (Identity a) where
  bitSize = foldl (\a b -> a + bitSize b) 0
  {-# INLINE bitSize #-}

instance BitSize a => BitSize [a] where
  bitSize = foldl (\a b -> a + bitSize b) 0
  {-# INLINE bitSize #-}

instance BitSize a => BitSize (DV.Vector a) where
  bitSize = DV.foldl (\a b -> a + bitSize b) 0
  {-# INLINE bitSize #-}

instance forall a. (BitSize a, DVS.Storable a) => BitSize (DVS.Vector a) where
  bitSize v = DVS.length v * bitSize (undefined :: a)
  {-# INLINE bitSize #-}
