{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.RankSelect.Base.Rank1
    ( Rank1(..)
    ) where

import Data.Bits.BitSize
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.ElemFixedBitSize
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

{- HLINT ignore "Reduce duplication"  -}

class Rank1 v where
  -- | Find the number of occurences of the bit @1@ in the prefix of the supplied bitstring of the given length
  --
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> :set -XTypeApplications
  --
  -- >>> rank1 (unsafeBitRead @Word8 "00000000") 4
  -- 0
  -- >>> rank1 (unsafeBitRead @Word8 "11000000") 4
  -- 2
  -- >>> rank1 (unsafeBitRead @Word8 "01100000") 4
  -- 2
  -- >>> rank1 (unsafeBitRead @Word8 "01100110") 4
  -- 2
  -- >>> rank1 (unsafeBitRead @Word8 "01100110") 6
  -- 3
  rank1
    :: v      -- ^ The bitstring
    -> Count  -- ^ The prefix length
    -> Count

deriving instance Rank1 a => Rank1 (BitShown a)

instance Rank1 Word8 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word16 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word32 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word64 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Bool where
  rank1 True  0 = 0
  rank1 True  1 = 1
  rank1 False 0 = 0
  rank1 False 1 = 0
  rank1 _     _ = error "Invalid position for rank1"

instance (PopCount1 w, Rank1 w, BitSize w) => Rank1 [w] where
  rank1 = go 0
    where go c (w:ws) p = if p <= bitCount w
            then c + rank1 w p
            else go (c + popCount1 w) ws (p - bitCount w)
          go c [] _ = c

  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVU.Vector Bit.Bit) where
  rank1 v p = fromIntegral (Bit.countBits (DVU.take (fromIntegral p) v))
  {-# INLINE rank1 #-}

instance Rank1 (DVU.Vector BitTS.Bit) where
  rank1 v p = fromIntegral (BitTS.countBits (DVU.take (fromIntegral p) v))
  {-# INLINE rank1 #-}
