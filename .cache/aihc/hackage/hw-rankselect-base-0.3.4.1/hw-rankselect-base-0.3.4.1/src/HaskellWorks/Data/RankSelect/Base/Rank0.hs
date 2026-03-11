{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.RankSelect.Base.Rank0
    ( Rank0(..)
    ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.ElemFixedBitSize
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1   as X
import Prelude                                   as P

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

{- HLINT ignore "Reduce duplication"  -}

class Rank0 v where
  -- | Find the number of occurences of the bit @0@ in the prefix of the supplied bitstring of the given length
  --
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> :set -XTypeApplications
  --
  -- >>> rank0 (unsafeBitRead @Word8 "11111111") 4
  -- 0
  -- >>> rank0 (unsafeBitRead @Word8 "00111111") 4
  -- 2
  -- >>> rank0 (unsafeBitRead @Word8 "10011111") 4
  -- 2
  -- >>> rank0 (unsafeBitRead @Word8 "10011001") 4
  -- 2
  -- >>> rank0 (unsafeBitRead @Word8 "10011001") 6
  -- 3
  rank0
    :: v      -- ^ The bitstring
    -> Count  -- ^ The prefix length
    -> Count

deriving instance Rank0 a => Rank0 (BitShown a)

instance Rank0 Word8 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINE rank0 #-}

instance Rank0 Word16 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINE rank0 #-}

instance Rank0 Word32 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINE rank0 #-}

instance Rank0 Word64 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINE rank0 #-}

instance Rank0 [Bool] where
  rank0 = go 0
    where go r _ 0          = r
          go r (False:bs) p = go (r + 1) bs (p - 1)
          go r (True:bs) p  = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINE rank0 #-}

instance Rank0 [Word8] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 [Word16] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 [Word32] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 [Word64] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DV.Vector Word8) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DV.Vector Word16) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DV.Vector Word32) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DV.Vector Word64) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DVS.Vector Word8) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DVS.Vector Word16) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DVS.Vector Word32) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DVS.Vector Word64) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINE rank0 #-}

instance Rank0 (DVU.Vector Bit.Bit) where
  rank0 v p = p - rank1 v p
  {-# INLINE rank0 #-}

instance Rank0 (DVU.Vector BitTS.Bit) where
  rank0 v p = p - rank1 v p
  {-# INLINE rank0 #-}
