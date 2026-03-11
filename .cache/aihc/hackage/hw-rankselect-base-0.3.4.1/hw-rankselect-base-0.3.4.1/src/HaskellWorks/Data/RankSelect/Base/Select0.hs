{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.RankSelect.Base.Select0
    ( Select0(..)
    ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.ElemFixedBitSize
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

{- HLINT ignore "Reduce duplication"  -}

class Select0 v where
  -- | Find length of the shortest prefix of the given prefix that contains specified number of occurences of the bit @1@
  --
  -- If the bitstring does not have enough occurences of bit @0@ is insufficient to satisfy the query the result is undefined.
  --
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> :set -XTypeApplications
  --
  -- >>> select0 (unsafeBitRead @Word8 "11111111") 0
  -- 0
  -- >>> select0 (unsafeBitRead @Word8 "11110111") 1
  -- 5
  -- >>> select0 (unsafeBitRead @Word8 "00000000") 4
  -- 4
  -- >>> select0 (unsafeBitRead @Word8 "11000000") 4
  -- 6
  -- >>> select0 (unsafeBitRead @Word8 "01100000") 4
  -- 6
  select0
    :: v      -- ^ The bitstring
    -> Count  -- ^ The number of zeros
    -> Count

deriving instance Select0 a => Select0 (BitShown a)

-- TODO: Implement NOT in terms of select for word-16
instance Select0 Word8 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word16 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word32 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word64 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 [Bool] where
  select0 = go 0
    where go r _ 0          = r
          go r (False:bs) c = go (r + 1) bs (c - 1)
          go r (True:bs)  c = go (r + 1) bs  c
          go _ []         _ = error "Out of range"
  {-# INLINABLE select0 #-}

instance Select0 [Word8] where
  select0 v c = go v c 0
    where go :: [Word8] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select0 #-}

instance Select0 [Word16] where
  select0 v c = go v c 0
    where go :: [Word16] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select0 #-}

instance Select0 [Word32] where
  select0 v c = go v c 0
    where go :: [Word32] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select0 #-}

instance Select0 [Word64] where
  select0 v c = go v c 0
    where go :: [Word64] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select0 #-}

instance Select0 (DV.Vector Word8) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DV.Vector Word16) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DV.Vector Word32) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DV.Vector Word64) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DVS.Vector Word8) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DVS.Vector Word16) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DVS.Vector Word32) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DVS.Vector Word64) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc -> select0 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select0 #-}

instance Select0 (DVU.Vector Bit.Bit) where
  select0 _ 0 = 0
  select0 v p = fromIntegral $ maybe (DVU.length v) (+ 1) $ Bit.nthBitIndex (Bit.Bit False) (fromIntegral p) v
  {-# INLINABLE select0 #-}

instance Select0 (DVU.Vector BitTS.Bit) where
  select0 _ 0 = 0
  select0 v p = fromIntegral $ maybe (DVU.length v) (+ 1) $ BitTS.nthBitIndex (BitTS.Bit False) (fromIntegral p) v
  {-# INLINABLE select0 #-}
