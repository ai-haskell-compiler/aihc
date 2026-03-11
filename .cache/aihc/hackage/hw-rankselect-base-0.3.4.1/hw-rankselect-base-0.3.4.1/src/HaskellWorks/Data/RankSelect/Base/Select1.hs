{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.RankSelect.Base.Select1
    ( Select1(..)
    ) where

import Data.Bits.BitSize
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.ElemFixedBitSize
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Int.Narrow
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Internal
import Prelude

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

class Select1 v where
  -- | Find length of the shortest prefix of the given prefix that contains specified number of occurences of the bit @1@
  --
  -- If the bitstring does not have enough occurences of bit @1@ is insufficient to satisfy the query the result is undefined.
  --
  -- >>> import HaskellWorks.Data.Bits.BitRead
  -- >>> :set -XTypeApplications
  --
  -- >>> select1 (unsafeBitRead @Word8 "00000000") 0
  -- 0
  -- >>> select1 (unsafeBitRead @Word8 "00001000") 1
  -- 5
  -- >>> select1 (unsafeBitRead @Word8 "11111111") 4
  -- 4
  -- >>> select1 (unsafeBitRead @Word8 "00111111") 4
  -- 6
  -- >>> select1 (unsafeBitRead @Word8 "10011111") 4
  -- 6
  select1
    :: v      -- ^ The bitstring
    -> Count  -- ^ The number of ones
    -> Count

deriving instance Select1 a => Select1 (BitShown a)

-- TODO: Implement NOT interms of select for word-16
instance Select1 Word8 where
  select1 _ 0 = 0
  select1 v p = select1 (fromIntegral v :: Word16) p
  {-# INLINE select1 #-}

-- TODO: Remove redundant code to optimise
instance Select1 Word16 where
  select1 _ 0 = 0
  select1 v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555) + ((v .>.  1) .&. 0x5555)    in
    let b = (a .&. 0x3333) + ((a .>.  2) .&. 0x3333)    in
    let c = (b .&. 0x0f0f) + ((b .>.  4) .&. 0x0f0f)    in
    let d = (c .&. 0x00ff) + ((c .>.  8) .&. 0x00ff)    in
    -- Now do branchless select!
    let r0 = d + 1 - narrow16 rn                                                in
    let s0 = 64 :: Word16                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINE select1 #-}

instance Select1 Word32 where
  select1 = select1Word32
  {-# INLINE select1 #-}

instance Select1 Word64 where
  select1 = select1Word64
  {-# INLINE select1 #-}

instance Select1 Bool where
  select1 b c = if c == 1 && b then 1 else 0

instance Select1 (DVS.Vector Word8) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DVS.Vector Word16) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DVS.Vector Word32) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DVS.Vector Word64) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DV.Vector Word8) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DV.Vector Word16) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DV.Vector Word32) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance Select1 (DV.Vector Word64) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc -> select1 w d + acc
              pc           -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINE select1 #-}

instance (PopCount1 w, Select1 w, BitSize w) => Select1 [w] where
  select1 v c = go v c 0
    where go _ 0 acc = acc
          go u d acc = case u of
            w:ws -> let pc = popCount1 w in
              if d <= pc
                then select1 w d + acc
                else go ws (d - pc) (acc + bitCount w)
            [] -> acc
  {-# INLINE select1 #-}

instance Select1 (DVU.Vector Bit.Bit) where
  select1 _ 0 = 0
  select1 v p = fromIntegral $ maybe (DVU.length v) (+ 1) $ Bit.nthBitIndex (Bit.Bit True) (fromIntegral p) v
  {-# INLINE select1 #-}

instance Select1 (DVU.Vector BitTS.Bit) where
  select1 _ 0 = 0
  select1 v p = fromIntegral $ maybe (DVU.length v) (+ 1) $ BitTS.nthBitIndex (BitTS.Bit True) (fromIntegral p) v
  {-# INLINE select1 #-}
