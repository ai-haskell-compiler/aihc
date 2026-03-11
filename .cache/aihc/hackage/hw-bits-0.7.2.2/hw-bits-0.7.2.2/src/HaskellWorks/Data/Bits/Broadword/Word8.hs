{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword.Word8
  ( h
  , l
  , kBitDiff
  , kBitDiffPos
  , kBitDiffUnsafe
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16, 32 } such that the lowest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (l 2) ""
-- "55"
-- >>> showHex (l 4) ""
-- "11"
-- >>> showHex (l 8) ""
-- "1"
l :: Int -> Word8
l 2 = 0x55
l 4 = 0x11
l 8 = 0x01
l k = error ("Invalid h k where k = " ++ show k)
{-# INLINE l #-}

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16, 32 } such that the highest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (h 2) ""
-- "aa"
-- >>> showHex (h 4) ""
-- "88"
-- >>> showHex (h 8) ""
-- "80"
h :: Int -> Word8
h 2 = 0xaa
h 4 = 0x88
h 8 = 0x80
h k = error ("Invalid h k where k = " ++ show k)
{-# INLINE h #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8 }.
--
-- The subtraction respects 2's complement so sub-words may be regarded as signed or unsigned words.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiff 8 0x02 0x01) ""
-- "1"
-- >>> showHex (kBitDiff 8 0x01 0x02) ""
-- "ff"
-- >>> showHex (kBitDiff 8 0xff 0xff) ""
-- "0"
kBitDiff :: Int -> Word8 -> Word8 -> Word8
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8 } where results are bounded from below by 0.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiff 8 0x02 0x01) ""
-- "1"
-- >>> showHex (kBitDiff 8 0x01 0x02) ""
-- "ff"
-- >>> showHex (kBitDiff 8 0xff 0xff) ""
-- "0"
kBitDiffPos :: Int -> Word8 -> Word8 -> Word8
kBitDiffPos k x y =
                                                                        -- let !_ = trace (">> x = " <> bitShow x) x in
                                                                        -- let !_ = trace (">> y = " <> bitShow y) y in
  let d = kBitDiff k x y                                            in  -- let !_ = trace (">> d = " <> bitShow d) d in
  let s = kBitDiff k 0 ((comp d .&. h k) .>. fromIntegral (k - 1))  in  -- let !_ = trace (">> s = " <> bitShow s) s in
  let r = d .&. s                                                   in  -- let !_ = trace (">> r = " <> bitShow r) r in
  r
{-# INLINE kBitDiffPos #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8 } where all the sub-words of 'x' and 'y' must
-- not have the signed bit set for the result to be meaningful.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffUnsafe 8 0x02 0x01) ""
-- "1"
-- >>> showHex (kBitDiffUnsafe 8 0x01 0x02) ""
-- "ff"
-- >>> showHex (kBitDiffUnsafe 8 0xff 0xff) "" -- produces nonsense in the last sub-word
-- "80"
kBitDiffUnsafe :: Int -> Word8 -> Word8 -> Word8
kBitDiffUnsafe k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffUnsafe #-}
