{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword.Word32
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
-- "55555555"
-- >>> showHex (l 4) ""
-- "11111111"
-- >>> showHex (l 8) ""
-- "1010101"
-- >>> showHex (l 16) ""
-- "10001"
-- >>> showHex (l 32) ""
-- "1"
l :: Int -> Word32
l 2  = 0x55555555
l 4  = 0x11111111
l 8  = 0x01010101
l 16 = 0x00010001
l 32 = 0x00000001
l k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE l #-}

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16, 32 } such that the highest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (h 2) ""
-- "aaaaaaaa"
-- >>> showHex (h 4) ""
-- "88888888"
-- >>> showHex (h 8) ""
-- "80808080"
-- >>> showHex (h 16) ""
-- "80008000"
-- >>> showHex (h 32) ""
-- "80000000"
h :: Int -> Word32
h 2  = 0xaaaaaaaa
h 4  = 0x88888888
h 8  = 0x80808080
h 16 = 0x80008000
h 32 = 0x80000000
h k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE h #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32 }.
--
-- The subtraction respects 2's complement so sub-words may be regarded as signed or unsigned words.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiff 8 0x04030201 0x02020101) ""
-- "2010100"
-- >>> showHex (kBitDiff 8 0x04030201 0x01020304) ""
-- "301fffd"
-- >>> showHex (kBitDiff 8 0x200000ff 0x100000ff) ""
-- "10000000"
kBitDiff :: Int -> Word32 -> Word32 -> Word32
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32 } where results are bounded from below by 0.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffPos 8 0x04030201 0x02020101) ""
-- "2010100"
-- >>> showHex (kBitDiffPos 8 0x04030201 0x01020304) ""
-- "3010000"
-- >>> showHex (kBitDiffPos 8 0x200000ff 0x100000ff) "" -- produces nonsense in the last sub-word
-- "10000000"
kBitDiffPos :: Int -> Word32 -> Word32 -> Word32
kBitDiffPos k x y =
                                                                        -- let !_ = trace (">> x = " <> bitShow x) x in
                                                                        -- let !_ = trace (">> y = " <> bitShow y) y in
  let d = kBitDiff k x y                                            in  -- let !_ = trace (">> d = " <> bitShow d) d in
  let s = kBitDiff k 0 ((comp d .&. h k) .>. fromIntegral (k - 1))  in  -- let !_ = trace (">> s = " <> bitShow s) s in
  let r = d .&. s                                                   in  -- let !_ = trace (">> r = " <> bitShow r) r in
  r
{-# INLINE kBitDiffPos #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32 } where all the sub-words of 'x' and 'y' must
-- not have the signed bit set for the result to be meaningful.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffUnsafe 8 0x04030201 0x02020101) ""
-- "2010100"
-- >>> showHex (kBitDiffUnsafe 8 0x04030201 0x05060708) ""
-- "fffdfbf9"
-- >>> showHex (kBitDiffUnsafe 8 0x200000ff 0x100000ff) ""
-- "10000080"
kBitDiffUnsafe :: Int -> Word32 -> Word32 -> Word32
kBitDiffUnsafe k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffUnsafe #-}
