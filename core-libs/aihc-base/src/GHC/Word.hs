{-# LANGUAGE MagicHash #-}

module GHC.Word
  ( Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    byteSwap16,
    byteSwap32,
    byteSwap64,
  )
where

import GHC.Internal.Integer (integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim
  ( Word#,
    Word16#,
    Word32#,
    Word64#,
    Word8#,
    byteSwap16#,
    byteSwap32#,
    byteSwap64#,
    eqWord#,
    int2Word#,
    minusWord#,
    plusWord#,
    timesWord#,
    word16ToWord#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
  )

data Word = W# Word#

data Word8 = W8# Word8#

data Word16 = W16# Word16#

data Word32 = W32# Word32#

data Word64 = W64# Word64#

-- | Reverse the byte order of a 16-bit word.
byteSwap16 :: Word16 -> Word16
byteSwap16 (W16# value) = W16# (wordToWord16# (byteSwap16# (word16ToWord# value)))

-- | Reverse the byte order of a 32-bit word.
byteSwap32 :: Word32 -> Word32
byteSwap32 (W32# value) = W32# (wordToWord32# (byteSwap32# (word32ToWord# value)))

-- | Reverse the byte order of a 64-bit word.
byteSwap64 :: Word64 -> Word64
byteSwap64 (W64# value) = W64# (byteSwap64# value)

-- | Two's complement negation of a machine word.
wordNegate :: Word# -> Word#
wordNegate = minusWord# (int2Word# 0#)

-- | One for a non-zero word and zero for zero.
wordSignum :: Word# -> Word#
wordSignum value =
  case eqWord# value (int2Word# 0#) of
    0# -> int2Word# 1#
    _ -> int2Word# 0#

instance Num Word where
  W# left + W# right = W# (plusWord# left right)
  W# left - W# right = W# (minusWord# left right)
  W# left * W# right = W# (timesWord# left right)
  negate (W# value) = W# (wordNegate value)
  abs value = value
  signum (W# value) = W# (wordSignum value)
  fromInteger value = W# (int2Word# (integerToInt# value))

instance Num Word8 where
  W8# left + W8# right = W8# (wordToWord8# (plusWord# (word8ToWord# left) (word8ToWord# right)))
  W8# left - W8# right = W8# (wordToWord8# (minusWord# (word8ToWord# left) (word8ToWord# right)))
  W8# left * W8# right = W8# (wordToWord8# (timesWord# (word8ToWord# left) (word8ToWord# right)))
  negate (W8# value) = W8# (wordToWord8# (wordNegate (word8ToWord# value)))
  abs value = value
  signum (W8# value) = W8# (wordToWord8# (wordSignum (word8ToWord# value)))
  fromInteger value = W8# (wordToWord8# (int2Word# (integerToInt# value)))

instance Num Word16 where
  W16# left + W16# right = W16# (wordToWord16# (plusWord# (word16ToWord# left) (word16ToWord# right)))
  W16# left - W16# right = W16# (wordToWord16# (minusWord# (word16ToWord# left) (word16ToWord# right)))
  W16# left * W16# right = W16# (wordToWord16# (timesWord# (word16ToWord# left) (word16ToWord# right)))
  negate (W16# value) = W16# (wordToWord16# (wordNegate (word16ToWord# value)))
  abs value = value
  signum (W16# value) = W16# (wordToWord16# (wordSignum (word16ToWord# value)))
  fromInteger value = W16# (wordToWord16# (int2Word# (integerToInt# value)))

instance Num Word32 where
  W32# left + W32# right = W32# (wordToWord32# (plusWord# (word32ToWord# left) (word32ToWord# right)))
  W32# left - W32# right = W32# (wordToWord32# (minusWord# (word32ToWord# left) (word32ToWord# right)))
  W32# left * W32# right = W32# (wordToWord32# (timesWord# (word32ToWord# left) (word32ToWord# right)))
  negate (W32# value) = W32# (wordToWord32# (wordNegate (word32ToWord# value)))
  abs value = value
  signum (W32# value) = W32# (wordToWord32# (wordSignum (word32ToWord# value)))
  fromInteger value = W32# (wordToWord32# (int2Word# (integerToInt# value)))

instance Num Word64 where
  W64# left + W64# right = W64# (wordToWord64# (plusWord# (word64ToWord# left) (word64ToWord# right)))
  W64# left - W64# right = W64# (wordToWord64# (minusWord# (word64ToWord# left) (word64ToWord# right)))
  W64# left * W64# right = W64# (wordToWord64# (timesWord# (word64ToWord# left) (word64ToWord# right)))
  negate (W64# value) = W64# (wordToWord64# (wordNegate (word64ToWord# value)))
  abs value = value
  signum (W64# value) = W64# (wordToWord64# (wordSignum (word64ToWord# value)))
  fromInteger value = W64# (wordToWord64# (int2Word# (integerToInt# value)))
