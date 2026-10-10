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

    -- * Equality operators
    eqWord,
    neWord,
    gtWord,
    geWord,
    ltWord,
    leWord,
    eqWord8,
    neWord8,
    gtWord8,
    geWord8,
    ltWord8,
    leWord8,
    eqWord16,
    neWord16,
    gtWord16,
    geWord16,
    ltWord16,
    leWord16,
    eqWord32,
    neWord32,
    gtWord32,
    geWord32,
    ltWord32,
    leWord32,
    eqWord64,
    neWord64,
    gtWord64,
    geWord64,
    ltWord64,
    leWord64,
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
    eqWord16#,
    eqWord32#,
    eqWord64#,
    eqWord8#,
    geWord#,
    geWord16#,
    geWord32#,
    geWord64#,
    geWord8#,
    gtWord#,
    gtWord16#,
    gtWord32#,
    gtWord64#,
    gtWord8#,
    int2Word#,
    leWord#,
    leWord16#,
    leWord32#,
    leWord64#,
    leWord8#,
    ltWord#,
    ltWord16#,
    ltWord32#,
    ltWord64#,
    ltWord8#,
    minusWord#,
    neWord#,
    neWord16#,
    neWord32#,
    neWord64#,
    neWord8#,
    plusWord#,
    plusWord16#,
    plusWord32#,
    plusWord64#,
    plusWord8#,
    subWord16#,
    subWord32#,
    subWord64#,
    subWord8#,
    timesWord#,
    timesWord16#,
    timesWord32#,
    timesWord64#,
    timesWord8#,
    word16ToWord#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
  )
import GHC.Types (Bool, Word (..), isTrue#)

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
  W8# left + W8# right = W8# (plusWord8# left right)
  W8# left - W8# right = W8# (subWord8# left right)
  W8# left * W8# right = W8# (timesWord8# left right)
  negate (W8# value) = W8# (subWord8# (wordToWord8# (int2Word# 0#)) value)
  abs value = value
  signum (W8# value) = W8# (wordToWord8# (wordSignum (word8ToWord# value)))
  fromInteger value = W8# (wordToWord8# (int2Word# (integerToInt# value)))

instance Num Word16 where
  W16# left + W16# right = W16# (plusWord16# left right)
  W16# left - W16# right = W16# (subWord16# left right)
  W16# left * W16# right = W16# (timesWord16# left right)
  negate (W16# value) = W16# (subWord16# (wordToWord16# (int2Word# 0#)) value)
  abs value = value
  signum (W16# value) = W16# (wordToWord16# (wordSignum (word16ToWord# value)))
  fromInteger value = W16# (wordToWord16# (int2Word# (integerToInt# value)))

-- The Word32 methods use the 32-bit primops. A method that widens to
-- Word# and narrows back costs a mask after each operation on a 64-bit
-- target.
instance Num Word32 where
  W32# left + W32# right = W32# (plusWord32# left right)
  W32# left - W32# right = W32# (subWord32# left right)
  W32# left * W32# right = W32# (timesWord32# left right)
  negate (W32# value) = W32# (subWord32# (wordToWord32# (int2Word# 0#)) value)
  abs value = value
  signum (W32# value) = W32# (wordToWord32# (wordSignum (word32ToWord# value)))
  fromInteger value = W32# (wordToWord32# (int2Word# (integerToInt# value)))

instance Num Word64 where
  W64# left + W64# right = W64# (plusWord64# left right)
  W64# left - W64# right = W64# (subWord64# left right)
  W64# left * W64# right = W64# (timesWord64# left right)
  negate (W64# value) = W64# (subWord64# (wordToWord64# (int2Word# 0#)) value)
  abs value = value
  signum (W64# value) = W64# (wordToWord64# (wordSignum (word64ToWord# value)))
  fromInteger value = W64# (wordToWord64# (int2Word# (integerToInt# value)))

-- The comparison functions that rewrite rules match on, as in GHC. A rule
-- names @eqWord8@ rather than @==@ because a class method has become the
-- instance's own method by the time rules fire.

eqWord, neWord, gtWord, geWord, ltWord, leWord :: Word -> Word -> Bool
eqWord (W# x) (W# y) = isTrue# (eqWord# x y)
neWord (W# x) (W# y) = isTrue# (neWord# x y)
gtWord (W# x) (W# y) = isTrue# (gtWord# x y)
geWord (W# x) (W# y) = isTrue# (geWord# x y)
ltWord (W# x) (W# y) = isTrue# (ltWord# x y)
leWord (W# x) (W# y) = isTrue# (leWord# x y)

eqWord8, neWord8, gtWord8, geWord8, ltWord8, leWord8 :: Word8 -> Word8 -> Bool
eqWord8 (W8# x) (W8# y) = isTrue# (eqWord8# x y)
neWord8 (W8# x) (W8# y) = isTrue# (neWord8# x y)
gtWord8 (W8# x) (W8# y) = isTrue# (gtWord8# x y)
geWord8 (W8# x) (W8# y) = isTrue# (geWord8# x y)
ltWord8 (W8# x) (W8# y) = isTrue# (ltWord8# x y)
leWord8 (W8# x) (W8# y) = isTrue# (leWord8# x y)

eqWord16, neWord16, gtWord16, geWord16, ltWord16, leWord16 :: Word16 -> Word16 -> Bool
eqWord16 (W16# x) (W16# y) = isTrue# (eqWord16# x y)
neWord16 (W16# x) (W16# y) = isTrue# (neWord16# x y)
gtWord16 (W16# x) (W16# y) = isTrue# (gtWord16# x y)
geWord16 (W16# x) (W16# y) = isTrue# (geWord16# x y)
ltWord16 (W16# x) (W16# y) = isTrue# (ltWord16# x y)
leWord16 (W16# x) (W16# y) = isTrue# (leWord16# x y)

eqWord32, neWord32, gtWord32, geWord32, ltWord32, leWord32 :: Word32 -> Word32 -> Bool
eqWord32 (W32# x) (W32# y) = isTrue# (eqWord32# x y)
neWord32 (W32# x) (W32# y) = isTrue# (neWord32# x y)
gtWord32 (W32# x) (W32# y) = isTrue# (gtWord32# x y)
geWord32 (W32# x) (W32# y) = isTrue# (geWord32# x y)
ltWord32 (W32# x) (W32# y) = isTrue# (ltWord32# x y)
leWord32 (W32# x) (W32# y) = isTrue# (leWord32# x y)

eqWord64, neWord64, gtWord64, geWord64, ltWord64, leWord64 :: Word64 -> Word64 -> Bool
eqWord64 (W64# x) (W64# y) = isTrue# (eqWord64# x y)
neWord64 (W64# x) (W64# y) = isTrue# (neWord64# x y)
gtWord64 (W64# x) (W64# y) = isTrue# (gtWord64# x y)
geWord64 (W64# x) (W64# y) = isTrue# (geWord64# x y)
ltWord64 (W64# x) (W64# y) = isTrue# (ltWord64# x y)
leWord64 (W64# x) (W64# y) = isTrue# (leWord64# x y)
