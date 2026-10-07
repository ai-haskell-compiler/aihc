{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Num.BigNat
  ( BigNat (..),
    BigNat#,
    bigNatCheck#,
    bigNatZero,
    bigNatOne,
    bigNatFromWord#,
    bigNatFromWord2#,
    bigNatFromWordArray,
    bigNatToWord#,
    bigNatToInt#,
    bigNatIndex#,
    bigNatSize#,
    bigNatSizeInBase#,
    bigNatSizeInBase,
    bigNatIsZero,
    bigNatIsOne,
    bigNatAdd,
    bigNatAddWord#,
    bigNatSub,
    bigNatSubWord#,
    bigNatMul,
    bigNatMulWord#,
    bigNatSqr,
    bigNatQuotRem#,
    bigNatQuotRemWord#,
    bigNatQuot,
    bigNatQuotWord#,
    bigNatRem,
    bigNatRemWord#,
    bigNatGcd,
    bigNatGcdWord#,
    bigNatAnd,
    bigNatOr,
    bigNatXor,
    bigNatShiftL#,
    bigNatShiftR#,
    bigNatTestBit#,
    bigNatBit#,
    bigNatSetBit#,
    bigNatClearBit#,
    bigNatComplementBit#,
    bigNatPopCount#,
    bigNatCompare,
    bigNatCompareWord#,
    bigNatEq,
    bigNatEq#,
    bigNatEqWord#,
    bigNatGtWord#,
    bigNatFromAddr#,
    bigNatToAddr#,
    bigNatFromByteArray#,
    bigNatToMutableByteArray#,
  )
where

import Data.Bits (bit, clearBit, complementBit, popCount, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import GHC.Num.Primitives (Bool#)
import GHC.Prim
  ( Addr#,
    ByteArray#,
    Int#,
    MutableByteArray#,
    State#,
    Word#,
    Word8#,
    compareByteArrays#,
    eqWord#,
    indexWord8Array#,
    indexWord8OffAddr#,
    indexWordArray#,
    int2Word#,
    neWord#,
    newByteArray#,
    plusWord#,
    quotInt#,
    realWorld#,
    remInt#,
    sizeofByteArray#,
    unsafeFreezeByteArray#,
    word2Int#,
    word8ToWord#,
    wordToWord8#,
    writeWord8Array#,
    writeWord8OffAddr#,
    writeWordArray#,
    (*#),
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Prim.Integer (Integer (..), integerAnd, integerFromMagnitude#, integerFromTwoWords#, integerFromWord#, integerLogBase#, integerOr, integerToInt#, integerXor)
import GHC.Types (Bool (..), Int (..), Word (..), isTrue#)
import Prelude (Eq (..), Integral (..), Num (..), Ord (..), Ordering, gcd, not, otherwise)

-- | The magnitude of an arbitrary-precision number: a canonical,
-- little-endian sequence of 64-bit limbs with no trailing zero limb.  This is
-- the payload that 'GHC.Num.Integer.Integer' carries in @IP@ and @IN@ and
-- that 'GHC.Num.Natural.Natural' carries in @NB@.
type BigNat# = ByteArray#

-- | Lifted wrapper for a 'BigNat#'.
--
-- The magnitude itself is unlifted, so it cannot be stored directly in
-- ordinary lifted data structures or passed to a class method.  This wrapper
-- is the representation packages such as @hashable@ match on.
data BigNat = BN# {unBigNat :: BigNat#}

-- | A magnitude is canonical, so two of them denote the same number exactly
-- when they hold the same bytes.
instance Eq BigNat where
  BN# left == BN# right = bigNatEq left right

  BN# left /= BN# right = not (bigNatEq left right)

-- | Whether a magnitude is canonical: whole limbs and no trailing zero limb.
bigNatCheck# :: BigNat# -> Bool#
bigNatCheck# magnitude =
  case remInt# (sizeofByteArray# magnitude) 8# of
    0# -> case bigNatSize# magnitude of
      0# -> 1#
      size -> neWord# (indexWordArray# magnitude (size -# 1#)) (int2Word# 0#)
    _ -> 0#

-- | The magnitude zero, which has no limb.
bigNatZero :: BigNat
bigNatZero =
  case newByteArray# 0# realWorld# of
    (# state, mutable #) ->
      case unsafeFreezeByteArray# mutable state of
        (# _, magnitude #) -> BN# magnitude

-- | The magnitude one.
bigNatOne :: BigNat
bigNatOne = BN# (bigNatFromWord# (int2Word# 1#))

-- | The magnitude of a word: no limb for zero, one limb otherwise.
bigNatFromWord# :: Word# -> BigNat#
bigNatFromWord# word =
  case eqWord# word (int2Word# 0#) of
    1# -> case bigNatZero of BN# zero -> zero
    _ ->
      case newByteArray# 8# realWorld# of
        (# state, mutable #) ->
          case writeWordArray# mutable 0# word state of
            state1 ->
              case unsafeFreezeByteArray# mutable state1 of
                (# _, magnitude #) -> magnitude

-- | The magnitude of a two-limb number, the high limb first.
bigNatFromWord2# :: Word# -> Word# -> BigNat#
bigNatFromWord2# high low = integerMagnitude# (integerFromTwoWords# 1# high low)

-- | The magnitude of the first limbs of a word array, least significant
-- limb first.  The limbs do not have to be canonical.
bigNatFromWordArray :: ByteArray# -> Word# -> BigNat
bigNatFromWordArray limbs count = BN# (integerMagnitude# (limbsInteger limbs (word2Int# count -# 1#) 0))

limbsInteger :: ByteArray# -> Int# -> Integer -> Integer
limbsInteger limbs index accumulator =
  case index <# 0# of
    1# -> accumulator
    _ -> limbsInteger limbs (index -# 1#) (shiftL accumulator 64 .|. wordInteger (indexWordArray# limbs index))

-- | The least significant limb, and zero for the magnitude zero.
bigNatToWord# :: BigNat# -> Word#
bigNatToWord# magnitude =
  case bigNatSize# magnitude of
    0# -> int2Word# 0#
    _ -> indexWordArray# magnitude 0#

-- | The least significant limb read as an 'Int#'.
bigNatToInt# :: BigNat# -> Int#
bigNatToInt# magnitude = word2Int# (bigNatToWord# magnitude)

-- | The limb at an index.
bigNatIndex# :: BigNat# -> Int# -> Word#
bigNatIndex# = indexWordArray#

-- | The number of limbs.
bigNatSize# :: BigNat# -> Int#
bigNatSize# magnitude = quotInt# (sizeofByteArray# magnitude) 8#

-- | The number of digits in a base greater than one.  Zero has no digit.
bigNatSizeInBase# :: Word# -> BigNat# -> Word#
bigNatSizeInBase# base magnitude =
  case bigNatSize# magnitude of
    0# -> int2Word# 0#
    _ -> plusWord# (integerLogBase# (wordInteger base) (magnitudeInteger magnitude)) (int2Word# 1#)

-- | The number of digits of a magnitude in a base, as a lifted 'Word'.
-- Zero has no digit.
bigNatSizeInBase :: Word -> BigNat# -> Word
bigNatSizeInBase (W# base) magnitude = W# (bigNatSizeInBase# base magnitude)

-- | Whether the magnitude is zero.
bigNatIsZero :: BigNat# -> Bool
bigNatIsZero magnitude = isTrue# (bigNatSize# magnitude ==# 0#)

-- | Whether the magnitude is one.
bigNatIsOne :: BigNat# -> Bool
bigNatIsOne magnitude =
  case bigNatSize# magnitude ==# 1# of
    1# -> isTrue# (eqWord# (indexWordArray# magnitude 0#) (int2Word# 1#))
    _ -> False

-- | The sum of two magnitudes.
bigNatAdd :: BigNat# -> BigNat# -> BigNat#
bigNatAdd left right = integerMagnitude# (magnitudeInteger left + magnitudeInteger right)

-- | The sum of a magnitude and a word.
bigNatAddWord# :: BigNat# -> Word# -> BigNat#
bigNatAddWord# left right = integerMagnitude# (magnitudeInteger left + wordInteger right)

-- | The difference of two magnitudes, or nothing when it is negative.
bigNatSub :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)
bigNatSub left right = naturalDifference (magnitudeInteger left - magnitudeInteger right)

-- | The difference of a magnitude and a word, or nothing when it is
-- negative.
bigNatSubWord# :: BigNat# -> Word# -> (# (# #) | BigNat# #)
bigNatSubWord# left right = naturalDifference (magnitudeInteger left - wordInteger right)

naturalDifference :: Integer -> (# (# #) | BigNat# #)
naturalDifference difference
  | difference < 0 = (# (# #) | #)
  | otherwise = (# | integerMagnitude# difference #)

-- | The product of two magnitudes.
bigNatMul :: BigNat# -> BigNat# -> BigNat#
bigNatMul left right = integerMagnitude# (magnitudeInteger left * magnitudeInteger right)

-- | The product of a magnitude and a word.
bigNatMulWord# :: BigNat# -> Word# -> BigNat#
bigNatMulWord# left right = integerMagnitude# (magnitudeInteger left * wordInteger right)

-- | The square of a magnitude.
bigNatSqr :: BigNat# -> BigNat#
bigNatSqr magnitude = bigNatMul magnitude magnitude

-- | The quotient and the remainder of two magnitudes.
bigNatQuotRem# :: BigNat# -> BigNat# -> (# BigNat#, BigNat# #)
bigNatQuotRem# left right =
  case quotRem (magnitudeInteger left) (magnitudeInteger right) of
    (quotient, remainder) -> (# integerMagnitude# quotient, integerMagnitude# remainder #)

-- | The quotient and the remainder of a magnitude and a word.
bigNatQuotRemWord# :: BigNat# -> Word# -> (# BigNat#, Word# #)
bigNatQuotRemWord# left right =
  case quotRem (magnitudeInteger left) (wordInteger right) of
    (quotient, remainder) -> (# integerMagnitude# quotient, integerWord# remainder #)

-- | The quotient of two magnitudes.
bigNatQuot :: BigNat# -> BigNat# -> BigNat#
bigNatQuot left right = integerMagnitude# (quot (magnitudeInteger left) (magnitudeInteger right))

-- | The quotient of a magnitude and a word.
bigNatQuotWord# :: BigNat# -> Word# -> BigNat#
bigNatQuotWord# left right = integerMagnitude# (quot (magnitudeInteger left) (wordInteger right))

-- | The remainder of two magnitudes.
bigNatRem :: BigNat# -> BigNat# -> BigNat#
bigNatRem left right = integerMagnitude# (rem (magnitudeInteger left) (magnitudeInteger right))

-- | The remainder of a magnitude and a word.
bigNatRemWord# :: BigNat# -> Word# -> Word#
bigNatRemWord# left right = integerWord# (rem (magnitudeInteger left) (wordInteger right))

-- | The greatest common divisor of two magnitudes.
bigNatGcd :: BigNat# -> BigNat# -> BigNat#
bigNatGcd left right = integerMagnitude# (gcd (magnitudeInteger left) (magnitudeInteger right))

-- | The greatest common divisor of a magnitude and a word.
bigNatGcdWord# :: BigNat# -> Word# -> Word#
bigNatGcdWord# left right = integerWord# (gcd (magnitudeInteger left) (wordInteger right))

-- | The bitwise and of two magnitudes.
bigNatAnd :: BigNat# -> BigNat# -> BigNat#
bigNatAnd left right = integerMagnitude# (integerAnd (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- | The bitwise or of two magnitudes.
bigNatOr :: BigNat# -> BigNat# -> BigNat#
bigNatOr left right = integerMagnitude# (integerOr (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- | The bitwise exclusive or of two magnitudes.
bigNatXor :: BigNat# -> BigNat# -> BigNat#
bigNatXor left right = integerMagnitude# (integerXor (integerFromMagnitude# 1# left) (integerFromMagnitude# 1# right))

-- | A magnitude shifted to the left by a number of bits.
bigNatShiftL# :: BigNat# -> Word# -> BigNat#
bigNatShiftL# magnitude count = integerMagnitude# (shiftL (magnitudeInteger magnitude) (wordInt count))

-- | A magnitude shifted to the right by a number of bits.
bigNatShiftR# :: BigNat# -> Word# -> BigNat#
bigNatShiftR# magnitude count = integerMagnitude# (shiftR (magnitudeInteger magnitude) (wordInt count))

-- | Whether a bit of a magnitude is set.
bigNatTestBit# :: BigNat# -> Word# -> Bool#
bigNatTestBit# magnitude index = boolInt# (testBit (magnitudeInteger magnitude) (wordInt index))

-- | The magnitude with only one bit set.
bigNatBit# :: Word# -> BigNat#
bigNatBit# index = integerMagnitude# (bit (wordInt index))

-- | A magnitude with a bit set.
bigNatSetBit# :: BigNat# -> Word# -> BigNat#
bigNatSetBit# magnitude index = integerMagnitude# (setBit (magnitudeInteger magnitude) (wordInt index))

-- | A magnitude with a bit cleared.
bigNatClearBit# :: BigNat# -> Word# -> BigNat#
bigNatClearBit# magnitude index = integerMagnitude# (clearBit (magnitudeInteger magnitude) (wordInt index))

-- | A magnitude with a bit inverted.
bigNatComplementBit# :: BigNat# -> Word# -> BigNat#
bigNatComplementBit# magnitude index = integerMagnitude# (complementBit (magnitudeInteger magnitude) (wordInt index))

-- | The number of set bits.
bigNatPopCount# :: BigNat# -> Word#
bigNatPopCount# magnitude =
  case popCount (magnitudeInteger magnitude) of
    I# count -> int2Word# count

-- | The order of two magnitudes.
bigNatCompare :: BigNat# -> BigNat# -> Ordering
bigNatCompare left right = compare (magnitudeInteger left) (magnitudeInteger right)

-- | The order of a magnitude and a word.
bigNatCompareWord# :: BigNat# -> Word# -> Ordering
bigNatCompareWord# left right = compare (magnitudeInteger left) (wordInteger right)

-- | Whether two magnitudes are equal.  A magnitude is canonical, so this
-- compares the bytes.
bigNatEq :: BigNat# -> BigNat# -> Bool
bigNatEq left right = isTrue# (bigNatEq# left right)

-- | Whether two magnitudes are equal, as a 'Bool#'.
bigNatEq# :: BigNat# -> BigNat# -> Bool#
bigNatEq# left right =
  case sizeofByteArray# left ==# sizeofByteArray# right of
    0# -> 0#
    _ -> compareByteArrays# left 0# right 0# (sizeofByteArray# left) ==# 0#

-- | Whether a magnitude is equal to a word.
bigNatEqWord# :: BigNat# -> Word# -> Bool#
bigNatEqWord# left right = boolInt# (magnitudeInteger left == wordInteger right)

-- | Whether a magnitude is greater than a word.
bigNatGtWord# :: BigNat# -> Word# -> Bool#
bigNatGtWord# left right = boolInt# (magnitudeInteger left > wordInteger right)

-- | Read a magnitude from a number of bytes at an address.  With the
-- endianness @1#@, the most significant byte is first.
bigNatFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, BigNat# #)
bigNatFromAddr# count address endian state =
  case integerMagnitude# (readBytes (addressByte address) (word2Int# count) endian 0# 0) of
    magnitude -> (# state, magnitude #)

-- | Read a magnitude from a number of bytes at an offset of a byte array.
-- With the endianness @1#@, the most significant byte is first.
bigNatFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> State# s -> (# State# s, BigNat# #)
bigNatFromByteArray# count bytes offset endian state =
  case integerMagnitude# (readBytes (arrayByte bytes (word2Int# offset)) (word2Int# count) endian 0# 0) of
    magnitude -> (# state, magnitude #)

-- | Write the bytes of a magnitude to an address and give their number.
-- Zero has no byte.  With the endianness @1#@, the most significant byte
-- is first.
bigNatToAddr# :: BigNat# -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
bigNatToAddr# magnitude address =
  writeBytes (writeWord8OffAddr# address) (magnitudeInteger magnitude) (word2Int# (bigNatSizeInBase# (int2Word# 256#) magnitude))

-- | Write the bytes of a magnitude to an offset of a mutable byte array and
-- give their number.  Zero has no byte.  With the endianness @1#@, the most
-- significant byte is first.
bigNatToMutableByteArray# :: BigNat# -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
bigNatToMutableByteArray# magnitude bytes offset =
  writeBytes (offsetWrite bytes (word2Int# offset)) (magnitudeInteger magnitude) (word2Int# (bigNatSizeInBase# (int2Word# 256#) magnitude))

addressByte :: Addr# -> Int# -> Word#
addressByte address index = word8ToWord# (indexWord8OffAddr# address index)

arrayByte :: ByteArray# -> Int# -> Int# -> Word#
arrayByte bytes offset index = word8ToWord# (indexWord8Array# bytes (offset +# index))

offsetWrite :: MutableByteArray# s -> Int# -> Int# -> Word8# -> State# s -> State# s
offsetWrite bytes offset index = writeWord8Array# bytes (offset +# index)

-- | The number that a sequence of bytes denotes.
readBytes :: (Int# -> Word#) -> Int# -> Bool# -> Int# -> Integer -> Integer
readBytes byteAt count endian index accumulator =
  case index ==# count of
    1# -> accumulator
    _ ->
      let position = case endian of
            1# -> index
            _ -> count -# 1# -# index
       in readBytes byteAt count endian (index +# 1#) (shiftL accumulator 8 .|. wordInteger (byteAt position))

-- | Write the bytes of a non-negative number, the least significant byte
-- first, and give their number.
writeBytes :: (Int# -> Word8# -> State# s -> State# s) -> Integer -> Int# -> Bool# -> State# s -> (# State# s, Word# #)
writeBytes store value count endian state =
  case writeBytesFrom store value count endian 0# state of
    state1 -> (# state1, int2Word# count #)

writeBytesFrom :: (Int# -> Word8# -> State# s -> State# s) -> Integer -> Int# -> Bool# -> Int# -> State# s -> State# s
writeBytesFrom store value count endian index state =
  case index ==# count of
    1# -> state
    _ ->
      let position = case endian of
            1# -> count -# 1# -# index
            _ -> index
       in case store position (wordToWord8# (integerWord# (shiftR value (I# (index *# 8#)) .&. 255))) state of
            state1 -> writeBytesFrom store value count endian (index +# 1#) state1

-- The arithmetic reuses the limb code behind 'Integer', so a magnitude goes
-- through a non-negative 'Integer' and comes back out of it.
magnitudeInteger :: BigNat# -> Integer
magnitudeInteger = integerFromMagnitude# 1#

integerMagnitude# :: Integer -> BigNat#
integerMagnitude# value =
  case value of
    IS small -> bigNatFromWord# (int2Word# small)
    IP magnitude -> magnitude
    IN magnitude -> magnitude

wordInteger :: Word# -> Integer
wordInteger = integerFromWord# 1#

-- | The least significant limb of a non-negative 'Integer'.
integerWord# :: Integer -> Word#
integerWord# value = int2Word# (integerToInt# value)

wordInt :: Word# -> Int
wordInt word = I# (word2Int# word)

boolInt# :: Bool -> Int#
boolInt# True = 1#
boolInt# False = 0#
