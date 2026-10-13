{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Num.Integer
  ( Integer (..),
    integerCheck#,
    integerFromBigNat#,
    integerFromBigNatNeg#,
    integerToBigNatClamp#,
    integerFromWordNeg#,
    integerFromWord#,
    integerFromNatural,
    integerToNatural,
    integerSqr,
    integerGcd,
    integerGcde#,
    integerLcm,
    integerRecipMod#,
    integerPowMod#,
    integerLog2#,
    integerLogBase#,
    integerShiftL#,
    integerShiftR#,
    integerSizeInBase#,
    integerFromAddr#,
    integerFromAddr,
    integerToAddr#,
    integerFromByteArray#,
    integerToMutableByteArray#,
  )
where

import GHC.Internal.Integer (Integer (..), integerFromMagnitude#, integerLog2#, integerLogBase#)
import GHC.Internal.Integer qualified as Internal
import GHC.Num.BigNat (BigNat (..), BigNat#, bigNatCheck#, bigNatFromAddr#, bigNatFromByteArray#, bigNatFromWord#, bigNatSizeInBase#, bigNatToAddr#, bigNatToMutableByteArray#, bigNatZero)
import GHC.Num.Primitives (Bool#)
import GHC.Prim (Addr#, ByteArray#, Int#, MutableByteArray#, State#, Word#, and#, clz#, indexWordArray#, int2Word#, quotInt#, quotRemWord2#, sizeofByteArray#, timesWord2#, uncheckedShiftRL#, word2Int#, (+#), (-#), (<#), (==#))
import GHC.Prim.Natural (Natural (..), naturalFromInteger#)
import GHC.Types (IO (..))
import Prelude (Eq (..), Integral (..), Num (..), Ord (..), gcd, lcm, odd, otherwise)

-- | Whether an 'Integer' is canonical: a small value is in @IS@, and the
-- magnitude of a large one is canonical.
integerCheck# :: Integer -> Bool#
integerCheck# value =
  case value of
    IS _ -> 1#
    IP magnitude -> largeCheck magnitude (value > maxInt)
    IN magnitude -> largeCheck magnitude (value < minInt)
  where
    maxInt = 9223372036854775807
    minInt = -9223372036854775808
    largeCheck magnitude outsideInt =
      case bigNatCheck# magnitude of
        1# | outsideInt -> 1#
        _ -> 0#

-- | The non-negative 'Integer' with a magnitude.
integerFromBigNat# :: BigNat# -> Integer
integerFromBigNat# = integerFromMagnitude# 1#

-- | The non-positive 'Integer' with a magnitude.
integerFromBigNatNeg# :: BigNat# -> Integer
integerFromBigNatNeg# magnitude = negate (integerFromBigNat# magnitude)

-- | The magnitude of a non-negative 'Integer', and zero for a negative one.
integerToBigNatClamp# :: Integer -> BigNat#
integerToBigNatClamp# value =
  case value of
    IP magnitude -> magnitude
    IS small ->
      case small <# 0# of
        1# -> case bigNatZero of BN# zero -> zero
        _ -> bigNatFromWord# (int2Word# small)
    IN _ -> case bigNatZero of BN# zero -> zero

-- | The magnitude of an 'Integer'.
integerMagnitude# :: Integer -> BigNat#
integerMagnitude# value = integerToBigNatClamp# (abs value)

-- | The non-positive 'Integer' with the magnitude of a word.
integerFromWordNeg# :: Word# -> Integer
integerFromWordNeg# word = negate (integerFromWord# word)

-- | Shift an 'Integer' to the left by a number of bits.
integerShiftL# :: Integer -> Word# -> Integer
integerShiftL# value count = Internal.integerShiftL# value (word2Int# count)

-- | Shift an 'Integer' to the right by a number of bits.
-- The result rounds to negative infinity.
integerShiftR# :: Integer -> Word# -> Integer
integerShiftR# value count = Internal.integerShiftR# value (word2Int# count)

-- | The 'Integer' with the same value as a 'Natural'.
integerFromNatural :: Natural -> Integer
integerFromNatural value =
  case value of
    NS word -> integerFromWord# word
    NB magnitude -> integerFromBigNat# magnitude

-- | The absolute value of an 'Integer' as a 'Natural'.
integerToNatural :: Integer -> Natural
integerToNatural value = naturalFromInteger# (abs value)

-- | The square of an 'Integer'.
integerSqr :: Integer -> Integer
integerSqr value = value * value

-- | The greatest common divisor, which is never negative.
integerGcd :: Integer -> Integer -> Integer
integerGcd = gcd

-- | The least common multiple, which is never negative.
integerLcm :: Integer -> Integer -> Integer
integerLcm = lcm

-- | The extended greatest common divisor: @(# g, x, y #)@ with
-- @a * x + b * y == g@, and @g@ is never negative.
integerGcde# :: Integer -> Integer -> (# Integer, Integer, Integer #)
integerGcde# 0 0 = (# 0, 0, 0 #)
integerGcde# left right =
  case euclid left 1 0 right 0 1 of
    (divisor, x, y)
      | divisor < 0 -> (# negate divisor, negate x, negate y #)
      | otherwise -> (# divisor, x, y #)
  where
    euclid remainder x y 0 _ _ = (remainder, x, y)
    euclid remainder x y remainder' x' y' =
      case quot remainder remainder' of
        quotient -> euclid remainder' x' y' (remainder - quotient * remainder') (x - quotient * x') (y - quotient * y')

-- | The inverse of a number modulo a natural number, in the range from zero
-- to the modulus.  There is no result when the inverse does not exist or
-- the modulus is zero.  Modulo one, the inverse is zero.
integerRecipMod# :: Integer -> Natural -> (# Natural | () #)
integerRecipMod# value modulus =
  case recipMod value (integerFromNatural modulus) of
    (# inverse | #) -> (# integerToNatural inverse | #)
    (# | () #) -> (# | () #)

recipMod :: Integer -> Integer -> (# Integer | () #)
recipMod value modulus
  | modulus == 0 = (# | () #)
  | modulus == 1 = (# 0 | #)
  | otherwise =
      case integerGcde# (mod value modulus) modulus of
        (# divisor, x, _ #)
          | divisor == 1 -> (# mod x modulus | #)
          | otherwise -> (# | () #)

-- | A power modulo a natural number.  A negative exponent uses the inverse
-- of the base.  There is no result when the modulus is zero or when the
-- inverse does not exist.
integerPowMod# :: Integer -> Integer -> Natural -> (# Natural | () #)
integerPowMod# base exponent modulus =
  case integerFromNatural modulus of
    0 -> (# | () #)
    1 -> (# integerToNatural 0 | #)
    modulus'
      | exponent >= 0 -> (# integerToNatural (powMod (mod base modulus') exponent modulus') | #)
      | otherwise ->
          case recipMod base modulus' of
            (# inverse | #) -> (# integerToNatural (powMod inverse (negate exponent) modulus') | #)
            (# | () #) -> (# | () #)

-- The caller supplies a reduced base, a non-negative exponent, and a
-- modulus greater than one. Keep word residues unboxed throughout the loop.
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod base exponent modulus =
  case modulus of
    IS small -> wordPower (int2Word# small)
    IP magnitude ->
      case sizeofByteArray# magnitude ==# 8# of
        1# -> wordPower (indexWordArray# magnitude 0#)
        _ -> powModInteger base exponent modulus 1
    IN _ -> powModInteger base exponent modulus 1
  where
    wordPower divisor =
      Internal.integerFromWord# 1# (powModWord# (int2Word# (Internal.integerToInt# base)) exponent divisor)

-- Both factors are below the modulus. Thus, the product high word is
-- below the modulus, and the wide quotient fits in one word.
mulModWord# :: Word# -> Word# -> Word# -> Word#
mulModWord# left right modulus =
  case timesWord2# left right of
    (# high, low #) ->
      case quotRemWord2# high low modulus of
        (# _, remainder #) -> remainder

powModWord# :: Word# -> Integer -> Word# -> Word#
powModWord# base (IS exponent) modulus =
  case int2Word# exponent of
    word ->
      case powModWordBits# base word modulus 1## (64# -# word2Int# (clz# word)) of
        (# _, result #) -> result
powModWord# base exponent modulus =
  case integerMagnitude# exponent of
    magnitude -> powModWordLimbs# base magnitude modulus 1## 0# (quotInt# (sizeofByteArray# magnitude) 8#)

-- Read each exponent limb once. No quotient array or exponent copy is needed.
powModWordLimbs# :: Word# -> ByteArray# -> Word# -> Word# -> Int# -> Int# -> Word#
powModWordLimbs# base exponent modulus accumulator index count =
  case index ==# count of
    1# -> accumulator
    _ ->
      case indexWordArray# exponent index of
        word ->
          let bits = case index ==# (count -# 1#) of
                1# -> 64# -# word2Int# (clz# word)
                _ -> 64#
           in case powModWordBits# base word modulus accumulator bits of
                (# nextBase, nextAccumulator #) ->
                  powModWordLimbs# nextBase exponent modulus nextAccumulator (index +# 1#) count

powModWordBits# :: Word# -> Word# -> Word# -> Word# -> Int# -> (# Word#, Word# #)
powModWordBits# base exponent modulus accumulator bits =
  case bits of
    0# -> (# base, accumulator #)
    _ ->
      case and# exponent 1## of
        0## -> next accumulator
        _ -> next (mulModWord# accumulator base modulus)
  where
    next result =
      powModWordBits#
        (mulModWord# base base modulus)
        (uncheckedShiftRL# exponent 1#)
        modulus
        result
        (bits -# 1#)

-- Force each reduced product before the next step. The final step needs
-- no square. A shift removes the low exponent bit without division.
powModInteger :: Integer -> Integer -> Integer -> Integer -> Integer
powModInteger !base !exponent modulus !accumulator
  | exponent == 0 = accumulator
  | exponent == 1 = mod (accumulator * base) modulus
  | odd exponent = powModInteger (mod (base * base) modulus) (Internal.integerShiftR# exponent 1#) modulus (mod (accumulator * base) modulus)
  | otherwise = powModInteger (mod (base * base) modulus) (Internal.integerShiftR# exponent 1#) modulus accumulator

-- | The number of digits of the absolute value in a base greater than one.
-- Zero has no digit.
integerSizeInBase# :: Word# -> Integer -> Word#
integerSizeInBase# base value = bigNatSizeInBase# base (integerMagnitude# value)

-- | Read a non-negative 'Integer' from a number of bytes at an address.
-- With the endianness @1#@, the most significant byte is first.
integerFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, Integer #)
integerFromAddr# count address endian state =
  case bigNatFromAddr# count address endian state of
    (# state1, magnitude #) -> (# state1, integerFromBigNat# magnitude #)

-- | Read a non-negative 'Integer' from a number of bytes at an address in
-- 'IO'.  With the endianness @1#@, the most significant byte is first.
integerFromAddr :: Word# -> Addr# -> Bool# -> IO Integer
integerFromAddr count address endian = IO (integerFromAddr# count address endian)

-- | Write the bytes of the absolute value to an address and give their
-- number.  With the endianness @1#@, the most significant byte is first.
integerToAddr# :: Integer -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
integerToAddr# value = bigNatToAddr# (integerMagnitude# value)

-- | Read a non-negative 'Integer' from a number of bytes at an offset of a
-- byte array.  With the endianness @1#@, the most significant byte is first.
integerFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> State# s -> (# State# s, Integer #)
integerFromByteArray# count bytes offset endian state =
  case bigNatFromByteArray# count bytes offset endian state of
    (# state1, magnitude #) -> (# state1, integerFromBigNat# magnitude #)

-- | Write the bytes of the absolute value to an offset of a mutable byte
-- array and give their number.  With the endianness @1#@, the most
-- significant byte is first.
integerToMutableByteArray# :: Integer -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
integerToMutableByteArray# value = bigNatToMutableByteArray# (integerMagnitude# value)

-- | The non-negative integer with the value of a machine word.
integerFromWord# :: Word# -> Integer
integerFromWord# = Internal.integerFromWord# 1#
