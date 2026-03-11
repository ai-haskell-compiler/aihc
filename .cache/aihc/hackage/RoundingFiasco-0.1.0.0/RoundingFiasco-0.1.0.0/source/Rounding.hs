
{-|

This module provides various combinations of explicit floating point operations.

The are three supported rounding variants:

  * ceil (rounding towards positive infinity)

  * floor (rounding towards negative infinity)

  * truncate (rounding towards zero)

Operators:

  * `+` add

  * `-` subtract

  * `*` multiplicate

  * `/` divide

  * `sqrt` squareRoot

  * `id` converting numbers between formats

The floating point data types:

  * `Float` (single precision)

  * `Double` (double precision)

The behaviour for `NaN`s is very hardware dependent. In case an operation would result in a `NaN` the corresponding round-to-nearest-even-on-tie variant is used to get the result.

-}



{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}



module Rounding

  -- * Arithmetic Signum

  ( i32_arithmic_signum_f64
  , i32_arithmic_signum_f64#
  , i32_arithmic_signum_f32
  , i32_arithmic_signum_f32#

  -- * Sign bit

  , i32_sign_bit_f64
  , i32_sign_bit_f64#
  , i32_sign_bit_f32
  , i32_sign_bit_f32#

  -- * Bit pattern neighbours

  , f64_successorIEEE
  , f64_successorIEEE#
  , f64_predecessorIEEE
  , f64_predecessorIEEE#

  , f32_successorIEEE
  , f32_successorIEEE#
  , f32_predecessorIEEE
  , f32_predecessorIEEE#

  -- * Ceil variants

  {-|

  Operators of the ceil variant operate as if they calculated the exact mathematic result. Then they take the minimum of all floating point numbers that are greater or equal to the exact result. Special care is taken for the sign of zero.

  Should the result from a plus or minus operation be zero the sign is positive.

  -}

  , f32_squareRoot_ceil
  , f32_add_ceil
  , f32_subtract_ceil
  , f32_multiplicate_ceil
  , f32_divide_ceil
  , f64_squareRoot_ceil
  , f64_add_ceil
  , f64_subtract_ceil
  , f64_multiplicate_ceil
  , f64_divide_ceil
  , f32_convert_i32_signed_ceil
  , f32_convert_i32_unsigned_ceil
  , f32_convert_i64_signed_ceil
  , f32_convert_i64_unsigned_ceil
  , f32_demote_f64_ceil
  , f64_convert_i32_signed_ceil
  , f64_convert_i32_unsigned_ceil
  , f64_convert_i64_signed_ceil
  , f64_convert_i64_unsigned_ceil
  , f64_promote_f32_ceil

  -- * Floor variants

  {-|

  The operators operate as if they calculate the exact mathematic result. Then they take the maximum of all floating point numbers that are less or equal to the exact result.

  Should the result from a plus or minus operation be zero the sign is negative.

  -}

  , f32_squareRoot_floor
  , f32_add_floor
  , f32_subtract_floor
  , f32_multiplicate_floor
  , f32_divide_floor
  , f64_squareRoot_floor
  , f64_add_floor
  , f64_subtract_floor
  , f64_multiplicate_floor
  , f64_divide_floor
  , f32_convert_i32_signed_floor
  , f32_convert_i32_unsigned_floor
  , f32_convert_i64_signed_floor
  , f32_convert_i64_unsigned_floor
  , f32_demote_f64_floor
  , f64_convert_i32_signed_floor
  , f64_convert_i32_unsigned_floor
  , f64_convert_i64_signed_floor
  , f64_convert_i64_unsigned_floor
  , f64_promote_f32_floor

  -- * Truncate variants

  {-|

  Operators of the truncate variant operate as if they calculated the exact result. Then they take the result from the floor variant in case the exact result is greater than zero. Should the exact result be zero or less the ceil variant is choosen.

  -}

  , f32_squareRoot_truncate
  , f32_add_truncate
  , f32_subtract_truncate
  , f32_multiplicate_truncate
  , f32_divide_truncate
  , f64_squareRoot_truncate
  , f64_add_truncate
  , f64_subtract_truncate
  , f64_multiplicate_truncate
  , f64_divide_truncate
  , f32_convert_i32_signed_truncate
  , f32_convert_i32_unsigned_truncate
  , f32_convert_i64_signed_truncate
  , f32_convert_i64_unsigned_truncate
  , f32_demote_f64_truncate
  , f64_convert_i32_signed_truncate
  , f64_convert_i32_unsigned_truncate
  , f64_convert_i64_signed_truncate
  , f64_convert_i64_unsigned_truncate
  , f64_promote_f32_truncate

  ) where



import GHC.Exts
import GHC.Float
import GHC.Int
import Data.Word


-- | There are three possible outputs:
--
-- prop> i32_arithmic_signum_f64 (-1.0) = -1
-- prop> i32_arithmic_signum_f64 (-0.0) = 0
-- prop> i32_arithmic_signum_f64 (0.0) = 0
-- prop> i32_arithmic_signum_f64 (1.0) = 1
--
-- @`NaN`@ values are mapped to @`0`@:
--
-- prop> i32_arithmic_signum_f64 (0/0) = 0
{-# INLINABLE i32_arithmic_signum_f64 #-}
i32_arithmic_signum_f64 :: Double -> Int32
i32_arithmic_signum_f64 (D# x) = I32# (intToInt32# (i32_arithmic_signum_f64# x))
{-# INLINABLE i32_arithmic_signum_f32 #-}
i32_arithmic_signum_f32 :: Float -> Int32
i32_arithmic_signum_f32 (F# x) = I32# (intToInt32# (i32_arithmic_signum_f32# x))

-- https://stackoverflow.com/questions/1903954/is-there-a-standard-sign-function-signum-sgn-in-c-c
{-# INLINABLE i32_arithmic_signum_f64# #-}
i32_arithmic_signum_f64# :: Double# -> Int#
i32_arithmic_signum_f64# x = (0.0## <## x) -# (x <## 0.0##)
{-# INLINABLE i32_arithmic_signum_f32# #-}
i32_arithmic_signum_f32# :: Float# -> Int#
i32_arithmic_signum_f32# x = (0.0# `ltFloat#` x) -# (x `ltFloat#` 0.0#)


-- | There are two possible outputs:
--
-- prop> i32_sign_bit_f64 (-1.0) = 1
-- prop> i32_sign_bit_f64 (-0.0) = 1
-- prop> i32_sign_bit_f64 (0.0) = 0
-- prop> i32_sign_bit_f64 (1.0) = 0
--
-- All @`NaN`@s have a proper sign:
--
-- prop> 1 = i32_sign_bit_f64 (0/0)
-- prop> 0 = i32_sign_bit_f64 (castWord64ToDouble 0b0111111111111000000000000000000000000000000000000000000000000000)
{-# INLINABLE i32_sign_bit_f64 #-}
i32_sign_bit_f64 :: Double -> Int32
i32_sign_bit_f64 (D# x) = I32# (i32_sign_bit_f64# x)

{-# INLINABLE i32_sign_bit_f64# #-}
i32_sign_bit_f64# :: Double# -> Int32#
i32_sign_bit_f64# value = result
  where
    value_as_word64 :: Word64#
    value_as_word64 = stgDoubleToWord64 value

    sign_bit :: Word64#
    sign_bit
      = 0b1000000000000000000000000000000000000000000000000000000000000000#Word64

    sign_bit_of_value :: Word64#
    sign_bit_of_value = and64# sign_bit value_as_word64

    result
      = case sign_bit_of_value of
          0b1000000000000000000000000000000000000000000000000000000000000000#Word64
            -> 0b1#Int32
          0b0000000000000000000000000000000000000000000000000000000000000000#Word64
            -> 0b0#Int32
          _ -> 0b0#Int32 -- error "cannot happen"



{-# INLINABLE i32_sign_bit_f32 #-}
i32_sign_bit_f32 :: Float -> Int32
i32_sign_bit_f32 (F# x) = I32# (i32_sign_bit_f32# x)

{-# INLINABLE i32_sign_bit_f32# #-}
i32_sign_bit_f32# :: Float# -> Int32#
i32_sign_bit_f32# value = result
  where
    value_as_word32 :: Word32#
    value_as_word32 = stgFloatToWord32 value

    sign_bit :: Word32#
    sign_bit
      = 0b10000000000000000000000000000000#Word32

    sign_bit_of_value :: Word32#
    sign_bit_of_value = andWord32# sign_bit value_as_word32

    result
      = case sign_bit_of_value of
          0b10000000000000000000000000000000#Word32
            -> 0b1#Int32
          0b00000000000000000000000000000000#Word32
            -> 0b0#Int32
          _ -> 0b0#Int32 -- error "cannot happen"


-----------



{-# INLINABLE f64_successorIEEE# #-}
f64_successorIEEE#
  :: Double#
  -> Double#
f64_successorIEEE#
  value
  = result
  where
    result = resulting_double

    value_as_word64 :: Word64#
    value_as_word64 = stgDoubleToWord64 value

    sign_bit :: Word64#
    sign_bit
      = 0b1000000000000000000000000000000000000000000000000000000000000000#Word64

    exponent_bits :: Word64#
    exponent_bits
      = 0b0111111111110000000000000000000000000000000000000000000000000000#Word64

    _negativ_infinity :: Word64#
    _negativ_infinity
      = 0b1111111111110000000000000000000000000000000000000000000000000000#Word64

    _least_finit_value :: Word64#
    _least_finit_value
      = 0b1111111111101111111111111111111111111111111111111111111111111111#Word64
      -- -1.7976931348623157e308##

    _least_positiv_value :: Word64#
    _least_positiv_value
      = 0b0000000000000000000000000000000000000000000000000000000000000001#Word64
      -- 5.0e-324##

    _negative_zero = sign_bit

    exponent_bits_of_value :: Word64#
    exponent_bits_of_value = and64# exponent_bits value_as_word64

    sign_bit_of_value :: Word64#
    sign_bit_of_value = and64# sign_bit value_as_word64

    resulting_double
      = case exponent_bits_of_value of
          {- not real value or not negativ zero -}
          0b0111111111110000000000000000000000000000000000000000000000000000#Word64 {-exponent_bits-}
            -> case value_as_word64 of
                0b1111111111110000000000000000000000000000000000000000000000000000#Word64 {-negativ_infinity-}
                  -> -1.7976931348623157e308## {-least_finit_value-}
                _
                  -> value --not real values stay as they are (except for negativ_infinity)

          {- real value or negativ zero -}
          _
            -> case sign_bit_of_value of
                {-positive real value-}
                0b0000000000000000000000000000000000000000000000000000000000000000#Word64
                  -> stgWord64ToDouble (plusWord64# 1#Word64 value_as_word64)

                {-negative real value-}
                _
                  -> case value_as_word64 of
                      {-negative_zero-}
                      0b1000000000000000000000000000000000000000000000000000000000000000#Word64 {-negative_zero-}
                        -> 5.0e-324## {-least_positiv_value-}

                      {-proper negativ value-}
                      _
                        -> stgWord64ToDouble (subWord64# value_as_word64 1#Word64)


{-|
This function is supposed to be a pure haskell replacement for `Numeric.IEEE.succIEEE`. ([succIEEE](https://hackage.haskell.org/package/ieee754-0.8.0/docs/Numeric-IEEE.html#v:succIEEE))

Floating point numbers of the same sign have canonical ordered bitpatterns. This means that first a float that represents a real number can first be reinterpeted as an integer. Then one can add or subtract `1` inorder to get the two floating point neighbours. Special cases are infinit values and negative and positive zero. @`NaN`@s are returned unchanged.

prop> f64_successorIEEE (-1/0) = -1.7976931348623157e308
prop> f64_successorIEEE (-0.0) = castWord64ToDouble 1
prop> f64_successorIEEE (0.0) = castWord64ToDouble 1
prop> f64_successorIEEE (castWord64ToDouble 1) = castWord64ToDouble 2
prop> f64_successorIEEE (castWord64ToDouble 2) = castWord64ToDouble 3
prop> f64_successorIEEE (1.7976931348623157e308) = 1/0
-}
{-# INLINABLE f64_successorIEEE #-}
f64_successorIEEE
  :: Double
  -> Double
f64_successorIEEE
  (D# value#)
  = D# (f64_successorIEEE# value#)




{-# INLINABLE f64_predecessorIEEE# #-}
f64_predecessorIEEE#
  :: Double#
  -> Double#
f64_predecessorIEEE#
  value
  = symetric_result
  where
    symetric_result
      = negateDouble#
      $# f64_successorIEEE#
      $# negateDouble#
      $# value

    infixr 0 $#
    ($#) :: (Double# -> Double#) -> Double# -> Double#
    f $# x = f x


{-# INLINABLE f64_predecessorIEEE #-}
f64_predecessorIEEE
  :: Double
  -> Double
f64_predecessorIEEE
  (D# value#)
  = D# (f64_predecessorIEEE# value#)





------------




{-# INLINABLE f32_successorIEEE# #-}
f32_successorIEEE#
  :: Float#
  -> Float#
f32_successorIEEE#
  value
  = result
  where
    result = resulting_float

    value_as_word64 :: Word32# --TODO recent GHC returns Word32#
    value_as_word64 = stgFloatToWord32 value

    sign_bit :: Word32#
    sign_bit
      = 0b10000000000000000000000000000000#Word32

    exponent_bits :: Word32#
    exponent_bits
      = 0b01111111100000000000000000000000#Word32

    _negativ_infinity :: Word32#
    _negativ_infinity
      = 0b11111111100000000000000000000000#Word32

    _least_finit_value :: Word32#
    _least_finit_value
      = 0b11111111011111111111111111111111#Word32
      -- -3.4028235e38#

    _least_positiv_value :: Word32#
    _least_positiv_value
      = 0b00000000000000000000000000000001#Word32
      -- 1.0e-45#

    _negative_zero = sign_bit

    exponent_bits_of_value :: Word32#
    exponent_bits_of_value = andWord32# exponent_bits value_as_word64

    sign_bit_of_value :: Word32#
    sign_bit_of_value = andWord32# sign_bit value_as_word64

    resulting_float
      = case exponent_bits_of_value of
          {- not real value or not negativ zero -}
          0b01111111100000000000000000000000#Word32 {-exponent_bits-}
            -> case value_as_word64 of
                0b11111111100000000000000000000000#Word32 {-negativ_infinity-}
                  -> -3.4028235e38# {-least_finit_value-}
                _
                  -> value --not real values stay as they are (except for negativ_infinity)

          {- real value or negativ zero -}
          _
            -> case sign_bit_of_value of
                {-positive real value-}
                0b00000000000000000000000000000000#Word32
                  -> stgWord32ToFloat (plusWord32# 1#Word32 value_as_word64)

                {-negative real value-}
                _
                  -> case value_as_word64 of
                      {-negative_zero-}
                      0b10000000000000000000000000000000#Word32 {-negative_zero-}
                        -> 1.0e-45# {-least_positiv_value-}

                      {-proper negativ value-}
                      _
                        -> stgWord32ToFloat (minusWord32# value_as_word64 1#Word32)

    minusWord32# :: Word32# -> Word32# -> Word32#
    minusWord32# x y = wordToWord32# (minusWord# (word32ToWord# x) (word32ToWord# y))


{-# INLINABLE f32_successorIEEE #-}
f32_successorIEEE
  :: Float
  -> Float
f32_successorIEEE
  (F# value#)
  = F# (f32_successorIEEE# value#)



{-# INLINABLE f32_predecessorIEEE# #-}
f32_predecessorIEEE#
  :: Float#
  -> Float#
f32_predecessorIEEE#
  value
  = result
  where
    symetric_result
      = negateFloat#
      $# f32_successorIEEE#
      $# negateFloat#
      $# value

    infixr 0 $#
    ($#) :: (Float# -> Float#) -> Float# -> Float#
    f $# x = f x

    result
      = if isNegativeZero (F# symetric_result)
          then 0.0#
          else symetric_result

{-# INLINABLE f32_predecessorIEEE #-}
f32_predecessorIEEE
  :: Float
  -> Float
f32_predecessorIEEE
  (F# value#)
  = F# (f32_predecessorIEEE# value#)










-----------



-- |
-- prop> (castWord64ToDouble 1) == f32_demote_f64_ceil (castWord64ToDouble 1)
f32_demote_f64_ceil :: Double -> Float
f32_demote_f64_ceil = sanatizeNaNDemotion $ sanatizeNegtiveZeroDemotionPromotion $ convert RoundUp

f32_demote_f64_floor :: Double -> Float
f32_demote_f64_floor = sanatizeNaNDemotion $ sanatizeNegtiveZeroDemotionPromotion $ convert RoundDown

f32_demote_f64_truncate :: Double -> Float
f32_demote_f64_truncate = sanatizeNaNDemotion $ sanatizeNegtiveZeroDemotionPromotion $ convert Truncate



f64_promote_f32_ceil :: Float -> Double
f64_promote_f32_ceil = sanatizeNaNPromotion $ sanatizeNegtiveZeroDemotionPromotion $ convert RoundUp

f64_promote_f32_floor :: Float -> Double
f64_promote_f32_floor = sanatizeNaNPromotion $ sanatizeNegtiveZeroDemotionPromotion $ convert RoundDown

f64_promote_f32_truncate :: Float -> Double
f64_promote_f32_truncate = sanatizeNaNPromotion $ sanatizeNegtiveZeroDemotionPromotion $ convert Truncate




-- |
-- prop> 0.0 = f32_squareRoot_ceil 0.0
-- prop> 1.0 = f32_squareRoot_ceil 1.0
-- prop> (1/0) = f32_squareRoot_ceil (1/0)
-- prop> True = isNaN (f32_squareRoot_ceil (-1/0))
f32_squareRoot_ceil :: Float -> Float
f32_squareRoot_ceil = squareRoot RoundUp

f32_squareRoot_floor :: Float -> Float
f32_squareRoot_floor = squareRoot RoundDown

-- |
-- prop> f32_squareRoot_truncate = f32_squareRoot_floor
f32_squareRoot_truncate :: Float -> Float
f32_squareRoot_truncate = squareRoot Truncate

f64_squareRoot_ceil :: Double -> Double
f64_squareRoot_ceil = squareRoot RoundUp

f64_squareRoot_floor :: Double -> Double
f64_squareRoot_floor = squareRoot RoundDown

f64_squareRoot_truncate :: Double -> Double
f64_squareRoot_truncate = squareRoot Truncate

f32_add_ceil :: Float -> Float -> Float
f32_add_ceil = binary_operator_rounded RoundUp (+)

-- |
-- prop> (-0.0) = f32_add_floor 1 (-1)
-- prop> (-0.0) = f32_add_floor (-1) 1
f32_add_floor :: Float -> Float -> Float
f32_add_floor = sanitizeZeroForAddition $ binary_operator_rounded RoundDown (+)

f32_add_truncate :: Float -> Float -> Float
f32_add_truncate = binary_operator_rounded Truncate (+)

-- |
-- prop> 0.0 == f32_subtract_ceil 1 1
f32_subtract_ceil :: Float -> Float -> Float
f32_subtract_ceil = binary_operator_rounded RoundUp (-)

f32_subtract_floor :: Float -> Float -> Float
f32_subtract_floor = sanitizeZeroForSubtraction $ binary_operator_rounded RoundDown (-)

f32_subtract_truncate :: Float -> Float -> Float
f32_subtract_truncate = binary_operator_rounded Truncate (-)

f32_multiplicate_ceil :: Float -> Float -> Float
f32_multiplicate_ceil = binary_operator_rounded RoundUp (*)

f32_multiplicate_floor :: Float -> Float -> Float
f32_multiplicate_floor = binary_operator_rounded RoundDown (*)

f32_multiplicate_truncate :: Float -> Float -> Float
f32_multiplicate_truncate = binary_operator_rounded Truncate (*)

f32_divide_ceil :: Float -> Float -> Float
f32_divide_ceil = binary_operator_rounded RoundUp (/)

f32_divide_floor :: Float -> Float -> Float
f32_divide_floor = binary_operator_rounded RoundDown (/)

f32_divide_truncate :: Float -> Float -> Float
f32_divide_truncate = binary_operator_rounded Truncate (/)

-- |
-- prop> 0.0 = f64_add_ceil 1 (-1)
f64_add_ceil :: Double -> Double -> Double
f64_add_ceil = binary_operator_rounded RoundUp (+)

f64_add_floor :: Double -> Double -> Double
f64_add_floor = sanitizeZeroForAddition $ binary_operator_rounded RoundDown (+)

f64_add_truncate :: Double -> Double -> Double
f64_add_truncate = binary_operator_rounded Truncate (+)

f64_subtract_ceil :: Double -> Double -> Double
f64_subtract_ceil = binary_operator_rounded RoundUp (-)

f64_subtract_floor :: Double -> Double -> Double
f64_subtract_floor = sanitizeZeroForSubtraction $ binary_operator_rounded RoundDown (-)

f64_subtract_truncate :: Double -> Double -> Double
f64_subtract_truncate = binary_operator_rounded Truncate (-)

f64_multiplicate_ceil :: Double -> Double -> Double
f64_multiplicate_ceil = binary_operator_rounded RoundUp (*)

f64_multiplicate_floor :: Double -> Double -> Double
f64_multiplicate_floor = binary_operator_rounded RoundDown (*)

f64_multiplicate_truncate :: Double -> Double -> Double
f64_multiplicate_truncate = binary_operator_rounded Truncate (*)

f64_divide_ceil :: Double -> Double -> Double
f64_divide_ceil = binary_operator_rounded RoundUp (/)

f64_divide_floor :: Double -> Double -> Double
f64_divide_floor = binary_operator_rounded RoundDown (/)

f64_divide_truncate :: Double -> Double -> Double
f64_divide_truncate = binary_operator_rounded Truncate (/)

f32_convert_i32_unsigned_ceil :: Word32 -> Float
f32_convert_i32_unsigned_ceil = convert RoundUp

f32_convert_i32_signed_ceil :: Int32 -> Float
f32_convert_i32_signed_ceil = convert RoundUp

f32_convert_i32_unsigned_floor :: Word32 -> Float
f32_convert_i32_unsigned_floor = convert RoundDown

f32_convert_i32_signed_floor :: Int32 -> Float
f32_convert_i32_signed_floor = convert RoundDown

f32_convert_i32_unsigned_truncate :: Word32 -> Float
f32_convert_i32_unsigned_truncate = convert Truncate

f32_convert_i32_signed_truncate :: Int32 -> Float
f32_convert_i32_signed_truncate = convert Truncate

f32_convert_i64_unsigned_ceil :: Word64 -> Float
f32_convert_i64_unsigned_ceil = convert RoundUp

f32_convert_i64_signed_ceil :: Int64 -> Float
f32_convert_i64_signed_ceil = convert RoundUp

f32_convert_i64_unsigned_floor :: Word64 -> Float
f32_convert_i64_unsigned_floor = convert RoundDown

f32_convert_i64_signed_floor :: Int64 -> Float
f32_convert_i64_signed_floor = convert RoundDown

f32_convert_i64_unsigned_truncate :: Word64 -> Float
f32_convert_i64_unsigned_truncate = convert Truncate

f32_convert_i64_signed_truncate :: Int64 -> Float
f32_convert_i64_signed_truncate = convert Truncate

f64_convert_i32_unsigned_ceil :: Word32 -> Double
f64_convert_i32_unsigned_ceil = convert RoundUp

f64_convert_i32_signed_ceil :: Int32 -> Double
f64_convert_i32_signed_ceil = convert RoundUp

f64_convert_i32_unsigned_floor :: Word32 -> Double
f64_convert_i32_unsigned_floor = convert RoundDown

f64_convert_i32_signed_floor :: Int32 -> Double
f64_convert_i32_signed_floor = convert RoundDown

f64_convert_i32_unsigned_truncate :: Word32 -> Double
f64_convert_i32_unsigned_truncate = convert Truncate

f64_convert_i32_signed_truncate :: Int32 -> Double
f64_convert_i32_signed_truncate = convert Truncate

f64_convert_i64_unsigned_ceil :: Word64 -> Double
f64_convert_i64_unsigned_ceil = convert RoundUp

f64_convert_i64_signed_ceil :: Int64 -> Double
f64_convert_i64_signed_ceil = convert RoundUp

f64_convert_i64_unsigned_floor :: Word64 -> Double
f64_convert_i64_unsigned_floor = convert RoundDown

f64_convert_i64_signed_floor :: Int64 -> Double
f64_convert_i64_signed_floor = convert RoundDown

f64_convert_i64_unsigned_truncate :: Word64 -> Double
f64_convert_i64_unsigned_truncate = convert Truncate

f64_convert_i64_signed_truncate :: Int64 -> Double
f64_convert_i64_signed_truncate = convert Truncate





---




-- https://stackoverflow.com/questions/28949774/what-is-0-0-by-ieee-floating-point-standard
sanitizeZeroForAddition
  :: (Eq a1, Num a1, RealFloat a2)
  => (a2 -> a2 -> a1)
  -> (a2 -> a2 -> a1)
sanitizeZeroForAddition
  operator
  a
  b
  = if (0 == x) && (sign_bit a * sign_bit b == -1)
      then negate x
      else x
  where
    x = operator a b

    sign_bit y
      = if 0 == y
          then (boolToNumber $ isNegativeZero y)
          else signum y

    boolToNumber True = -1
    boolToNumber False = 1

sanitizeZeroForSubtraction
  :: (Eq a1, RealFloat a2, Num a1)
  => (a2 -> a2 -> a1)
  -> (a2 -> a2 -> a1)
sanitizeZeroForSubtraction
  operator
  a
  b
  = if (0 == x) && (sign_bit a == sign_bit b)
      then negate x
      else x
  where
    x = operator a b

    sign_bit y
      = if 0 == y
          then (boolToNumber $ isNegativeZero y)
          else signum y

    boolToNumber True = -1
    boolToNumber False = 1

sanatizeNegtiveZeroDemotionPromotion
  :: (RealFloat p, Fractional a)
  => (p -> a)
  -> (p -> a)
sanatizeNegtiveZeroDemotionPromotion
  conversion
  input
  = result
  where
    result
      = if isNegativeZero input
          then negate 0.0
          else conversion input

sanatizeNaNDemotion
  :: (Double -> Float)
  -> (Double -> Float)
sanatizeNaNDemotion
  conversion
  input
  = result
  where
    result
      = if isNaN input
          then double2Float input
          else conversion input

sanatizeNaNPromotion
  :: (Float -> Double)
  -> (Float -> Double)
sanatizeNaNPromotion
  conversion
  input
  = result
  where
    result
      = if isNaN input
          then float2Double input
          else conversion input




---------




data RationalWithSpecials
  = NegativInfinity
  | Exisiting Rational
  | PositiveInfinity
  deriving (Show, Eq)

getExisiting :: Rational -> RationalWithSpecials -> Rational
getExisiting _ (Exisiting x) = x
getExisiting defaultValue _ = defaultValue


-- | left: exact result, right: wannebe result
liftComparator
  :: Round
  -> (Rational -> Rational -> Bool)
  -> RationalWithSpecials
  -> RationalWithSpecials
  -> Bool
liftComparator _ sorted (Exisiting x) (Exisiting y) = x `sorted` y

liftComparator RoundDown _ PositiveInfinity PositiveInfinity = True

liftComparator RoundDown _ _ PositiveInfinity = False
liftComparator RoundDown _ _ NegativInfinity = True

liftComparator RoundUp _ PositiveInfinity PositiveInfinity = True
liftComparator RoundUp _ _ PositiveInfinity = True
liftComparator RoundUp _ _ NegativInfinity = False

liftComparator Truncate _ xx@(Exisiting x) PositiveInfinity = if 0 < x then liftComparator RoundDown undefined xx PositiveInfinity else undefined --error "off by sign"
liftComparator Truncate _ xx@(Exisiting x) NegativInfinity = if x < 0 then liftComparator RoundUp undefined xx NegativInfinity else undefined --error "off by sign"

liftComparator Truncate _ PositiveInfinity PositiveInfinity = True

liftComparator _roundDirection _sorted _x _y = undefined --error $ show ("liftComparator", roundDirection, x, y)




liftComparatorConvert
  :: Round
  -> (Rational -> Rational -> Bool)
  -> RationalWithSpecials
  -> RationalWithSpecials
  -> Bool
liftComparatorConvert _ sorted (Exisiting x) (Exisiting y) = x `sorted` y
liftComparatorConvert RoundDown _sorted PositiveInfinity (Exisiting _) = False
liftComparatorConvert RoundDown _ NegativInfinity (Exisiting _) = True

liftComparatorConvert RoundUp _sorted PositiveInfinity (Exisiting _) = True
liftComparatorConvert RoundUp _sorted NegativInfinity (Exisiting _) = False

liftComparatorConvert Truncate _ PositiveInfinity xx@(Exisiting x)
  = if 0 < x
      then liftComparatorConvert RoundDown undefined PositiveInfinity xx
      else undefined --off_by_sign
liftComparatorConvert Truncate _ NegativInfinity xx@(Exisiting x)
  = if x < 0
      then liftComparatorConvert RoundUp undefined NegativInfinity xx
      else undefined --off_by_sign

liftComparatorConvert _ _sorted PositiveInfinity PositiveInfinity = True
liftComparatorConvert _ _sorted NegativInfinity NegativInfinity = True

liftComparatorConvert _roundDirection _sorted _x _y = undefined --error $ show ("liftComparatorConvert", roundDirection, x, y)





liftUnary
  :: (Rational -> Rational)
  -> (RationalWithSpecials -> RationalWithSpecials)
liftUnary f (Exisiting x) = Exisiting $ f x
liftUnary _ PositiveInfinity = PositiveInfinity
liftUnary _ NegativInfinity = PositiveInfinity





class RationalWithSpecialsLike a where
  exactToRational :: {-HasCallStack =>-} a -> RationalWithSpecials


class
  (Show a, Num a, RealFloat a, RationalWithSpecialsLike a)
  => IEEE_RoundingFuckUp_Correctable a
  where
  exactFromRational :: {-HasCallStack =>-} RationalWithSpecials -> a
  nudgeDown :: a -> a
  nudgeUp :: a -> a
  nudgeToZero :: a -> a
  nudgeToZero j
    = if j < 0
        then nudgeUp j
        else if 0 < j
          then nudgeDown j
          else undefined -- error "what now?"
  nudgeAwayFromZero :: a -> a
  nudgeAwayFromZero j
    = if j < 0
        then nudgeDown j
        else if 0 < j
          then nudgeUp j
          else undefined -- error "what now?"


instance RationalWithSpecialsLike Word32 where
  exactToRational = Exisiting . toRational
instance RationalWithSpecialsLike Word64 where
  exactToRational = Exisiting . toRational
instance RationalWithSpecialsLike Int32 where
  exactToRational = Exisiting . toRational
instance RationalWithSpecialsLike Int64 where
  exactToRational = Exisiting . toRational

withFrozenCallStack :: a -> a
withFrozenCallStack = id

assertNote :: a -> b -> c -> c
assertNote _note _check = id

instance IEEE_RoundingFuckUp_Correctable Double where
  exactFromRational = withFrozenCallStack hopefullyExactFromRational
  nudgeDown = f64_predecessorIEEE
  nudgeUp = f64_successorIEEE

instance RationalWithSpecialsLike Double where
  exactToRational = withFrozenCallStack hopeFullyExactToRational


instance IEEE_RoundingFuckUp_Correctable Float where
  exactFromRational = withFrozenCallStack hopefullyExactFromRational
  nudgeDown = f32_predecessorIEEE
  nudgeUp = f32_successorIEEE


instance RationalWithSpecialsLike Float where
  exactToRational = withFrozenCallStack hopeFullyExactToRational

hopeFullyExactToRational
  :: ({-HasCallStack,-} RealFloat a, Show a)
  => a
  -> RationalWithSpecials
hopeFullyExactToRational
  value
  = id
  $ withFrozenCallStack
  $ assertNote note check
  $ result
  where
    result
      = if isInfinite value
          then case signum value of
                  -1 -> NegativInfinity
                  1 -> PositiveInfinity
                  _ -> undefined -- error "infinit has sign 0"
          else if not $ isNaN value
            then Exisiting $ toRational value
            else undefined --error $ "cannot convert to rational: " ++ show value

    note = "this double cannot be represented as Rational: " ++ show value

    check
      = (not $ isNaN $ value)
      && (not $ isNegativeZero $ value)


hopefullyExactFromRational
  :: ({-HasCallStack,-} RealFloat a)
  => RationalWithSpecials
  -> a
hopefullyExactFromRational (Exisiting rational) = fromRational rational
hopefullyExactFromRational (PositiveInfinity) = 1/0
hopefullyExactFromRational (NegativInfinity) = -1/0




------------





data Round = RoundDown | RoundUp | Truncate
  deriving Show


roundingOrder_binaryOperator
  :: Round
  -> (Rational -> Rational -> Bool)
roundingOrder_binaryOperator RoundDown = (>=)
roundingOrder_binaryOperator RoundUp = (<=)
roundingOrder_binaryOperator Truncate = \exact_result allegedly_rounded_result
  -> if exact_result < 0 && allegedly_rounded_result < 0
        then (roundingOrder_binaryOperator RoundUp) exact_result allegedly_rounded_result
        else if 0 < exact_result && 0 < allegedly_rounded_result
                then roundingOrder_binaryOperator RoundDown exact_result allegedly_rounded_result
                else if 0 == exact_result && 0 == allegedly_rounded_result
                  then True
                  else if 0 == allegedly_rounded_result
                    then True -- zero is allways rounded ok
                    else undefined -- error $ "roundingOrder_binaryOperator: unconsiderd case: " ++ show (exact_result, allegedly_rounded_result)

    -- exact_result `sorted` rounded_result
    -- -2.0 `sorted` -1.0
    -- 1.0 `sorted` 2.0

roundingOrder_UnaryOperator
  :: Round
  -> (Rational -> Rational -> Bool)
roundingOrder_UnaryOperator RoundDown = (<=)
roundingOrder_UnaryOperator RoundUp = (>=)
roundingOrder_UnaryOperator Truncate = \allegedly_rounded_result exact_result
  -> if allegedly_rounded_result < 0 && exact_result < 0
        then (roundingOrder_UnaryOperator RoundUp) allegedly_rounded_result exact_result
        else if 0 < allegedly_rounded_result && 0 < exact_result
                then roundingOrder_UnaryOperator RoundDown allegedly_rounded_result exact_result
                else if 0 == allegedly_rounded_result && 0 == exact_result
                  then True
                  else if 0 == allegedly_rounded_result
                    then True -- zero is allways rounded ok
                    else undefined -- error $ "roundingOrder_binaryOperator: unconsiderd case: " ++ show (exact_result, allegedly_rounded_result)

chooseNudging
  :: IEEE_RoundingFuckUp_Correctable a
  => Round
  -> a
  -> a
chooseNudging RoundDown = nudgeDown
chooseNudging RoundUp = nudgeUp
chooseNudging Truncate = nudgeToZero

chooseTheTheOtherNudging
  :: IEEE_RoundingFuckUp_Correctable a
  => Round
  -> a
  -> a
chooseTheTheOtherNudging RoundDown = nudgeUp
chooseTheTheOtherNudging RoundUp = nudgeDown
chooseTheTheOtherNudging Truncate = nudgeAwayFromZero




------------




binary_operator_rounded
  :: Round
  -> (forall a. (Num a, Fractional a) => a -> a -> a)
  -> ((IEEE_RoundingFuckUp_Correctable b) => b -> b -> b)
binary_operator_rounded
  upOrDown
  operator
  left
  right
  = id
  $ result
  where
    sorted
      = liftComparator upOrDown
      $ roundingOrder_binaryOperator upOrDown

    nudge = chooseNudging upOrDown

    the_other_nudge = chooseTheTheOtherNudging upOrDown

    badArguments = undefined --error $ show ("case not supported", left, right)
    left_rational = getExisiting badArguments $ exactToRational left
    right_rational = getExisiting badArguments $ exactToRational right

    exact_result :: Rational
    exact_result = left_rational `operator` right_rational

    somehow_rounded_result = left `operator` right

    somehow_rounded_result_as_rational
      = exactToRational
      $ somehow_rounded_result

    was_already_rounded_correctly
      = (Exisiting exact_result) `sorted` somehow_rounded_result_as_rational

    correctly_rounded_result
      = if not (isNaN somehow_rounded_result || 0 == right)
          then if was_already_rounded_correctly
            then somehow_rounded_result
            else nudge somehow_rounded_result
          else somehow_rounded_result


    note
      = "new nudging did not work: "
      ++ show
      ( upOrDown
      , left
      , right
      )

    check
      = is_special_case
      ||
      (True
        && (Exisiting exact_result)
            `sorted`
              (exactToRational correctly_rounded_result)
        && (exactToRational $ the_other_nudge correctly_rounded_result)
            `strict_sorted`
              (Exisiting exact_result)
      )

    strict_sorted x y = (x /= y) && (x `sorted` y)


    is_special_case = False
      || isNaN left
      || isNaN right
      || isNegativeZero left
      || isNegativeZero right
      || isInfinite left
      || isInfinite right

    result
      = if not (isNaN left || isNaN right)
          then normal_case
          else somehow_rounded_result

    normal_case
      = if not(isInfinite left || isInfinite right)
          then propernumbercase
          else somehow_rounded_result

    propernumbercase = assertNote note check correctly_rounded_result






------------




squareRoot
  :: (Floating a, IEEE_RoundingFuckUp_Correctable a)
  => Round
  -> a
  -> a
squareRoot
  upOrDown
  preImage
  = result
  where
    sorted
      = liftComparator upOrDown
      $ roundingOrder_UnaryOperator upOrDown
      -- TODO refactor into one function

    nudge = chooseNudging upOrDown

    the_other_nudge = chooseTheTheOtherNudging upOrDown

    result = normal_case

    somehowRoundedResult = sqrt preImage

    preImage_Rational = exactToRational preImage

    somehowRoundedResult_preImage :: RationalWithSpecials
    somehowRoundedResult_preImage
      = id
      $ liftUnary (\x -> x*x)
      $ exactToRational
      $ somehowRoundedResult

    -- this works because squareroot is monotonic
    wasRoundedCorrectly
      = somehowRoundedResult_preImage
      `sorted` preImage_Rational

    correctly_rounded_result
      = if (not $ isNaN somehowRoundedResult) || (isNegativeZero preImage)
          then
            if wasRoundedCorrectly
              then somehowRoundedResult
              else nudge somehowRoundedResult
          else
            somehowRoundedResult

    normal_case = assertNote note check correctly_rounded_result

    note = "something went wrong"
    check
      = True
      && we_rounded_correctly
      && we_rounded_tightly

    we_rounded_correctly = ((liftUnary (\x->x*x) $ exactToRational correctly_rounded_result) `sorted` (exactToRational preImage))
    we_rounded_tightly = (exactToRational preImage) `strict_sorted` (liftUnary (\x->x*x) $ exactToRational (the_other_nudge correctly_rounded_result))
      where
        strict_sorted x y = (x /= y) && (x `sorted` y)





convert
  ::
    ( Show a
    , Real a
    , RationalWithSpecialsLike a
    , IEEE_RoundingFuckUp_Correctable b
    )
  => Round
  -> a
  -> b
convert
  upOrDown
  integral
  = id
  $ result
  where
    result = normal_case

    sorted
      = liftComparatorConvert upOrDown
      $ roundingOrder_UnaryOperator upOrDown

    nudge = chooseNudging upOrDown

    the_other_nudge = chooseTheTheOtherNudging upOrDown

    argument_Rational = exactToRational $ integral

    somehowRoundedResult = exactFromRational $ argument_Rational

    somehow_rounded_result_Rational
      = exactToRational somehowRoundedResult

    wasRoundedCorrectly
      = somehow_rounded_result_Rational
      `sorted` argument_Rational

    correctly_rounded_result
      = if wasRoundedCorrectly
          then somehowRoundedResult
          else nudge somehowRoundedResult

    normal_case = assertNote note check correctly_rounded_result

    note = "something went wrong"
    check
      = True
      && we_rounded_correctly
      && we_rounded_tightly

    we_rounded_correctly = (Exisiting $ toRational correctly_rounded_result) `sorted` (Exisiting $ toRational integral)
    we_rounded_tightly = (Exisiting $ toRational integral) `strict_sorted` (exactToRational $ the_other_nudge correctly_rounded_result)
      where
        strict_sorted = liftComparator upOrDown $ \x y -> (x /= y) && (Exisiting x `sorted` Exisiting y)

