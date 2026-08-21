{-# LANGUAGE MagicHash #-}

module GHC.Bits
  ( Bits (..),
    FiniteBits (..),
    bitDefault,
    testBitDefault,
    popCountDefault,
    toIntegralSized,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Integer
  ( Integer,
    compareInteger#,
    integerAnd,
    integerBit#,
    integerComplement,
    integerOr,
    integerPopCount#,
    integerShiftL#,
    integerShiftR#,
    integerTestBit#,
    integerXor,
  )
import GHC.Prim
  ( Int#,
    Word#,
    and#,
    clz#,
    ctz#,
    int2Word#,
    not#,
    or#,
    popCnt#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    word2Int#,
    xor#,
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Real (Integral (..), fromIntegral)
import Prelude
  ( Bool (..),
    Eq (..),
    Maybe (..),
    Num (..),
    not,
    (&&),
    (<),
    (>=),
    (||),
  )

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`

infixl 7 .&.

infixl 6 `xor`

infixl 5 .|.

class (Eq a) => Bits a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a

  shift :: a -> Int -> a
  shift = shiftDefault

  rotate :: a -> Int -> a
  rotate = rotateDefault

  zeroBits :: a
  zeroBits = zeroBitsDefault

  bit :: Int -> a

  setBit :: a -> Int -> a
  setBit = setBitDefault

  clearBit :: a -> Int -> a
  clearBit = clearBitDefault

  complementBit :: a -> Int -> a
  complementBit = complementBitDefault

  testBit :: a -> Int -> Bool

  bitSizeMaybe :: a -> Maybe Int

  bitSize :: a -> Int
  bitSize = bitSizeDefault

  isSigned :: a -> Bool

  shiftL :: a -> Int -> a
  shiftL = shiftLDefault

  unsafeShiftL :: a -> Int -> a
  unsafeShiftL = unsafeShiftLDefault

  shiftR :: a -> Int -> a
  shiftR = shiftRDefault

  unsafeShiftR :: a -> Int -> a
  unsafeShiftR = unsafeShiftRDefault

  rotateL :: a -> Int -> a
  rotateL = rotateLDefault

  rotateR :: a -> Int -> a
  rotateR = rotateRDefault

  popCount :: a -> Int

class (Bits a) => FiniteBits a where
  finiteBitSize :: a -> Int

  countLeadingZeros :: a -> Int
  countLeadingZeros = countLeadingZerosDefault

  countTrailingZeros :: a -> Int
  countTrailingZeros = countTrailingZerosDefault

shiftDefault :: (Bits a) => a -> Int -> a
shiftDefault value amount =
  case amount < 0 of
    True -> shiftR value (negate amount)
    False -> shiftL value amount

rotateDefault :: (Bits a) => a -> Int -> a
rotateDefault value amount =
  case amount < 0 of
    True -> rotateR value (negate amount)
    False -> rotateL value amount

zeroBitsDefault :: (Bits a) => a
zeroBitsDefault = clearBit (bit 0) 0

setBitDefault :: (Bits a) => a -> Int -> a
setBitDefault value index = value .|. bit index

clearBitDefault :: (Bits a) => a -> Int -> a
clearBitDefault value index = value .&. complement (bit index)

complementBitDefault :: (Bits a) => a -> Int -> a
complementBitDefault value index = xor value (bit index)

bitSizeDefault :: (Bits a) => a -> Int
bitSizeDefault value =
  case bitSizeMaybe value of
    Just size -> size
    Nothing -> undefinedBitSize value

shiftLDefault :: (Bits a) => a -> Int -> a
shiftLDefault = shift

unsafeShiftLDefault :: (Bits a) => a -> Int -> a
unsafeShiftLDefault = shiftL

shiftRDefault :: (Bits a) => a -> Int -> a
shiftRDefault value amount = shift value (negate amount)

unsafeShiftRDefault :: (Bits a) => a -> Int -> a
unsafeShiftRDefault = shiftR

rotateLDefault :: (Bits a) => a -> Int -> a
rotateLDefault = rotate

rotateRDefault :: (Bits a) => a -> Int -> a
rotateRDefault value amount = rotate value (negate amount)

countLeadingZerosDefault :: (FiniteBits a) => a -> Int
countLeadingZerosDefault value = leading (finiteBitSize value - 1)
  where
    leading index =
      case index < 0 of
        True -> finiteBitSize value
        False ->
          case testBit value index of
            True -> finiteBitSize value - 1 - index
            False -> leading (index - 1)

countTrailingZerosDefault :: (FiniteBits a) => a -> Int
countTrailingZerosDefault value = trailing 0
  where
    trailing index =
      case index >= finiteBitSize value of
        True -> finiteBitSize value
        False ->
          case testBit value index of
            True -> index
            False -> trailing (index + 1)

bitDefault :: (Bits a, Num a) => Int -> a
bitDefault = shiftL 1

testBitDefault :: (Bits a, Num a) => a -> Int -> Bool
testBitDefault value index =
  case popCount (value .&. bit index) of
    0 -> False
    _ -> True

popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault value =
  case bitSizeMaybe value of
    Just width -> countBits value 0 width 0
    Nothing -> popCount value

countBits :: (Bits a) => a -> Int -> Int -> Int -> Int
countBits value index width total =
  case index >= width of
    True -> total
    False ->
      case testBit value index of
        True -> countBits value (index + 1) width (total + 1)
        False -> countBits value (index + 1) width total

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized value =
  case fromIntegral value of
    converted ->
      case compareInteger# (toInteger value) (toInteger converted) of
        0# -> Just converted
        _ -> Nothing

undefinedBitSize :: a -> Int
undefinedBitSize = undefinedBitSize

invalidShift :: a -> Int -> a
invalidShift = invalidShift

instance Bits Bool where
  (.&.) = (&&)
  (.|.) = (||)
  xor = (/=)
  complement = not
  shift value (I# amount) =
    case amount of
      0# -> value
      _ -> False
  rotate value _ = value
  zeroBits = False
  bit (I# index) =
    case index of
      0# -> True
      _ -> False
  testBit value (I# index) =
    case index of
      0# -> value
      _ -> False
  bitSizeMaybe _ = Just 1
  bitSize _ = 1
  isSigned _ = False
  popCount False = 0
  popCount True = 1

instance FiniteBits Bool where
  finiteBitSize _ = 1
  countLeadingZeros False = 1
  countLeadingZeros True = 0
  countTrailingZeros False = 1
  countTrailingZeros True = 0

instance Bits Int where
  I# left .&. I# right = I# (word2Int# (and# (int2Word# left) (int2Word# right)))
  I# left .|. I# right = I# (word2Int# (or# (int2Word# left) (int2Word# right)))
  xor (I# left) (I# right) = I# (word2Int# (xor# (int2Word# left) (int2Word# right)))
  complement (I# value) = I# (word2Int# (not# (int2Word# value)))
  shift value (I# amount) =
    case (<#) amount 0# of
      1# -> shiftR value (I# ((-#) 0# amount))
      _ -> shiftL value (I# amount)
  shiftL (I# word) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I# word) (I# count)
      _ ->
        case (<#) count 64# of
          1# -> I# (word2Int# (uncheckedShiftL# (int2Word# word) count))
          _ -> I# 0#
  unsafeShiftL (I# word) (I# count) = I# (word2Int# (uncheckedShiftL# (int2Word# word) count))
  shiftR (I# word) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I# word) (I# count)
      _ ->
        case (<#) count 64# of
          1# -> I# (arithmeticShiftR# word count)
          _ ->
            case (<#) word 0# of
              1# -> I# ((-#) 0# 1#)
              _ -> I# 0#
  unsafeShiftR (I# word) (I# count) = I# (arithmeticShiftR# word count)
  rotate (I# word) (I# amount) = I# (word2Int# (rotateWord# (int2Word# word) (normalizeRotate# amount)))
  zeroBits = I# 0#
  bit (I# index) =
    case (<#) index 0# of
      1# -> I# 0#
      _ ->
        case (<#) index 64# of
          1# -> I# (word2Int# (uncheckedShiftL# (int2Word# 1#) index))
          _ -> I# 0#
  testBit (I# value) (I# index) =
    case (<#) index 0# of
      1# -> False
      _ ->
        case (<#) index 64# of
          1# -> wordIsNonzero# (and# (int2Word# value) (uncheckedShiftL# (int2Word# 1#) index))
          _ -> False
  bitSizeMaybe _ = Just (I# 64#)
  bitSize _ = I# 64#
  isSigned _ = True
  popCount (I# value) = I# (word2Int# (popCnt# (int2Word# value)))

instance FiniteBits Int where
  finiteBitSize _ = I# 64#
  countLeadingZeros (I# value) = I# (word2Int# (clz# (int2Word# value)))
  countTrailingZeros (I# value) = I# (word2Int# (ctz# (int2Word# value)))

instance Bits Integer where
  (.&.) = integerAnd
  (.|.) = integerOr
  xor = integerXor
  complement = integerComplement
  shift value (I# amount) =
    case (<#) amount 0# of
      1# -> integerShiftR# value ((-#) 0# amount)
      _ -> integerShiftL# value amount
  shiftL value (I# count) =
    case (<#) count 0# of
      1# -> invalidShift value (I# count)
      _ -> integerShiftL# value count
  unsafeShiftL value (I# count) = integerShiftL# value count
  shiftR value (I# count) =
    case (<#) count 0# of
      1# -> invalidShift value (I# count)
      _ -> integerShiftR# value count
  unsafeShiftR value (I# count) = integerShiftR# value count
  rotate = shift
  zeroBits = 0
  bit (I# index) = integerBit# index
  testBit value (I# index) =
    case integerTestBit# value index of
      0# -> False
      _ -> True
  bitSizeMaybe _ = Nothing
  bitSize = undefinedBitSize
  isSigned _ = True
  popCount value = I# (integerPopCount# value)

arithmeticShiftR# :: Int# -> Int# -> Int#
arithmeticShiftR# value amount =
  case amount of
    0# -> value
    _ ->
      case uncheckedShiftRL# (int2Word# value) amount of
        shifted ->
          case (<#) value 0# of
            1# -> word2Int# (or# shifted (uncheckedShiftL# (not# (int2Word# 0#)) ((-#) 64# amount)))
            _ -> word2Int# shifted

normalizeRotate# :: Int# -> Int#
normalizeRotate# amount = word2Int# (and# (int2Word# amount) (int2Word# 63#))

rotateWord# :: Word# -> Int# -> Word#
rotateWord# value amount =
  case amount of
    0# -> value
    _ -> or# (uncheckedShiftL# value amount) (uncheckedShiftRL# value ((-#) 64# amount))

wordIsNonzero# :: Word# -> Bool
wordIsNonzero# value =
  case word2Int# value of
    0# -> False
    _ -> True
