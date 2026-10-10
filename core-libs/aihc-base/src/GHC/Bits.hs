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

import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
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
    Word16#,
    Word32#,
    Word64#,
    Word8#,
    and#,
    and64#,
    andWord16#,
    andWord32#,
    andWord8#,
    clz#,
    clz16#,
    clz32#,
    clz64#,
    clz8#,
    ctz#,
    ctz16#,
    ctz32#,
    ctz64#,
    ctz8#,
    int16ToInt#,
    int2Word#,
    int32ToInt#,
    int64ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt64#,
    intToInt8#,
    minusWord#,
    not#,
    not64#,
    notWord16#,
    notWord32#,
    notWord8#,
    or#,
    or64#,
    orWord16#,
    orWord32#,
    orWord8#,
    popCnt#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedShiftL#,
    uncheckedShiftL64#,
    uncheckedShiftLWord16#,
    uncheckedShiftLWord32#,
    uncheckedShiftLWord8#,
    uncheckedShiftRL#,
    uncheckedShiftRL64#,
    uncheckedShiftRLWord16#,
    uncheckedShiftRLWord32#,
    uncheckedShiftRLWord8#,
    word16ToWord#,
    word2Int#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
    xor#,
    xor64#,
    xorWord16#,
    xorWord32#,
    xorWord8#,
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Real (Integral (..), fromIntegral)
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))
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
  unsafeShiftL (I# word) (I# count) = I# (uncheckedIShiftL# word count)
  shiftR (I# word) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I# word) (I# count)
      _ ->
        case (<#) count 64# of
          1# -> I# (uncheckedIShiftRA# word count)
          _ ->
            case (<#) word 0# of
              1# -> I# ((-#) 0# 1#)
              _ -> I# 0#
  unsafeShiftR (I# word) (I# count) = I# (uncheckedIShiftRA# word count)
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

instance Bits Word8 where
  W8# left .&. W8# right = W8# (andWord8# left right)
  W8# left .|. W8# right = W8# (orWord8# left right)
  xor (W8# left) (W8# right) = W8# (xorWord8# left right)
  complement (W8# value) = W8# (notWord8# value)
  shiftL (W8# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W8# value) (I# count)
      _ -> W8# (shiftLWord8# value count)
  unsafeShiftL (W8# value) (I# count) = W8# (uncheckedShiftLWord8# value count)
  shiftR (W8# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W8# value) (I# count)
      _ -> W8# (shiftRLWord8# value count)
  unsafeShiftR (W8# value) (I# count) = W8# (uncheckedShiftRLWord8# value count)
  rotate (W8# value) (I# amount) = W8# (rotateWord8# value amount)
  zeroBits = W8# (wordToWord8# (int2Word# 0#))
  bit (I# index) = W8# (wordToWord8# (sizedBit# 8# index))
  testBit (W8# value) (I# index) = sizedTestBit# 8# (word8ToWord# value) index
  bitSizeMaybe _ = Just (I# 8#)
  bitSize _ = I# 8#
  isSigned _ = False
  popCount (W8# value) = I# (word2Int# (popCnt# (word8ToWord# value)))

instance FiniteBits Word8 where
  finiteBitSize _ = I# 8#
  countLeadingZeros (W8# value) = I# (word2Int# (clz8# (word8ToWord# value)))
  countTrailingZeros (W8# value) = I# (word2Int# (ctz8# (word8ToWord# value)))

instance Bits Word16 where
  W16# left .&. W16# right = W16# (andWord16# left right)
  W16# left .|. W16# right = W16# (orWord16# left right)
  xor (W16# left) (W16# right) = W16# (xorWord16# left right)
  complement (W16# value) = W16# (notWord16# value)
  shiftL (W16# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W16# value) (I# count)
      _ -> W16# (shiftLWord16# value count)
  unsafeShiftL (W16# value) (I# count) = W16# (uncheckedShiftLWord16# value count)
  shiftR (W16# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W16# value) (I# count)
      _ -> W16# (shiftRLWord16# value count)
  unsafeShiftR (W16# value) (I# count) = W16# (uncheckedShiftRLWord16# value count)
  rotate (W16# value) (I# amount) = W16# (rotateWord16# value amount)
  zeroBits = W16# (wordToWord16# (int2Word# 0#))
  bit (I# index) = W16# (wordToWord16# (sizedBit# 16# index))
  testBit (W16# value) (I# index) = sizedTestBit# 16# (word16ToWord# value) index
  bitSizeMaybe _ = Just (I# 16#)
  bitSize _ = I# 16#
  isSigned _ = False
  popCount (W16# value) = I# (word2Int# (popCnt# (word16ToWord# value)))

instance FiniteBits Word16 where
  finiteBitSize _ = I# 16#
  countLeadingZeros (W16# value) = I# (word2Int# (clz16# (word16ToWord# value)))
  countTrailingZeros (W16# value) = I# (word2Int# (ctz16# (word16ToWord# value)))

-- The Word32 methods use the 32-bit primops. A method that widens to
-- Word# and narrows back costs a mask after each operation on a 64-bit
-- target.
instance Bits Word32 where
  W32# left .&. W32# right = W32# (andWord32# left right)
  W32# left .|. W32# right = W32# (orWord32# left right)
  xor (W32# left) (W32# right) = W32# (xorWord32# left right)
  complement (W32# value) = W32# (notWord32# value)
  shiftL (W32# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W32# value) (I# count)
      _ -> W32# (shiftLWord32# value count)
  unsafeShiftL (W32# value) (I# count) = W32# (uncheckedShiftLWord32# value count)
  shiftR (W32# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W32# value) (I# count)
      _ -> W32# (shiftRLWord32# value count)
  unsafeShiftR (W32# value) (I# count) = W32# (uncheckedShiftRLWord32# value count)
  rotate (W32# value) (I# amount) = W32# (rotateWord32# value amount)
  zeroBits = W32# (wordToWord32# (int2Word# 0#))
  bit (I# index) = W32# (wordToWord32# (sizedBit# 32# index))
  testBit (W32# value) (I# index) = sizedTestBit# 32# (word32ToWord# value) index
  bitSizeMaybe _ = Just (I# 32#)
  bitSize _ = I# 32#
  isSigned _ = False
  popCount (W32# value) = I# (word2Int# (popCnt# (word32ToWord# value)))

instance FiniteBits Word32 where
  finiteBitSize _ = I# 32#
  countLeadingZeros (W32# value) = I# (word2Int# (clz32# (word32ToWord# value)))
  countTrailingZeros (W32# value) = I# (word2Int# (ctz32# (word32ToWord# value)))

instance Bits Word64 where
  W64# left .&. W64# right = W64# (and64# left right)
  W64# left .|. W64# right = W64# (or64# left right)
  xor (W64# left) (W64# right) = W64# (xor64# left right)
  complement (W64# value) = W64# (not64# value)
  shiftL (W64# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W64# value) (I# count)
      _ -> W64# (shiftL64Word# value count)
  unsafeShiftL (W64# value) (I# count) = W64# (uncheckedShiftL64# value count)
  shiftR (W64# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W64# value) (I# count)
      _ -> W64# (shiftRL64Word# value count)
  unsafeShiftR (W64# value) (I# count) = W64# (uncheckedShiftRL64# value count)
  rotate (W64# value) (I# amount) = W64# (rotateWord64# value amount)
  zeroBits = W64# (wordToWord64# (int2Word# 0#))
  bit (I# index) = W64# (wordToWord64# (sizedBit# 64# index))
  testBit (W64# value) (I# index) = sizedTestBit# 64# (word64ToWord# value) index
  bitSizeMaybe _ = Just (I# 64#)
  bitSize _ = I# 64#
  isSigned _ = False
  popCount (W64# value) = I# (word2Int# (popCnt# (word64ToWord# value)))

instance FiniteBits Word64 where
  finiteBitSize _ = I# 64#
  countLeadingZeros (W64# value) = I# (word2Int# (clz64# value))
  countTrailingZeros (W64# value) = I# (word2Int# (ctz64# value))

instance Bits Word where
  W# left .&. W# right = W# (and# left right)
  W# left .|. W# right = W# (or# left right)
  xor (W# left) (W# right) = W# (xor# left right)
  complement (W# value) = W# (not# value)
  shiftL (W# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W# value) (I# count)
      _ -> W# (sizedShiftL# 64# value count)
  unsafeShiftL (W# value) (I# count) = W# (uncheckedShiftL# value count)
  shiftR (W# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (W# value) (I# count)
      _ -> W# (sizedShiftRL# 64# value count)
  unsafeShiftR (W# value) (I# count) = W# (uncheckedShiftRL# value count)
  rotate (W# value) (I# amount) = W# (sizedRotate# 64# value amount)
  zeroBits = W# (int2Word# 0#)
  bit (I# index) = W# (sizedBit# 64# index)
  testBit (W# value) (I# index) = sizedTestBit# 64# value index
  bitSizeMaybe _ = Just (I# 64#)
  bitSize _ = I# 64#
  isSigned _ = False
  popCount (W# value) = I# (word2Int# (popCnt# value))

instance FiniteBits Word where
  finiteBitSize _ = I# 64#
  countLeadingZeros (W# value) = I# (word2Int# (clz# value))
  countTrailingZeros (W# value) = I# (word2Int# (ctz# value))

instance Bits Int8 where
  I8# left .&. I8# right = I8# (intToInt8# (word2Int# (and# (int2Word# (int8ToInt# left)) (int2Word# (int8ToInt# right)))))
  I8# left .|. I8# right = I8# (intToInt8# (word2Int# (or# (int2Word# (int8ToInt# left)) (int2Word# (int8ToInt# right)))))
  xor (I8# left) (I8# right) = I8# (intToInt8# (word2Int# (xor# (int2Word# (int8ToInt# left)) (int2Word# (int8ToInt# right)))))
  complement (I8# value) = I8# (intToInt8# (word2Int# (not# (int2Word# (int8ToInt# value)))))
  shiftL (I8# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I8# value) (I# count)
      _ -> I8# (intToInt8# (word2Int# (sizedShiftL# 8# (int2Word# (int8ToInt# value)) count)))
  unsafeShiftL (I8# value) (I# count) = I8# (intToInt8# (uncheckedIShiftL# (int8ToInt# value) count))
  shiftR (I8# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I8# value) (I# count)
      _ -> I8# (intToInt8# (sizedShiftRA# 8# (int8ToInt# value) count))
  unsafeShiftR (I8# value) (I# count) = I8# (intToInt8# (uncheckedIShiftRA# (int8ToInt# value) count))
  rotate (I8# value) (I# amount) = I8# (intToInt8# (word2Int# (sizedRotate# 8# (narrowWord# 8# (int2Word# (int8ToInt# value))) amount)))
  zeroBits = I8# (intToInt8# 0#)
  bit (I# index) = I8# (intToInt8# (word2Int# (sizedBit# 8# index)))
  testBit (I8# value) (I# index) = sizedTestBit# 8# (narrowWord# 8# (int2Word# (int8ToInt# value))) index
  bitSizeMaybe _ = Just (I# 8#)
  bitSize _ = I# 8#
  isSigned _ = True
  popCount (I8# value) = I# (word2Int# (popCnt# (narrowWord# 8# (int2Word# (int8ToInt# value)))))

instance FiniteBits Int8 where
  finiteBitSize _ = I# 8#
  countLeadingZeros (I8# value) = I# (word2Int# (clz8# (int2Word# (int8ToInt# value))))
  countTrailingZeros (I8# value) = I# (word2Int# (ctz8# (int2Word# (int8ToInt# value))))

instance Bits Int16 where
  I16# left .&. I16# right = I16# (intToInt16# (word2Int# (and# (int2Word# (int16ToInt# left)) (int2Word# (int16ToInt# right)))))
  I16# left .|. I16# right = I16# (intToInt16# (word2Int# (or# (int2Word# (int16ToInt# left)) (int2Word# (int16ToInt# right)))))
  xor (I16# left) (I16# right) = I16# (intToInt16# (word2Int# (xor# (int2Word# (int16ToInt# left)) (int2Word# (int16ToInt# right)))))
  complement (I16# value) = I16# (intToInt16# (word2Int# (not# (int2Word# (int16ToInt# value)))))
  shiftL (I16# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I16# value) (I# count)
      _ -> I16# (intToInt16# (word2Int# (sizedShiftL# 16# (int2Word# (int16ToInt# value)) count)))
  unsafeShiftL (I16# value) (I# count) = I16# (intToInt16# (uncheckedIShiftL# (int16ToInt# value) count))
  shiftR (I16# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I16# value) (I# count)
      _ -> I16# (intToInt16# (sizedShiftRA# 16# (int16ToInt# value) count))
  unsafeShiftR (I16# value) (I# count) = I16# (intToInt16# (uncheckedIShiftRA# (int16ToInt# value) count))
  rotate (I16# value) (I# amount) = I16# (intToInt16# (word2Int# (sizedRotate# 16# (narrowWord# 16# (int2Word# (int16ToInt# value))) amount)))
  zeroBits = I16# (intToInt16# 0#)
  bit (I# index) = I16# (intToInt16# (word2Int# (sizedBit# 16# index)))
  testBit (I16# value) (I# index) = sizedTestBit# 16# (narrowWord# 16# (int2Word# (int16ToInt# value))) index
  bitSizeMaybe _ = Just (I# 16#)
  bitSize _ = I# 16#
  isSigned _ = True
  popCount (I16# value) = I# (word2Int# (popCnt# (narrowWord# 16# (int2Word# (int16ToInt# value)))))

instance FiniteBits Int16 where
  finiteBitSize _ = I# 16#
  countLeadingZeros (I16# value) = I# (word2Int# (clz16# (int2Word# (int16ToInt# value))))
  countTrailingZeros (I16# value) = I# (word2Int# (ctz16# (int2Word# (int16ToInt# value))))

instance Bits Int32 where
  I32# left .&. I32# right = I32# (intToInt32# (word2Int# (and# (int2Word# (int32ToInt# left)) (int2Word# (int32ToInt# right)))))
  I32# left .|. I32# right = I32# (intToInt32# (word2Int# (or# (int2Word# (int32ToInt# left)) (int2Word# (int32ToInt# right)))))
  xor (I32# left) (I32# right) = I32# (intToInt32# (word2Int# (xor# (int2Word# (int32ToInt# left)) (int2Word# (int32ToInt# right)))))
  complement (I32# value) = I32# (intToInt32# (word2Int# (not# (int2Word# (int32ToInt# value)))))
  shiftL (I32# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I32# value) (I# count)
      _ -> I32# (intToInt32# (word2Int# (sizedShiftL# 32# (int2Word# (int32ToInt# value)) count)))
  unsafeShiftL (I32# value) (I# count) = I32# (intToInt32# (uncheckedIShiftL# (int32ToInt# value) count))
  shiftR (I32# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I32# value) (I# count)
      _ -> I32# (intToInt32# (sizedShiftRA# 32# (int32ToInt# value) count))
  unsafeShiftR (I32# value) (I# count) = I32# (intToInt32# (uncheckedIShiftRA# (int32ToInt# value) count))
  rotate (I32# value) (I# amount) = I32# (intToInt32# (word2Int# (sizedRotate# 32# (narrowWord# 32# (int2Word# (int32ToInt# value))) amount)))
  zeroBits = I32# (intToInt32# 0#)
  bit (I# index) = I32# (intToInt32# (word2Int# (sizedBit# 32# index)))
  testBit (I32# value) (I# index) = sizedTestBit# 32# (narrowWord# 32# (int2Word# (int32ToInt# value))) index
  bitSizeMaybe _ = Just (I# 32#)
  bitSize _ = I# 32#
  isSigned _ = True
  popCount (I32# value) = I# (word2Int# (popCnt# (narrowWord# 32# (int2Word# (int32ToInt# value)))))

instance FiniteBits Int32 where
  finiteBitSize _ = I# 32#
  countLeadingZeros (I32# value) = I# (word2Int# (clz32# (int2Word# (int32ToInt# value))))
  countTrailingZeros (I32# value) = I# (word2Int# (ctz32# (int2Word# (int32ToInt# value))))

instance Bits Int64 where
  I64# left .&. I64# right = I64# (intToInt64# (word2Int# (and# (int2Word# (int64ToInt# left)) (int2Word# (int64ToInt# right)))))
  I64# left .|. I64# right = I64# (intToInt64# (word2Int# (or# (int2Word# (int64ToInt# left)) (int2Word# (int64ToInt# right)))))
  xor (I64# left) (I64# right) = I64# (intToInt64# (word2Int# (xor# (int2Word# (int64ToInt# left)) (int2Word# (int64ToInt# right)))))
  complement (I64# value) = I64# (intToInt64# (word2Int# (not# (int2Word# (int64ToInt# value)))))
  shiftL (I64# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I64# value) (I# count)
      _ -> I64# (intToInt64# (word2Int# (sizedShiftL# 64# (int2Word# (int64ToInt# value)) count)))
  unsafeShiftL (I64# value) (I# count) = I64# (intToInt64# (uncheckedIShiftL# (int64ToInt# value) count))
  shiftR (I64# value) (I# count) =
    case (<#) count 0# of
      1# -> invalidShift (I64# value) (I# count)
      _ -> I64# (intToInt64# (sizedShiftRA# 64# (int64ToInt# value) count))
  unsafeShiftR (I64# value) (I# count) = I64# (intToInt64# (uncheckedIShiftRA# (int64ToInt# value) count))
  rotate (I64# value) (I# amount) = I64# (intToInt64# (word2Int# (sizedRotate# 64# (narrowWord# 64# (int2Word# (int64ToInt# value))) amount)))
  zeroBits = I64# (intToInt64# 0#)
  bit (I# index) = I64# (intToInt64# (word2Int# (sizedBit# 64# index)))
  testBit (I64# value) (I# index) = sizedTestBit# 64# (narrowWord# 64# (int2Word# (int64ToInt# value))) index
  bitSizeMaybe _ = Just (I# 64#)
  bitSize _ = I# 64#
  isSigned _ = True
  popCount (I64# value) = I# (word2Int# (popCnt# (narrowWord# 64# (int2Word# (int64ToInt# value)))))

instance FiniteBits Int64 where
  finiteBitSize _ = I# 64#
  countLeadingZeros (I64# value) = I# (word2Int# (clz# (int2Word# (int64ToInt# value))))
  countTrailingZeros (I64# value) = I# (word2Int# (ctz# (int2Word# (int64ToInt# value))))

-- | Make a mask that has the low bits of a fixed width set.
widthMask# :: Int# -> Word#
widthMask# width =
  case (==#) width 64# of
    1# -> not# (int2Word# 0#)
    _ -> minusWord# (uncheckedShiftL# (int2Word# 1#) width) (int2Word# 1#)

-- | Keep only the low bits of a fixed width.
narrowWord# :: Int# -> Word# -> Word#
narrowWord# width value = and# value (widthMask# width)

-- | Shift a fixed-width word to the left and drop the bits above the width.
sizedShiftL# :: Int# -> Word# -> Int# -> Word#
sizedShiftL# width value count =
  case (<#) count width of
    1# -> narrowWord# width (uncheckedShiftL# value count)
    _ -> int2Word# 0#

-- | Shift a fixed-width word to the right and put zero bits in.
sizedShiftRL# :: Int# -> Word# -> Int# -> Word#
sizedShiftRL# width value count =
  case (<#) count width of
    1# -> uncheckedShiftRL# value count
    _ -> int2Word# 0#

-- | Shift a fixed-width signed value to the right and keep its sign.
sizedShiftRA# :: Int# -> Int# -> Int# -> Int#
sizedShiftRA# width value count =
  case (<#) count width of
    1# -> uncheckedIShiftRA# value count
    _ ->
      case (<#) value 0# of
        1# -> (-#) 0# 1#
        _ -> 0#

-- | Rotate a fixed-width word. The width must be a power of two.
sizedRotate# :: Int# -> Word# -> Int# -> Word#
sizedRotate# width value amount =
  case word2Int# (and# (int2Word# amount) (int2Word# ((-#) width 1#))) of
    0# -> value
    normalized -> narrowWord# width (or# (uncheckedShiftL# value normalized) (uncheckedShiftRL# value ((-#) width normalized)))

-- | Shift a 32-bit word to the left. A count of 32 or more gives zero.
shiftLWord32# :: Word32# -> Int# -> Word32#
shiftLWord32# value count =
  case (<#) count 32# of
    1# -> uncheckedShiftLWord32# value count
    _ -> wordToWord32# (int2Word# 0#)

-- | Shift a 32-bit word to the right and put zero bits in. A count of 32
-- or more gives zero.
shiftRLWord32# :: Word32# -> Int# -> Word32#
shiftRLWord32# value count =
  case (<#) count 32# of
    1# -> uncheckedShiftRLWord32# value count
    _ -> wordToWord32# (int2Word# 0#)

-- | Rotate a 32-bit word to the left. The amount wraps at 32, so a
-- negative amount rotates to the right.
rotateWord32# :: Word32# -> Int# -> Word32#
rotateWord32# value amount =
  case word2Int# (and# (int2Word# amount) (int2Word# 31#)) of
    0# -> value
    normalized -> orWord32# (uncheckedShiftLWord32# value normalized) (uncheckedShiftRLWord32# value ((-#) 32# normalized))

-- | Shift a 8-bit word to the left. A count of 8 or more gives zero.
shiftLWord8# :: Word8# -> Int# -> Word8#
shiftLWord8# value count =
  case (<#) count 8# of
    1# -> uncheckedShiftLWord8# value count
    _ -> wordToWord8# (int2Word# 0#)

-- | Shift a 8-bit word to the right and put zero bits in. A count of 8
-- or more gives zero.
shiftRLWord8# :: Word8# -> Int# -> Word8#
shiftRLWord8# value count =
  case (<#) count 8# of
    1# -> uncheckedShiftRLWord8# value count
    _ -> wordToWord8# (int2Word# 0#)

-- | Rotate a 8-bit word to the left. The amount wraps at 8, so a
-- negative amount rotates to the right.
rotateWord8# :: Word8# -> Int# -> Word8#
rotateWord8# value amount =
  case word2Int# (and# (int2Word# amount) (int2Word# 7#)) of
    0# -> value
    normalized -> orWord8# (uncheckedShiftLWord8# value normalized) (uncheckedShiftRLWord8# value ((-#) 8# normalized))

-- | Shift a 16-bit word to the left. A count of 16 or more gives zero.
shiftLWord16# :: Word16# -> Int# -> Word16#
shiftLWord16# value count =
  case (<#) count 16# of
    1# -> uncheckedShiftLWord16# value count
    _ -> wordToWord16# (int2Word# 0#)

-- | Shift a 16-bit word to the right and put zero bits in. A count of 16
-- or more gives zero.
shiftRLWord16# :: Word16# -> Int# -> Word16#
shiftRLWord16# value count =
  case (<#) count 16# of
    1# -> uncheckedShiftRLWord16# value count
    _ -> wordToWord16# (int2Word# 0#)

-- | Rotate a 16-bit word to the left. The amount wraps at 16, so a
-- negative amount rotates to the right.
rotateWord16# :: Word16# -> Int# -> Word16#
rotateWord16# value amount =
  case word2Int# (and# (int2Word# amount) (int2Word# 15#)) of
    0# -> value
    normalized -> orWord16# (uncheckedShiftLWord16# value normalized) (uncheckedShiftRLWord16# value ((-#) 16# normalized))

-- | Shift a 64-bit word to the left. A count of 64 or more gives zero.
shiftL64Word# :: Word64# -> Int# -> Word64#
shiftL64Word# value count =
  case (<#) count 64# of
    1# -> uncheckedShiftL64# value count
    _ -> wordToWord64# (int2Word# 0#)

-- | Shift a 64-bit word to the right and put zero bits in. A count of 64
-- or more gives zero.
shiftRL64Word# :: Word64# -> Int# -> Word64#
shiftRL64Word# value count =
  case (<#) count 64# of
    1# -> uncheckedShiftRL64# value count
    _ -> wordToWord64# (int2Word# 0#)

-- | Rotate a 64-bit word to the left. The amount wraps at 64, so a
-- negative amount rotates to the right.
rotateWord64# :: Word64# -> Int# -> Word64#
rotateWord64# value amount =
  case word2Int# (and# (int2Word# amount) (int2Word# 63#)) of
    0# -> value
    normalized -> or64# (uncheckedShiftL64# value normalized) (uncheckedShiftRL64# value ((-#) 64# normalized))

-- | Make a fixed-width word that has one bit set.
sizedBit# :: Int# -> Int# -> Word#
sizedBit# width index =
  case (<#) index 0# of
    1# -> int2Word# 0#
    _ ->
      case (<#) index width of
        1# -> uncheckedShiftL# (int2Word# 1#) index
        _ -> int2Word# 0#

-- | Test one bit of a fixed-width word.
sizedTestBit# :: Int# -> Word# -> Int# -> Bool
sizedTestBit# width value index =
  case (<#) index 0# of
    1# -> False
    _ ->
      case (<#) index width of
        1# -> wordIsNonzero# (and# value (uncheckedShiftL# (int2Word# 1#) index))
        _ -> False

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
