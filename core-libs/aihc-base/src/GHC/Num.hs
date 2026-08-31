{-# LANGUAGE MagicHash #-}

module GHC.Num
  ( Num (..),
    integerAdd,
    integerFromTwoWords#,
    integerNegate,
    integerShiftL#,
  )
where

import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Integer
  ( Integer,
    integerAbs,
    integerAdd,
    integerFromTwoWords#,
    integerMul,
    integerNegate,
    integerShiftL#,
    integerSignum,
    integerSub,
    integerToInt#,
  )
import GHC.Prim
  ( castWord32ToFloat#,
    castWord64ToDouble#,
    eqWord#,
    int2Word#,
    intToInt16#,
    intToInt32#,
    intToInt64#,
    intToInt8#,
    minusWord#,
    plusWord#,
    timesWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
    (*#),
    (+#),
    (-#),
    (<#),
  )
import GHC.Types (Double (..), Float (..))
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

infixl 6 +, -

infixl 7 *

instance Num Integer where
  (+) = integerAdd
  (-) = integerSub
  (*) = integerMul
  negate = integerNegate
  abs = integerAbs
  signum = integerSignum
  fromInteger x = x

instance Num Int where
  I# x + I# y = I# ((+#) x y)
  I# x - I# y = I# ((-#) x y)
  I# x * I# y = I# ((*#) x y)
  negate (I# x) = I# ((-#) 0# x)
  abs (I# x) =
    case (<#) x 0# of
      0# -> I# x
      _ -> I# ((-#) 0# x)
  signum (I# x) =
    case x of
      0# -> I# 0#
      _ ->
        case (<#) x 0# of
          0# -> I# 1#
          _ -> I# ((-#) 0# 1#)
  fromInteger x = I# (integerToInt# x)

instance Num Word where
  W# x + W# y = W# (plusWord# x y)
  W# x - W# y = W# (minusWord# x y)
  W# x * W# y = W# (timesWord# x y)
  negate (W# x) = W# (minusWord# (int2Word# 0#) x)
  abs x = x
  signum (W# x) =
    case eqWord# x (int2Word# 0#) of
      0# -> W# (int2Word# 1#)
      _ -> W# (int2Word# 0#)
  fromInteger x = W# (int2Word# (integerToInt# x))

instance Num Float where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = F# (castWord32ToFloat# (wordToWord32# (int2Word# (integerToInt# x))))

instance Num Double where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = D# (castWord64ToDouble# (wordToWord64# (int2Word# (integerToInt# x))))

instance Num Int8 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = I8# (intToInt8# (integerToInt# x))

instance Num Int16 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = I16# (intToInt16# (integerToInt# x))

instance Num Int32 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = I32# (intToInt32# (integerToInt# x))

instance Num Int64 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs = unsupportedNumUnary
  signum = unsupportedNumUnary
  fromInteger x = I64# (intToInt64# (integerToInt# x))

instance Num Word8 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs x = x
  signum = unsupportedNumUnary
  fromInteger x = W8# (wordToWord8# (int2Word# (integerToInt# x)))

instance Num Word16 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs x = x
  signum = unsupportedNumUnary
  fromInteger x = W16# (wordToWord16# (int2Word# (integerToInt# x)))

instance Num Word32 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs x = x
  signum = unsupportedNumUnary
  fromInteger x = W32# (wordToWord32# (int2Word# (integerToInt# x)))

instance Num Word64 where
  (+) = unsupportedNumBinary
  (-) = unsupportedNumBinary
  (*) = unsupportedNumBinary
  negate = unsupportedNumUnary
  abs x = x
  signum = unsupportedNumUnary
  fromInteger x = W64# (wordToWord64# (int2Word# (integerToInt# x)))

unsupportedNumUnary :: a -> a
unsupportedNumUnary _ = unsupportedNumUnary unsupportedNumValue

unsupportedNumBinary :: a -> a -> a
unsupportedNumBinary _ _ = unsupportedNumValue

unsupportedNumValue :: a
unsupportedNumValue = unsupportedNumValue
