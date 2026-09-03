{-# LANGUAGE MagicHash #-}

module GHC.Int
  ( Int (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
  )
where

import GHC.Internal.Integer (integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim
  ( Int#,
    Int16#,
    Int32#,
    Int64#,
    Int8#,
    int16ToInt#,
    int32ToInt#,
    int64ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt64#,
    intToInt8#,
    (*#),
    (+#),
    (-#),
    (<#),
  )
import GHC.Types (Int (..))

data Int8 = I8# Int8#

data Int16 = I16# Int16#

data Int32 = I32# Int32#

data Int64 = I64# Int64#

intAbs :: Int# -> Int#
intAbs value =
  case (<#) value 0# of
    0# -> value
    _ -> (-#) 0# value

intSignum :: Int# -> Int#
intSignum value =
  case value of
    0# -> 0#
    _ ->
      case (<#) value 0# of
        0# -> 1#
        _ -> (-#) 0# 1#

instance Num Int8 where
  I8# left + I8# right = I8# (intToInt8# ((+#) (int8ToInt# left) (int8ToInt# right)))
  I8# left - I8# right = I8# (intToInt8# ((-#) (int8ToInt# left) (int8ToInt# right)))
  I8# left * I8# right = I8# (intToInt8# ((*#) (int8ToInt# left) (int8ToInt# right)))
  negate (I8# value) = I8# (intToInt8# ((-#) 0# (int8ToInt# value)))
  abs (I8# value) = I8# (intToInt8# (intAbs (int8ToInt# value)))
  signum (I8# value) = I8# (intToInt8# (intSignum (int8ToInt# value)))
  fromInteger value = I8# (intToInt8# (integerToInt# value))

instance Num Int16 where
  I16# left + I16# right = I16# (intToInt16# ((+#) (int16ToInt# left) (int16ToInt# right)))
  I16# left - I16# right = I16# (intToInt16# ((-#) (int16ToInt# left) (int16ToInt# right)))
  I16# left * I16# right = I16# (intToInt16# ((*#) (int16ToInt# left) (int16ToInt# right)))
  negate (I16# value) = I16# (intToInt16# ((-#) 0# (int16ToInt# value)))
  abs (I16# value) = I16# (intToInt16# (intAbs (int16ToInt# value)))
  signum (I16# value) = I16# (intToInt16# (intSignum (int16ToInt# value)))
  fromInteger value = I16# (intToInt16# (integerToInt# value))

instance Num Int32 where
  I32# left + I32# right = I32# (intToInt32# ((+#) (int32ToInt# left) (int32ToInt# right)))
  I32# left - I32# right = I32# (intToInt32# ((-#) (int32ToInt# left) (int32ToInt# right)))
  I32# left * I32# right = I32# (intToInt32# ((*#) (int32ToInt# left) (int32ToInt# right)))
  negate (I32# value) = I32# (intToInt32# ((-#) 0# (int32ToInt# value)))
  abs (I32# value) = I32# (intToInt32# (intAbs (int32ToInt# value)))
  signum (I32# value) = I32# (intToInt32# (intSignum (int32ToInt# value)))
  fromInteger value = I32# (intToInt32# (integerToInt# value))

instance Num Int64 where
  I64# left + I64# right = I64# (intToInt64# ((+#) (int64ToInt# left) (int64ToInt# right)))
  I64# left - I64# right = I64# (intToInt64# ((-#) (int64ToInt# left) (int64ToInt# right)))
  I64# left * I64# right = I64# (intToInt64# ((*#) (int64ToInt# left) (int64ToInt# right)))
  negate (I64# value) = I64# (intToInt64# ((-#) 0# (int64ToInt# value)))
  abs (I64# value) = I64# (intToInt64# (intAbs (int64ToInt# value)))
  signum (I64# value) = I64# (intToInt64# (intSignum (int64ToInt# value)))
  fromInteger value = I64# (intToInt64# (integerToInt# value))
