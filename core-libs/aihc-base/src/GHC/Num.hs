{-# LANGUAGE MagicHash #-}

module GHC.Num
  ( Num (..),
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Integer
  ( Integer,
    integerAbs,
    integerAdd,
    integerMul,
    integerNegate,
    integerSignum,
    integerSub,
    integerToInt#,
  )

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

foreign import prim (<#) :: Int# -> Int# -> Int#

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
