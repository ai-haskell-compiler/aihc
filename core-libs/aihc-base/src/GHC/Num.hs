{-# LANGUAGE MagicHash #-}

module GHC.Num
  ( Num (..),
  )
where

import GHC.Internal.Integer (Integer (..))

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

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
  IS x + IS y = IS ((+#) x y)
  IS x - IS y = IS ((-#) x y)
  IS x * IS y = IS ((*#) x y)
  negate (IS x) = IS ((-#) 0# x)
  abs x = x
  signum _ = IS 1#
  fromInteger x = x
