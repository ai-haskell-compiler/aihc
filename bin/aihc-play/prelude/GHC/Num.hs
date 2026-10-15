{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Numbers for the small Prelude of The Lazy Reader.
--
-- The 'Integer' of the guide is one machine word. A number literal in a
-- module of the reader goes through 'fromInteger', as it does in GHC.
module GHC.Num
  ( Integer (..),
    Num (..),
    subtract,
  )
where

import GHC.Classes (Eq (..), Ord (..))
import GHC.Prim (Int#, negateInt#, (*#), (+#), (-#))
import GHC.Types (Int (..))

data Integer = IS Int#

infixl 6 +, -

infixl 7 *

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Integer -> a

instance Num Int where
  I# x + I# y = I# (x +# y)
  I# x - I# y = I# (x -# y)
  I# x * I# y = I# (x *# y)
  negate (I# x) = I# (negateInt# x)
  fromInteger (IS x) = I# x

instance Num Integer where
  IS x + IS y = IS (x +# y)
  IS x - IS y = IS (x -# y)
  IS x * IS y = IS (x *# y)
  negate (IS x) = IS (negateInt# x)
  fromInteger x = x

instance Eq Integer where
  IS x == IS y = I# x == I# y

instance Ord Integer where
  compare (IS x) (IS y) = compare (I# x) (I# y)

subtract :: (Num a) => a -> a -> a
subtract x y = y - x
