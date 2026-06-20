module GHC.Num
  ( Num (..),
  )
where

import GHC.Integer (Integer)

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
