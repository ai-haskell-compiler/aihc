{-# LANGUAGE RebindableSyntax #-}
module Algebra.FloatingPoint where

import qualified Algebra.RealRing as RealRing
import NumericPrelude.Base

import qualified Prelude as P
import Prelude (Int, Integer, Float, Double, )


{- |
Counterpart of 'Prelude.RealFloat' but with NumericPrelude superclass.
-}
class RealRing.C a => C a where
   radix :: a -> Integer
   digits :: a -> Int
   range :: a -> (Int, Int)
   decode :: a -> (Integer, Int)
   encode :: Integer -> Int -> a
   exponent :: a -> Int
   significand :: a -> a
   scale :: Int -> a -> a
   isNaN :: a -> Bool
   isInfinite :: a -> Bool
   isDenormalized :: a -> Bool
   isNegativeZero :: a -> Bool
   isIEEE :: a -> Bool

instance C Float where
   radix = P.floatRadix
   digits = P.floatDigits
   range = P.floatRange
   decode = P.decodeFloat
   encode = P.encodeFloat
   exponent = P.exponent
   significand = P.significand
   scale = P.scaleFloat
   isNaN = P.isNaN
   isInfinite = P.isInfinite
   isDenormalized = P.isDenormalized
   isNegativeZero = P.isNegativeZero
   isIEEE = P.isIEEE

instance C Double where
   radix = P.floatRadix
   digits = P.floatDigits
   range = P.floatRange
   decode = P.decodeFloat
   encode = P.encodeFloat
   exponent = P.exponent
   significand = P.significand
   scale = P.scaleFloat
   isNaN = P.isNaN
   isInfinite = P.isInfinite
   isDenormalized = P.isDenormalized
   isNegativeZero = P.isNegativeZero
   isIEEE = P.isIEEE
