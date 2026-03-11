module PowerSeries (
   T, fromScalar, one, add, sub, neg, scale, mul,
   derivativeCoefficients, differentiate,
   ) where

import Polynomial
   (fromScalar, add, sub, neg, scale, mul,
    differentiate, progression)


type T a = [a]

one :: Num a => T a
one = fromScalar 1


derivativeCoefficients :: Fractional a => T a
derivativeCoefficients =
   scanl (/) 1 progression
--   map recip (scanl (*) 1 progression)
