{-# LANGUAGE RebindableSyntax #-}
module Number.ResidueClass where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.IntegralDomain as Integral

import NumericPrelude.Base
import NumericPrelude.Numeric hiding (recip)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)


add, sub :: (Integral.C a) => a -> a -> a -> a
add m x y = mod (x+y) m
sub m x y = mod (x-y) m

neg :: (Integral.C a) => a -> a -> a
neg m x = mod (-x) m

mul :: (Integral.C a) => a -> a -> a -> a
mul m x y = mod (x*y) m


{- |
The division may be ambiguous.
In this case an arbitrary quotient is returned.

@
0/:4 * 2/:4 == 0/:4
2/:4 * 2/:4 == 0/:4
@
-}
divideMaybe :: (PID.C a) => a -> a -> a -> Maybe a
divideMaybe m x y =
   let (d,(_,z)) = extendedGCD m y
       (q,r)     = divMod x d
   in  toMaybe (isZero r) (mod (q*z) m)

divide :: (PID.C a) => a -> a -> a -> a
divide m x y =
   fromMaybe (error "ResidueClass.divide: indivisible")
             (divideMaybe m x y)

recip :: (PID.C a) => a -> a -> a
recip m = divide m one
