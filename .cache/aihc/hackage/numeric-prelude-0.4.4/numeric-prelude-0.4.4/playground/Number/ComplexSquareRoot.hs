module Number.ComplexSquareRoot where

import qualified Algebra.RealField as RealField
import qualified Algebra.RealRing as RealRing
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Number.Complex as Complex

import Test.QuickCheck (Arbitrary, arbitrary, )

import Control.Monad (liftM2, )

import qualified NumericPrelude.Numeric as NP
import NumericPrelude.Numeric hiding (recip, )
import NumericPrelude.Base
import Prelude ()


{- $setup
>>> import qualified Number.ComplexSquareRoot as SR
>>> import qualified Number.Complex as Complex
>>> import qualified Algebra.Laws as Laws
>>> import Test.QuickCheck ((==>))
>>> import NumericPrelude.Numeric
>>> import NumericPrelude.Base
>>> import Prelude ()
>>>
>>> sr :: SR.T Rational -> SR.T Rational
>>> sr = id
-}

{- |
Represent the square root of a complex number
without actually having to compute a square root.
If the Bool is False,
then the square root is represented with positive real part
or zero real part and positive imaginary part.
If the Bool is True the square root is negated.

prop> Laws.identity SR.mul SR.one . sr
prop> Laws.commutative SR.mul . sr
prop> Laws.associative SR.mul . sr
prop> Laws.homomorphism SR.fromNumber (\x y -> x * (y :: Complex.T Rational)) SR.mul
prop> Laws.rightIdentity SR.div SR.one . sr
prop> \x -> not (isZero x) ==> SR.recip (SR.recip x) == sr x
prop> \x -> not (isZero x) ==> Laws.inverse SR.mul SR.recip SR.one (sr x)
-}
data T a = Cons Bool (Complex.T a)
   deriving (Show)

{- |
You must use @fmap@ only for number type conversion.
-}
instance Functor T where
   fmap f (Cons n x) = Cons n (fmap f x)

instance (ZeroTestable.C a) => ZeroTestable.C (T a) where
   isZero (Cons _b s) = isZero s

instance (ZeroTestable.C a, Eq a) => Eq (T a) where
   (Cons xb xs) == (Cons yb ys) =
      isZero xs && isZero ys  ||
      xb==yb && xs==ys

instance (Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM2 Cons arbitrary arbitrary


fromNumber :: (RealRing.C a) => Complex.T a -> T a
fromNumber x =
   Cons
      (case compare zero (Complex.real x) of
         LT -> False
         GT -> True
         EQ -> Complex.imag x < zero)
      (x^2)

-- htam:Wavelet.DyadicResultant.parityFlip
toNumber :: (RealRing.C a, Complex.Power a) => T a -> Complex.T a
toNumber (Cons n x) =
   case sqrt x of y -> if n then NP.negate y else y


one :: (Ring.C a) => T a
one = Cons False NP.one

inUpperHalfplane :: (Additive.C a, Ord a) => Complex.T a -> Bool
inUpperHalfplane x =
   case compare (Complex.imag x) zero of
      GT -> True
      LT -> False
      EQ -> Complex.real x < zero

mul, mulAlt, mulAlt2 :: (RealRing.C a) => T a -> T a -> T a
mul (Cons xb xs) (Cons yb ys) =
   let zs = xs*ys
   in  Cons
          ((xb /= yb) /=
             case (inUpperHalfplane xs,
                   inUpperHalfplane ys,
                   inUpperHalfplane zs) of
                (True,True,False) -> True
                (False,False,True) -> True
                _ -> False)
          zs

mulAlt (Cons xb xs) (Cons yb ys) =
   let zs = xs*ys
   in  Cons
          ((xb /= yb) /=
             let xi = Complex.imag xs
                 yi = Complex.imag ys
                 zi = Complex.imag zs
             in  (xi>=zero) /= (yi>=zero) &&
                 (xi>=zero) /= (zi>=zero))
          zs

mulAlt2 (Cons xb xs) (Cons yb ys) =
   let zs = xs*ys
   in  Cons
          ((xb /= yb) /=
             let xi = Complex.imag xs
                 yi = Complex.imag ys
                 zi = Complex.imag zs
             in  xi*yi<zero && xi*zi<zero)
          zs

div :: (RealField.C a) => T a -> T a -> T a
div x y = mul x (recip y)

recip :: (RealField.C a) => T a -> T a
recip (Cons b s) =
   Cons
      (b /= (Complex.imag s == zero && Complex.real s < zero))
      (NP.recip s)
