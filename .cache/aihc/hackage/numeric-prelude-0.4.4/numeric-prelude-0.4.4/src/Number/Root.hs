{-
ToDo:
having the root exponent as type-level number would be nice
there is a package for basic type-level number support
-}
module Number.Root where

import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring

import qualified MathObj.RootSet as RootSet
import qualified Number.Ratio as Ratio

import Algebra.IntegralDomain (divChecked, )

import qualified NumericPrelude.Numeric as NP
import NumericPrelude.Numeric hiding (recip, )
import NumericPrelude.Base
import Prelude ()

{- |
The root degree must be positive.
This way we can implement multiplication
using only multiplication from type @a@.
-}
data T a = Cons Integer a
   deriving (Show)

{- |
When you use @fmap@ you must assert that
@forall n. fmap f (Cons d x) == fmap f (Cons (n*d) (x^n))@
-}
instance Functor T where
   fmap f (Cons d x) = Cons d (f x)

fromNumber :: a -> T a
fromNumber = Cons 1

toNumber :: Algebraic.C a => T a -> a
toNumber (Cons n x) = Algebraic.root n x

toRootSet :: Ring.C a => T a -> RootSet.T a
toRootSet (Cons d x) =
   RootSet.lift0 ([negate x] ++ replicate (pred (fromInteger d)) zero ++ [one])


commonDegree :: Ring.C a => T a -> T a -> T (a,a)
commonDegree (Cons xd x) (Cons yd y) =
   let zd = lcm xd yd
   in  Cons zd (x ^ divChecked zd xd, y ^ divChecked zd yd)

instance (Eq a, Ring.C a) => Eq (T a) where
   x == y  =
      case commonDegree x y of
         Cons _ (xn,yn) -> xn==yn

instance (Ord a, Ring.C a) => Ord (T a) where
   compare x y  =
      case commonDegree x y of
         Cons _ (xn,yn) -> compare xn yn


mul :: Ring.C a => T a -> T a -> T a
mul x y = fmap (uncurry (*)) $ commonDegree x y

div :: Field.C a => T a -> T a -> T a
div x y = fmap (uncurry (/)) $ commonDegree x y

recip :: Field.C a => T a -> T a
recip = fmap NP.recip

{- |
exponent must be non-negative
-}
cardinalPower :: Ring.C a => Integer -> T a -> T a
cardinalPower n (Cons d x) =
   let m = gcd n d
   in  Cons (divChecked d m) (x ^ divChecked n m)

{- |
exponent can be negative
-}
integerPower :: Field.C a => Integer -> T a -> T a
integerPower n =
   if n<0
     then cardinalPower (-n) . recip
     else cardinalPower n

rationalPower :: Field.C a => Rational -> T a -> T a
rationalPower n =
   integerPower (Ratio.numerator n) .
   root (Ratio.denominator n)

{- |
exponent must be positive
-}
root :: Ring.C a => Integer -> T a -> T a
root n (Cons d x) = Cons (d*n) x

sqrt :: Ring.C a => T a -> T a
sqrt = root 2
