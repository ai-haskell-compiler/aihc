{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
Rationale for -fno-warn-orphans:
 * The orphan instances can't be put into Numeric.NonNegative.Wrapper
   since that's in another package.
 * We had to spread the instance declarations
   over the modules defining the typeclasses instantiated.
   Do we want that?
-}

{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

A type for non-negative numbers.
It performs a run-time check at construction time (i.e. at run-time)
and is a member of the non-negative number type class
'Numeric.NonNegative.Class.C'.
-}
module Number.NonNegative
   (T, fromNumber, fromNumberMsg, fromNumberClip, fromNumberUnsafe, toNumber,
    NonNegW.Int, NonNegW.Integer, NonNegW.Float, NonNegW.Double,
    Ratio, Rational) where

import Numeric.NonNegative.Wrapper
   (T, fromNumberUnsafe, toNumber, )
import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Algebra.NonNegative        as NonNeg
import qualified Algebra.Transcendental     as Trans
import qualified Algebra.Algebraic          as Algebraic
import qualified Algebra.RealRing          as RealRing
import qualified Algebra.Field              as Field
import qualified Algebra.RealIntegral       as RealIntegral
import qualified Algebra.IntegralDomain     as Integral
import qualified Algebra.Absolute               as Absolute
import qualified Algebra.Ring               as Ring
import qualified Algebra.Additive           as Additive
import qualified Algebra.Monoid             as Monoid
import qualified Algebra.ZeroTestable       as ZeroTestable

import qualified Algebra.ToInteger          as ToInteger
import qualified Algebra.ToRational         as ToRational

import qualified Number.Ratio as R

import NumericPrelude.Base
import Data.Tuple.HT (mapSnd, mapPair, )
import NumericPrelude.Numeric hiding (Int, Integer, Float, Double, Rational, )


{- |
Convert a number to a non-negative number.
If a negative number is given, an error is raised.
-}
fromNumber :: (Ord a, Additive.C a) =>
      a
   -> T a
fromNumber = fromNumberMsg "fromNumber"

fromNumberMsg :: (Ord a, Additive.C a) =>
      String  {- ^ name of the calling function to be used in the error message -}
   -> a
   -> T a
fromNumberMsg funcName x =
   if x>=zero
     then fromNumberUnsafe x
     else error (funcName++": negative number")

fromNumberWrap :: (Ord a, Additive.C a) =>
      String
   -> a
   -> T a
fromNumberWrap funcName =
   fromNumberMsg ("Number.NonNegative."++funcName)

{- |
Convert a number to a non-negative number.
A negative number will be replaced by zero.
Use this function with care since it may hide bugs.
-}
fromNumberClip :: (Ord a, Additive.C a) =>
      a
   -> T a
fromNumberClip = fromNumberUnsafe . max zero



{- |
Results are not checked for positivity.
-}
lift :: (a -> a) -> (T a -> T a)
lift f = fromNumberUnsafe . f . toNumber

liftWrap :: (Ord a, Additive.C a) => String -> (a -> a) -> (T a -> T a)
liftWrap msg f = fromNumberWrap msg . f . toNumber


{- |
Results are not checked for positivity.
-}
lift2 :: (a -> a -> a) -> (T a -> T a -> T a)
lift2 f x y =
   fromNumberUnsafe $ f (toNumber x) (toNumber y)



instance ZeroTestable.C a => ZeroTestable.C (T a) where
   isZero = isZero . toNumber

instance (Additive.C a) => Monoid.C (T a) where
   idt = fromNumberUnsafe Additive.zero
   x <*> y = fromNumberUnsafe (toNumber x + toNumber y)
--   mconcat = fromNumberUnsafe . sum . map toNumber

instance (Ord a, Additive.C a) => NonNeg.C (T a) where
   split = NonNeg.splitDefault toNumber fromNumberUnsafe

instance (Ord a, Additive.C a) => Additive.C (T a) where
   zero   = fromNumberUnsafe zero
   (+)    = lift2 (+)
   (-)    = liftWrap "-" . (-) . toNumber
   negate = liftWrap "negate" negate

instance (Ord a, Ring.C a) => Ring.C (T a) where
   (*)    = lift2 (*)
   fromInteger = fromNumberWrap "fromInteger" . fromInteger

instance (Ord a, ToRational.C a) => ToRational.C (T a) where
   toRational = ToRational.toRational . toNumber

instance ToInteger.C a => ToInteger.C (T a) where
   toInteger = toInteger . toNumber

{- already defined in the imported module
instance (Ord a, Additive.C a, Enum a) => Enum (T a) where
   toEnum   = fromNumberWrap "toEnum" . toEnum
   fromEnum = fromEnum . toNumber

instance (Ord a, Additive.C a, Bounded a) => Bounded (T a) where
   minBound = fromNumberClip minBound
   maxBound = fromNumberWrap "maxBound" maxBound

instance (Additive.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM (fromNumberUnsafe . abs) arbitrary
-}

instance RealIntegral.C a => RealIntegral.C (T a) where
   quot = lift2 quot
   rem  = lift2 rem
   quotRem x y =
      mapPair
         (fromNumberUnsafe, fromNumberUnsafe)
         (quotRem (toNumber x) (toNumber y))

instance (Ord a, Integral.C a) => Integral.C (T a) where
   div  = lift2 div
   mod  = lift2 mod
   divMod x y =
      mapPair
         (fromNumberUnsafe, fromNumberUnsafe)
         (divMod (toNumber x) (toNumber y))

instance (Ord a, Field.C a) => Field.C (T a) where
   fromRational' = fromNumberWrap "fromRational" . fromRational'
   (/) = lift2 (/)


instance (ZeroTestable.C a, Ord a, Absolute.C a) => Absolute.C (T a) where
   abs    = lift abs
   signum = lift signum

instance (ZeroTestable.C a, RealRing.C a) => RealRing.C (T a) where
   splitFraction = mapSnd fromNumberUnsafe . splitFraction . toNumber
   truncate = truncate . toNumber
   round    = round    . toNumber
   ceiling  = ceiling  . toNumber
   floor    = floor    . toNumber

instance (Ord a, Algebraic.C a) => Algebraic.C (T a) where
   sqrt = lift sqrt
   (^/) x r = lift (^/ r) x

instance (Ord a, Trans.C a) => Trans.C (T a) where
   pi = fromNumber pi
   exp  = lift exp
   log  = liftWrap "log" log
   (**) = lift2 (**)
   logBase = liftWrap "logBase" . logBase . toNumber
   sin = liftWrap "sin" sin
   tan = liftWrap "tan" tan
   cos = liftWrap "cos" cos
   asin = liftWrap "asin" asin
   atan = liftWrap "atan" atan
   acos = liftWrap "acos" acos
   sinh = liftWrap "sinh" sinh
   tanh = liftWrap "tanh" tanh
   cosh = liftWrap "cosh" cosh
   asinh = liftWrap "asinh" asinh
   atanh = liftWrap "atanh" atanh
   acosh = liftWrap "acosh" acosh


type Ratio a  = T (R.T a)
type Rational = T R.Rational


{- legacy instances already defined in non-negative package -}
