{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
A wrapper that provides instances of Haskell 98 and NumericPrelude
numeric type classes
for types that have NumericPrelude instances.
-}
module MathObj.Wrapper.NumericPrelude where

import qualified Algebra.Absolute as Absolute
import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.FloatingPoint as Float
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.RealField as RealField
import qualified Algebra.RealIntegral as RealIntegral
import qualified Algebra.RealRing as RealRing
import qualified Algebra.RealTranscendental as RealTrans
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.ToRational as ToRational
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Units as Units
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Algebra.NormedSpace.Euclidean as NormEuc
import qualified Algebra.NormedSpace.Maximum as NormMax
import qualified Algebra.NormedSpace.Sum as NormSum
import qualified Algebra.OccasionallyScalar as OccScalar
import qualified Algebra.Differential as Differential
import qualified Algebra.DivisibleSpace as Divisible
import qualified Algebra.VectorSpace as VectorSpace
import qualified Algebra.Module as Module

import qualified Number.Ratio as Ratio

import Data.Ix (Ix, )

import Data.Tuple.HT (mapPair, )


{- |
This makes a type usable with Haskell98 type classes
that was initially implemented for NumericPrelude typeclasses.
E.g. if @a@ is in class 'Ring.C',
then @T a@ is both in class 'Num' and in 'Ring.C'.

You can even lift container types.
If @Polynomial a@ is in 'Ring.C' for all types @a@ that are in 'Ring.C',
then @T (Polynomial (MathObj.Wrapper.Haskell98.T a))@
is in 'Num' for all types @a@ that are in 'Num'.
-}
newtype T a = Cons {decons :: a}
   deriving
      (Show, Eq, Ord, Ix, Bounded, Enum,
       Ring.C, Additive.C, Field.C, Algebraic.C, Trans.C,
       Integral.C, PID.C, Units.C,
       Absolute.C, ZeroTestable.C,
       RealField.C, RealIntegral.C, RealRing.C, RealTrans.C,
       ToInteger.C, ToRational.C, Float.C,
       Differential.C)

{-# INLINE lift1 #-}
lift1 :: (a -> b) -> T a -> T b
lift1 f (Cons a) = Cons (f a)

{-# INLINE lift2 #-}
lift2 :: (a -> b -> c) -> T a -> T b -> T c
lift2 f (Cons a) (Cons b) = Cons (f a b)


instance Functor T where
   {-# INLINE fmap #-}
   fmap f (Cons a) = Cons (f a)


{-
instance Enum a => Enum (T a) where
   succ (Cons n) = Cons (succ n)
   pred (Cons n) = Cons (pred n)
   toEnum n = Cons (toEnum n)
   fromEnum (Cons n) = fromEnum n
   enumFrom (Cons n) =
      map Cons (enumFrom n)
   enumFromThen (Cons n) (Cons m) =
      map Cons (enumFromThen n m)
   enumFromTo (Cons n) (Cons m) =
      map Cons (enumFromTo n m)
   enumFromThenTo (Cons n) (Cons m) (Cons p) =
      map Cons (enumFromThenTo n m p)
-}

instance (Ring.C a, Absolute.C a, Eq a, Show a) => Num (T a) where
   (+) = lift2 (Additive.+)
   (-) = lift2 (Additive.-)
   negate = lift1 Additive.negate

   fromInteger = Cons . Ring.fromInteger
   (*) = lift2 (Ring.*)

   abs = lift1 Absolute.abs
   signum = lift1 Absolute.signum

instance (RealIntegral.C a, Absolute.C a, ToInteger.C a, Ord a, Enum a, Show a) => Integral (T a) where
   quot = lift2 RealIntegral.quot
   rem = lift2 RealIntegral.rem
   quotRem (Cons a) (Cons b) =
      mapPair (Cons, Cons) (RealIntegral.quotRem a b)
   div = lift2 Integral.div
   mod = lift2 Integral.mod
   divMod (Cons a) (Cons b) =
      mapPair (Cons, Cons) (Integral.divMod a b)
   toInteger (Cons a) = ToInteger.toInteger a

instance (Field.C a, Absolute.C a, Eq a, Show a) => Fractional (T a) where
   (/) = lift2 (Field./)
   recip = lift1 Field.recip
   fromRational = Cons . Field.fromRational

instance (Trans.C a, Absolute.C a, Eq a, Show a) => Floating (T a) where
   sqrt    = lift1 Algebraic.sqrt
   pi      = Cons Trans.pi
   log     = lift1 Trans.log
   exp     = lift1 Trans.exp
   logBase = lift2 Trans.logBase
   (**)    = lift2 (Trans.**)
   cos     = lift1 Trans.cos
   tan     = lift1 Trans.tan
   sin     = lift1 Trans.sin
   acos    = lift1 Trans.acos
   atan    = lift1 Trans.atan
   asin    = lift1 Trans.asin
   cosh    = lift1 Trans.cosh
   tanh    = lift1 Trans.tanh
   sinh    = lift1 Trans.sinh
   acosh   = lift1 Trans.acosh
   atanh   = lift1 Trans.atanh
   asinh   = lift1 Trans.asinh

instance (ToRational.C a, Absolute.C a, Ord a, Show a) => Real (T a) where
   toRational (Cons a) =
      Ratio.toRational98 (ToRational.toRational a)

instance (Field.C a, RealRing.C a, ToRational.C a, Absolute.C a, Ord a, Show a) => RealFrac (T a) where
   properFraction (Cons a) =
      let b = RealRing.truncate a
      in  (fromInteger b, Cons (a Additive.- Ring.fromInteger b))
   ceiling (Cons a) = fromInteger (RealRing.ceiling a)
   floor (Cons a) = fromInteger (RealRing.floor a)
   truncate (Cons a) = fromInteger (RealRing.truncate a)
   round (Cons a) = fromInteger (RealRing.round a)

instance (RealTrans.C a, Float.C a, ToRational.C a, Absolute.C a, Ord a, Show a) => RealFloat (T a) where
   atan2 = RealTrans.atan2
   floatRadix = Float.radix . decons
   floatDigits = Float.digits . decons
   floatRange = Float.range . decons
   decodeFloat = Float.decode . decons
   encodeFloat m = Cons . Float.encode m
   exponent = Float.exponent . decons
   significand = lift1 Float.significand
   scaleFloat = lift1 . Float.scale
   isNaN = Float.isNaN . decons
   isInfinite = Float.isInfinite . decons
   isDenormalized = Float.isDenormalized . decons
   isNegativeZero = Float.isNegativeZero . decons
   isIEEE = Float.isIEEE . decons

{-
instance Additive.C (T a) where
instance Ring.C (T a) where
instance Field.C (T a) where
instance Algebraic.C (T a) where
instance Trans.C (T a) where

instance Units.C (T a) where
instance Integral.C (T a) where
instance PID.C (T a) where

instance ZeroTestable.C (T a) where
instance Absolute.C (T a) where
instance (Ord a) => RealField.C (T a) where
instance (Ord a) => RealIntegral.C (T a) where
instance (Ord a) => RealRing.C (T a) where
instance (Ord a) => RealTrans.C (T a) where

instance (Ord a) => ToInteger.C (T a) where
instance (Ord a) => ToRational.C (T a) where
-}

instance Module.C a v => Module.C (T a) (T v) where
   (*>) = lift2 (Module.*>)

instance VectorSpace.C a v => VectorSpace.C (T a) (T v) where

instance Divisible.C a v => Divisible.C (T a) (T v) where
   (</>) = lift2 (Divisible.</>)

instance OccScalar.C a v => OccScalar.C (T a) (T v) where
   toScalar = lift1 OccScalar.toScalar
   toMaybeScalar (Cons a) = fmap Cons (OccScalar.toMaybeScalar a)
   fromScalar = lift1 OccScalar.fromScalar

instance NormEuc.Sqr a v => NormEuc.Sqr (T a) (T v) where
   normSqr = lift1 NormEuc.normSqr

instance NormEuc.C a v => NormEuc.C (T a) (T v) where
   norm = lift1 NormEuc.norm

instance NormMax.C a v => NormMax.C (T a) (T v) where
   norm = lift1 NormMax.norm

instance NormSum.C a v => NormSum.C (T a) (T v) where
   norm = lift1 NormSum.norm


unimplemented :: String -> a
unimplemented name =
   error (name ++ "cannot be implemented in terms of NumericPrelude type classes")
