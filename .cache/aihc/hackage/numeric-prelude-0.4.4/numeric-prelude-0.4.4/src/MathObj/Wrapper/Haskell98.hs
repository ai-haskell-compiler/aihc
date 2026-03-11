{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
A wrapper that provides instances of Haskell 98 and NumericPrelude
numeric type classes
for types that have Haskell 98 instances.
-}
module MathObj.Wrapper.Haskell98 where

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

import qualified Number.Ratio as Ratio

import qualified Algebra.RealRing98 as RealRing98

import Data.Ix (Ix, )

import Data.Tuple.HT (mapPair, )


{- |
This makes a type usable in the NumericPrelude framework
that was initially implemented for Haskell98 typeclasses.
E.g. if @a@ is in class 'Num',
then @T a@ is both in class 'Num' and in 'Ring.C'.

You can even lift container types.
If @Polynomial a@ is in 'Num' for all types @a@ that are in 'Num',
then @T (Polynomial (MathObj.Wrapper.NumericPrelude.T a))@
is in 'Ring.C' for all types @a@ that are in 'Ring.C'.
-}
newtype T a = Cons {decons :: a}
   deriving
      (Show, Eq, Ord, Ix, Bounded, Enum,
       Num, Integral, Fractional, Floating,
       Real, RealFrac, RealFloat)


{-# INLINE lift1 #-}
lift1 :: (a -> b) -> T a -> T b
lift1 f (Cons a) = Cons (f a)

{-# INLINE lift2 #-}
lift2 :: (a -> b -> c) -> T a -> T b -> T c
lift2 f (Cons a) (Cons b) = Cons (f a b)


{-# INLINE unliftF1 #-}
unliftF1 :: Functor f => (f (T a) -> f (T b)) -> f a -> f b
unliftF1 f a = fmap decons $ f (fmap Cons a)

{-# INLINE unliftF2 #-}
unliftF2 :: Functor f => (f (T a) -> f (T b) -> f (T c)) -> f a -> f b -> f c
unliftF2 f a b = fmap decons $ f (fmap Cons a) (fmap Cons b)


instance Functor T where
   {-# INLINE fmap #-}
   fmap f (Cons a) = Cons (f a)


instance Num a => Additive.C (T a) where
   zero = 0
   (+) = lift2 (+)
   (-) = lift2 (-)
   negate = lift1 negate

instance (Num a) => Ring.C (T a) where
   fromInteger = Cons . fromInteger
   (*) = lift2 (*)
   (^) a n = lift1 (^n) a

instance (Fractional a) => Field.C (T a) where
   fromRational' r = Cons (fromRational (Ratio.toRational98 r))
   (/) = lift2 (/)
   recip = lift1 recip
   (^-) a n = lift1 (^^n) a

instance (Floating a) => Algebraic.C (T a) where
   sqrt = lift1 sqrt
   (^/) a r = lift1 (** fromRational (Ratio.toRational98 r)) a
   root n a = lift1 (** recip (fromInteger n)) a

instance (Floating a) => Trans.C (T a) where
   pi      = Cons pi
   log     = lift1 log
   exp     = lift1 exp
   logBase = lift2 logBase
   (**)    = lift2 (**)
   cos     = lift1 cos
   tan     = lift1 tan
   sin     = lift1 sin
   acos    = lift1 acos
   atan    = lift1 atan
   asin    = lift1 asin
   cosh    = lift1 cosh
   tanh    = lift1 tanh
   sinh    = lift1 sinh
   acosh   = lift1 acosh
   atanh   = lift1 atanh
   asinh   = lift1 asinh

instance (Integral a) => Integral.C (T a) where
   div = lift2 div
   mod = lift2 mod
   divMod (Cons a) (Cons b) =
      mapPair (Cons, Cons) (divMod a b)

instance (Integral a) => Units.C (T a) where
   isUnit = unimplemented "isUnit"
   stdAssociate = unimplemented "stdAssociate"
   stdUnit = unimplemented "stdUnit"
   stdUnitInv = unimplemented "stdUnitInv"

instance (Integral a) => PID.C (T a) where
   gcd = gcd
   lcm = lcm

instance (Eq a, Num a) => ZeroTestable.C (T a) where
   isZero (Cons a) = a==0

instance (Num a) => Absolute.C (T a) where
   abs = abs
   signum = signum

instance (RealFrac a) => RealRing.C (T a) where
   splitFraction (Cons a) =
      mapPair (Ring.fromInteger, Cons)
         (RealRing98.fixSplitFraction (properFraction a))
   fraction (Cons a) =
      Cons (RealRing98.fixFraction (RealRing98.signedFraction a))
   ceiling (Cons a) = Ring.fromInteger (ceiling a)
   floor (Cons a) = Ring.fromInteger (floor a)
   truncate (Cons a) = Ring.fromInteger (truncate a)
   round (Cons a) = Ring.fromInteger (round a)

instance (RealFrac a) => RealField.C (T a) where

instance (RealFloat a) => RealTrans.C (T a) where
   atan2 = atan2

instance (Integral a) => RealIntegral.C (T a) where
   quot = lift2 quot
   rem = lift2 rem
   quotRem (Cons a) (Cons b) =
      mapPair (Cons, Cons) (quotRem a b)

instance (Integral a) => ToInteger.C (T a) where
   toInteger (Cons a) = toInteger a

instance (Real a) => ToRational.C (T a) where
   toRational (Cons a) = Field.fromRational (toRational a)

instance (RealFloat a) => Float.C (T a) where
   radix = floatRadix . decons
   digits = floatDigits . decons
   range = floatRange . decons
   decode = decodeFloat . decons
   encode m = Cons . encodeFloat m
   exponent = exponent . decons
   significand = lift1 significand
   scale = lift1 . scaleFloat
   isNaN = isNaN . decons
   isInfinite = isInfinite . decons
   isDenormalized = isDenormalized . decons
   isNegativeZero = isNegativeZero . decons
   isIEEE = isIEEE . decons



unimplemented :: String -> a
unimplemented name =
   error (name ++ "cannot be implemented in terms of Haskell98 type classes")
