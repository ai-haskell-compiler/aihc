{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

A type for non-negative numbers.
It performs a run-time check at construction time (i.e. at run-time)
and is a member of the non-negative number type class
'Numeric.NonNegative.Class.C'.
-}
module Numeric.NonNegative.Wrapper
   (T, fromNumber, fromNumberMsg, fromNumberClip, fromNumberUnsafe, toNumber,
    Int, Integer, Float, Double, Ratio, Rational) where

import qualified Numeric.NonNegative.Class as NonNeg
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.Semigroup (Semigroup(sconcat, (<>)))
import Data.List.NonEmpty (NonEmpty((:|)))

import Test.QuickCheck (Arbitrary(arbitrary, shrink))
import Data.Tuple.HT (mapPair, mapSnd, )
import Control.Monad (liftM)

import qualified Data.Ratio as R
import qualified Prelude as P
import Prelude hiding (Int, Integer, Float, Double, Rational)


newtype T a = Cons {unwrap :: a}
   deriving (Eq, Ord)

instance Show a => Show (T a) where
   showsPrec p (Cons a) = showsPrec p a


{- |
Convert a number to a non-negative number.
If a negative number is given, an error is raised.
-}
fromNumber :: (Ord a, Num a) =>
      a
   -> T a
fromNumber = fromNumberMsg "fromNumber"

fromNumberMsg :: (Ord a, Num a) =>
      String  {- ^ name of the calling function to be used in the error message -}
   -> a
   -> T a
fromNumberMsg funcName x =
   if x>=0
     then Cons x
     else error (funcName++": negative number")

fromNumberWrap :: (Ord a, Num a) =>
      String
   -> a
   -> T a
fromNumberWrap funcName =
   fromNumberMsg ("NonNegative.Wrapper."++funcName)

{- |
Convert a number to a non-negative number.
A negative number will be replaced by zero.
Use this function with care since it may hide bugs.
-}
fromNumberClip :: (Ord a, Num a) =>
      a
   -> T a
fromNumberClip = Cons . max 0

{- |
Wrap a number into a non-negative number without doing checks.
This routine exists entirely for efficiency reasons
and must be used only in cases where you are absolutely sure,
that the input number is non-negative.
-}
fromNumberUnsafe ::
      a
   -> T a
fromNumberUnsafe = Cons

{-
export only this in order to disable direct access to the record field
by record update syntax
-}
toNumber :: T a -> a
toNumber = unwrap


{- |
Results are not checked for positivity.
-}
lift :: (a -> a) -> (T a -> T a)
lift f = Cons . f . toNumber

liftWrap :: (Ord a, Num a) => String -> (a -> a) -> (T a -> T a)
liftWrap msg f = fromNumberWrap msg . f . toNumber


{- |
Results are not checked for positivity.
-}
lift2 :: (a -> a -> a) -> (T a -> T a -> T a)
lift2 f (Cons x) (Cons y) = Cons $ f x y


instance (Num a) => Semigroup (T a) where
   Cons x <> Cons y = Cons (x+y)
   sconcat (x :| xs) = Cons $ toNumber x + sum (map toNumber xs)

instance (Num a) => Monoid (T a) where
   mempty = Cons 0
   mappend (Cons x) (Cons y) = Cons (x+y)
   mconcat = Cons . sum . map toNumber

instance (Ord a, Num a) => NonNeg.C (T a) where
   split = NonNeg.splitDefault toNumber Cons

instance (Ord a, Num a) => Num (T a) where
   (+)    = lift2 (+)
   (Cons x) - (Cons y) = fromNumberWrap "-" (x-y)
   negate = liftWrap "negate" negate
   fromInteger x = fromNumberWrap "fromInteger" (fromInteger x)
   (*)    = lift2 (*)
   abs    = lift abs
   signum = lift signum

instance Real a => Real (T a) where
   toRational = toRational . toNumber

{- required for Integral instance -}
instance (Ord a, Num a, Enum a) => Enum (T a) where
   toEnum   = fromNumberWrap "toEnum" . toEnum
   fromEnum = fromEnum . toNumber

instance (Ord a, Num a, Bounded a) => Bounded (T a) where
   minBound = fromNumberClip minBound
   maxBound = fromNumberWrap "maxBound" maxBound

instance Integral a => Integral (T a) where
   toInteger = toInteger . toNumber
   quot = lift2 quot
   rem  = lift2 rem
   quotRem (Cons x) (Cons y) =
      mapPair (Cons, Cons) (quotRem x y)
   div  = lift2 div
   mod  = lift2 mod
   divMod (Cons x) (Cons y) =
      mapPair (Cons, Cons) (divMod x y)

instance (Ord a, Fractional a) => Fractional (T a) where
   fromRational = fromNumberWrap "fromRational" . fromRational
   (/) = lift2 (/)


instance (RealFrac a) => RealFrac (T a) where
   properFraction = mapSnd fromNumberUnsafe . properFraction . toNumber
   truncate = truncate . toNumber
   round    = round    . toNumber
   ceiling  = ceiling  . toNumber
   floor    = floor    . toNumber

instance (Ord a, Floating a) => Floating (T a) where
   pi = fromNumber pi
   exp  = lift exp
   sqrt = lift sqrt
   log  = liftWrap "log" log
   (**) = lift2 (**)
   logBase (Cons x) = liftWrap "logBase" (logBase x)
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


instance (Num a, Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM (Cons . abs) arbitrary
   shrink (Cons xs) = map (Cons . abs) $ shrink xs


type Int      = T P.Int
type Integer  = T P.Integer
type Ratio a  = T (R.Ratio a)
type Rational = T P.Rational
type Float    = T P.Float
type Double   = T P.Double
