{-# LANGUAGE RebindableSyntax #-}
{- |
Define Transcendental functions on arbitrary fields.
These functions are defined for only a few (in most cases only one) arguments,
that's why we discourage making these types instances of 'Algebra.Transcendental.C'.
But instances of 'Algebra.Transcendental.C' can be useful when working with power series.
If you intend to work with power series with 'Rational' coefficients,
you might consider using @MathObj.PowerSeries.T (Number.PartiallyTranscendental.T Rational)@
instead of @MathObj.PowerSeries.T Rational@.
-}
module Number.PartiallyTranscendental (T, fromValue, toValue) where

import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Prelude as P


newtype T a = Cons {toValue :: a}
   deriving (Eq, Ord, Show)

fromValue :: a -> T a
fromValue = lift0

lift0 :: a -> T a
lift0 = Cons

lift1 :: (a -> a) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

lift2 :: (a -> a -> a) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)


instance (Additive.C a) => Additive.C (T a) where
    negate = lift1 negate
    (+)    = lift2 (+)
    (-)    = lift2 (-)
    zero   = lift0 zero

instance (Ring.C a) => Ring.C (T a) where
    one           = lift0 one
    fromInteger n = lift0 (fromInteger n)
    (*)           = lift2 (*)

instance (Field.C a) => Field.C (T a) where
    (/) = lift2 (/)

instance (Algebraic.C a) => Algebraic.C (T a) where
    sqrt x = lift1 sqrt x
    root n = lift1 (Algebraic.root n)
    (^/) x y = lift1 (^/y) x

instance (Algebraic.C a, Eq a) => Transcendental.C (T a) where
    pi = undefined
    exp = \0 -> 1
    sin = \0 -> 0
    cos = \0 -> 1
    tan = \0 -> 0
    x ** y = if x==1 || y==0
               then 1
               else error "partially transcendental power undefined"
    log  = \1 -> 0
    asin = \0 -> 0
    acos = \1 -> 0
    atan = \0 -> 0



instance (P.Num a) => P.Num (T a) where
   fromInteger = lift0 . P.fromInteger
   negate = lift1 P.negate
   (+)    = lift2 (P.+)
   (-)    = lift2 (P.-)
   (*)    = lift2 (P.*)
   abs    = lift1 P.abs
   signum = lift1 P.signum

instance (P.Fractional a) => P.Fractional (T a) where
   fromRational = P.fromRational
   (/) = lift2 (P./)
