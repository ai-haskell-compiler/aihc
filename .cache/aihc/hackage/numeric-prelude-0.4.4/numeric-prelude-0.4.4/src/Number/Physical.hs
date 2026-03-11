{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Numeric values combined with abstract Physical Units
-}

module Number.Physical where

import qualified Number.Physical.Unit as Unit

import           Algebra.OccasionallyScalar  as OccScalar
import qualified Algebra.VectorSpace         as VectorSpace
import qualified Algebra.Module              as Module
import qualified Algebra.Vector              as Vector
import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Algebraic           as Algebraic
import qualified Algebra.Field               as Field
import qualified Algebra.Absolute            as Absolute
import qualified Algebra.Ring                as Ring
import qualified Algebra.Additive            as Additive
import qualified Algebra.ZeroTestable        as ZeroTestable

import qualified Algebra.ToInteger      as ToInteger

import qualified Number.Ratio as Ratio

import Control.Monad (guard, liftM, liftM2, ap)
import Control.Applicative (Applicative(pure, (<*>)))

import Data.Maybe.HT(toMaybe)
import Data.Maybe(fromMaybe)

import NumericPrelude.Numeric
import NumericPrelude.Base


-- | A Physics.Quantity.Value.T combines a numeric value with a physical unit.
data T i a = Cons (Unit.T i) a

-- | Construct a physical value from a numeric value and
-- the full vector representation of a unit.
quantity :: (Ord i, Enum i, Ring.C a) => [Int] -> a -> T i a
quantity v = Cons (Unit.fromVector v)

fromScalarSingle :: a -> T i a
fromScalarSingle = Cons Unit.scalar

-- | Test for the neutral Unit.T. Also a zero has a unit!
isScalar :: T i a -> Bool
isScalar (Cons u _) = Unit.isScalar u


{- Using (((join.).).liftM2) you can turn madd and msub
   into operations that map Maybes to Maybes -}

-- | apply a function to the numeric value while preserving the unit
lift :: (a -> b) -> T i a -> T i b
lift f (Cons xu x) = Cons xu (f x)

lift2 :: (Eq i) => String -> (a -> b -> c) -> T i a -> T i b -> T i c
lift2 opName op x y =
   fromMaybe (errorUnitMismatch opName) (lift2Maybe op x y)

lift2Maybe :: (Eq i) => (a -> b -> c) -> T i a -> T i b -> Maybe (T i c)
lift2Maybe op (Cons xu x) (Cons yu y) =
   toMaybe (xu==yu) (Cons xu (op x y))

lift2Gen :: (Eq i) => String -> (a -> b -> c) -> T i a -> T i b -> c
lift2Gen opName op (Cons xu x) (Cons yu y) =
   if (xu==yu)
     then op x y
     else errorUnitMismatch opName

errorUnitMismatch :: String -> a
errorUnitMismatch opName =
   error ("Physics.Quantity.Value."++opName++": units mismatch")



-- | Add two values if the units match, otherwise return Nothing
addMaybe :: (Eq i, Additive.C a) =>
  T i a -> T i a -> Maybe (T i a)
addMaybe = lift2Maybe (+)

-- | Subtract two values if the units match, otherwise return Nothing
subMaybe :: (Eq i, Additive.C a) =>
  T i a -> T i a -> Maybe (T i a)
subMaybe = lift2Maybe (-)


scale :: (Ord i, Ring.C a) => a -> T i a -> T i a
scale x = lift (x*)

ratPow :: Trans.C a => Ratio.T Int -> T i a -> T i a
ratPow expo (Cons xu x) =
  Cons (Unit.ratScale expo xu) (x ** fromRatio expo)

ratPowMaybe :: (Trans.C a) =>
    Ratio.T Int -> T i a -> Maybe (T i a)
ratPowMaybe expo (Cons xu x) =
  fmap (flip Cons (x ** fromRatio expo)) (Unit.ratScaleMaybe expo xu)

fromRatio :: (Field.C b, ToInteger.C a) => Ratio.T a -> b
fromRatio expo = fromIntegral (numerator expo) /
                 fromIntegral (denominator expo)



instance (ZeroTestable.C v) => ZeroTestable.C (T a v) where
  isZero (Cons _ x) = isZero x

instance (Eq i, Eq a) => Eq (T i a) where
  (==) = lift2Gen "(==)" (==)

instance (Ord i, Enum i, Show a) => Show (T i a) where
  --show (Cons xu x) = show x ++ " !* " ++ show (Unit.toVector xu)
  show (Cons xu x) = "quantity " ++ show (Unit.toVector xu) ++ " " ++ show x

instance (Ord i, Additive.C a) => Additive.C (T i a) where
  zero   = fromScalarSingle zero
  -- Add two values if the units match, otherwise raise an error
  (+)    = lift2 "(+)" (+)
  -- Subtract two values if the units match, otherwise raise an error
  (-)    = lift2 "(-)" (-)
  negate = lift negate

instance (Ord i, Ring.C a) => Ring.C (T i a) where
  (Cons xu x) * (Cons yu y) = Cons (xu+yu) (x*y)
  fromInteger = fromScalarSingle . fromInteger

instance (Ord i, Ord a) => Ord (T i a) where
  max     = lift2    "max"     max
  min     = lift2    "min"     min
  compare = lift2Gen "compare" compare
  (<)     = lift2Gen "(<)"     (<)
  (>)     = lift2Gen "(>)"     (>)
  (<=)    = lift2Gen "(<=)"    (<=)
  (>=)    = lift2Gen "(>=)"    (>=)

{-
  Are absolute value and signum sensible for unit values?
  What is the sign, what is the absolute value?
  We could see it this way:
  The absolute value has no unit and
  the signum contains the unit and the scalar's sign.
  However the units contain also information of magnitude.
  E.g. if the base unit would be gramm instead kilogramm
  then the scalars would grow to a factor thousand.

  So is it better to give
  the absolute value unit and the absolute value of the scalar and
  the signum has no unit and the signum of the scalar?
  But the unit may also carry a kind of 'negativity' inside,
  e.g. the electric charge.

  It seems that there is no clear answer.
  However in my synthesizer application
  I need absolute values for sample rates and amplitudes.
  There the second interpretation is needed.
-}
instance (Ord i, Absolute.C a) => Absolute.C (T i a) where
  abs               = lift abs
  signum (Cons _ x) = fromScalarSingle (signum x)


instance (Ord i, Field.C a) => Field.C (T i a) where
  (Cons xu x) / (Cons yu y) = Cons (xu-yu) (x/y)
  fromRational' = fromScalarSingle . fromRational'

instance (Ord i, Algebraic.C a) => Algebraic.C (T i a) where
  sqrt (Cons xu x) = Cons (Unit.ratScale 0.5 xu) (sqrt x)
  Cons xu x ^/ y =
     Cons (Unit.ratScale (fromRational' y) xu) (x ^/ y)

instance (Ord i, Trans.C a) => Trans.C (T i a) where
  pi      = fromScalarSingle pi
  log     = liftM  log
  exp     = liftM  exp
  logBase = liftM2 logBase
  (**)    = liftM2 (**)
  cos     = liftM  cos
  tan     = liftM  tan
  sin     = liftM  sin
  acos    = liftM  acos
  atan    = liftM  atan
  asin    = liftM  asin
  cosh    = liftM  cosh
  tanh    = liftM  tanh
  sinh    = liftM  sinh
  acosh   = liftM  acosh
  atanh   = liftM  atanh
  asinh   = liftM  asinh

instance Ord i => Vector.C (T i) where
  zero  = zero
  (<+>) = (+)
  (*>)  = scale

instance (Ord i, Module.C a v) => Module.C a (T i v) where
  x *> (Cons yu y) = Cons yu (x Module.*> y)

instance (Ord i, VectorSpace.C a v) => VectorSpace.C a (T i v)


instance (OccScalar.C a v)
      => OccScalar.C a (T i v) where
   toScalar = toScalarDefault
   toMaybeScalar (Cons xu x)
            = guard (Unit.isScalar xu) >> toMaybeScalar x
   fromScalar = fromScalarSingle . fromScalar



{- Operators for lifting scalar operations to
   operations on physical values -}
instance Functor (T i) where
  fmap f (Cons xu x) =
    if Unit.isScalar xu
    then fromScalarSingle (f x)
    else error "Physics.Quantity.Value.fmap: function for scalars, only"

instance Applicative (T a) where
   (<*>) = ap
   pure = fromScalarSingle

instance Monad (T i) where
   (>>=) (Cons xu x) f =
    if Unit.isScalar xu
    then f x
    else error "Physics.Quantity.Value.(>>=): function for scalars, only"
   return = pure
