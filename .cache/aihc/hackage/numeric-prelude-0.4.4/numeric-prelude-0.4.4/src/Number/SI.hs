{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Numerical values equipped with SI units.
This is considered as the user front-end.
-}

module Number.SI where

import qualified Number.SI.Unit       as SIUnit
import           Number.SI.Unit (Dimension, bytesize)

import qualified Number.Physical      as Value
import qualified Number.Physical.Unit as Unit
import qualified Number.Physical.Show as PVShow
import qualified Number.Physical.Read as PVRead
import qualified Number.Physical.UnitDatabase as UnitDatabase

import           Algebra.OccasionallyScalar  as OccScalar
import qualified Algebra.NormedSpace.Maximum as NormedMax

import qualified Algebra.VectorSpace         as VectorSpace
import qualified Algebra.Module              as Module
import qualified Algebra.Vector              as Vector
import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Algebraic           as Algebraic
import qualified Algebra.Field               as Field
import qualified Algebra.Absolute                as Absolute
import qualified Algebra.Ring                as Ring
import qualified Algebra.Additive            as Additive
import qualified Algebra.ZeroTestable        as ZeroTestable

import Data.Tuple.HT (mapFst, )

import qualified MathObj.Wrapper.Haskell98 as W98

import qualified Prelude as P

import NumericPrelude.Numeric
import NumericPrelude.Base


newtype T a v = Cons (PValue v)
   deriving (Functor)

type PValue v = Value.T Dimension v

{-
import Control.Monad

instance Functor (SIValue.T a) where
  fmap f (SIValue.Cons x) = SIValue.Cons (f x)

instance Monad (SIValue.T a) where
  (>>=) (SIValue.Cons x) f = f x
  return = SIValue.Cons
-}

{- I hoped it would be possible to replace these functions
   by fmap and monadic liftM, liftM2, return -
   but SIValue.Cons lifts from the base type 'v' to 'SIValue.T a v'
   rather than the type 'PValue v' to 'SIValue.T a v'.

   I.e.
     fmap :: (v -> v) -> SIValue.T a v -> SIValue.T a v
-}
lift :: (PValue v0 -> PValue v1) ->
            (T a v0 -> T a v1)
lift f (Cons x) = (Cons (f x))

lift2 :: (PValue v0 -> PValue v1 -> PValue v2) ->
            (T a v0 -> T a v1 -> T a v2)
lift2 f (Cons x) (Cons y) = (Cons (f x y))

liftGen :: (PValue v -> x) -> (T a v -> x)
liftGen f (Cons x) = f x

lift2Gen :: (PValue v0 -> PValue v1 -> x) ->
               (T a v0 -> T a v1 -> x)
lift2Gen f (Cons x) (Cons y) = f x y


{- There is almost nothing new to implement for SIValues.
   We have to lift existing functions to SIValues mainly. -}

scale :: Ring.C v => v -> T a v -> T a v
scale = lift . Value.scale

fromScalarSingle :: v -> T a v
fromScalarSingle = Cons . Value.fromScalarSingle


instance (ZeroTestable.C v) => ZeroTestable.C (T a v) where
  isZero = liftGen isZero

instance Eq v => Eq (T a v) where
  (==)  =  lift2Gen (==)

showNat :: (Show v, Field.C a, Ord a, NormedMax.C a v) =>
   UnitDatabase.T Dimension a -> T a v -> String
showNat db =
   liftGen (PVShow.showNat db)

instance (Show v, Ord a, Trans.C a, NormedMax.C a v) =>
    Show (T a v) where
  showsPrec prec x =
    showParen (prec > PVShow.mulPrec)
       (showNat SIUnit.databaseShow x ++)

readsNat :: (Read v, VectorSpace.C a v) =>
   UnitDatabase.T Dimension a -> Int -> ReadS (T a v)
readsNat db prec =
   map (mapFst Cons) . PVRead.readsNat db prec

instance (Read v, Ord a, Trans.C a, VectorSpace.C a v) =>
    Read (T a v) where
  readsPrec = readsNat SIUnit.databaseRead

instance (Additive.C v) => Additive.C (T a v) where
  zero   = Cons zero
  (+)    = lift2 (+)
  (-)    = lift2 (-)
  negate = lift negate

instance (Ring.C v) => Ring.C (T a v) where
  (*) = lift2 (*)
  fromInteger = Cons . fromInteger

instance (Ord v) => Ord (T a v) where
  max     = lift2    max
  min     = lift2    min
  compare = lift2Gen compare
  (<)     = lift2Gen (<)
  (>)     = lift2Gen (>)
  (<=)    = lift2Gen (<=)
  (>=)    = lift2Gen (>=)

instance (Absolute.C v) => Absolute.C (T a v) where
  abs    = lift abs
  signum = lift signum

instance (Field.C v) => Field.C (T a v) where
  (/) = lift2 (/)
  fromRational' = Cons . fromRational'

instance (Algebraic.C v) => Algebraic.C (T a v) where
  sqrt    = lift  sqrt
  x ^/ y  = lift  (^/ y) x

instance (Trans.C v) => Trans.C (T a v) where
  pi      = Cons pi
  log     = lift  log
  exp     = lift  exp
  logBase = lift2 logBase
  (**)    = lift2 (**)
  cos     = lift  cos
  tan     = lift  tan
  sin     = lift  sin
  acos    = lift  acos
  atan    = lift  atan
  asin    = lift  asin
  cosh    = lift  cosh
  tanh    = lift  tanh
  sinh    = lift  sinh
  acosh   = lift  acosh
  atanh   = lift  atanh
  asinh   = lift  asinh


instance Vector.C (T a) where
  zero  = zero
  (<+>) = (+)
  (*>)  = scale

instance (Module.C a v) => Module.C a (T b v) where
  (*>) x = lift (x Module.*>)

instance (VectorSpace.C a v) => VectorSpace.C a (T b v)

instance (Trans.C a, Ord a, OccScalar.C a v,
          Show v, NormedMax.C a v)
      => OccScalar.C a (T a v) where
   toScalar      = toScalarShow
   toMaybeScalar = liftGen toMaybeScalar
   fromScalar    = Cons . fromScalar



quantity :: (Field.C a, Field.C v) => Unit.T Dimension -> v -> T a v
quantity xu = Cons . Value.Cons xu

hertz, second, minute, hour, day, year,
 meter, liter, gramm, tonne,
 newton, pascal, bar, joule, watt,
 kelvin,
 coulomb, ampere, volt, ohm, farad,
 bit, byte, baud,
 inch, foot, yard, astronomicUnit, parsec,
 mach, speedOfLight, electronVolt,
 calorien, horsePower, accelerationOfEarthGravity ::
    (Field.C a, Field.C v) => T a v

hertz   = quantity SIUnit.frequency   1e+0
second  = quantity SIUnit.time        1e+0
minute  = quantity SIUnit.time        SIUnit.secondsPerMinute
hour    = quantity SIUnit.time        SIUnit.secondsPerHour
day     = quantity SIUnit.time        SIUnit.secondsPerDay
year    = quantity SIUnit.time        SIUnit.secondsPerYear
meter   = quantity SIUnit.length      1e+0
liter   = quantity SIUnit.volume      1e-3
gramm   = quantity SIUnit.mass        1e-3
tonne   = quantity SIUnit.mass        1e+3
newton  = quantity SIUnit.force       1e+0
pascal  = quantity SIUnit.pressure    1e+0
bar     = quantity SIUnit.pressure    1e+5
joule   = quantity SIUnit.energy      1e+0
watt    = quantity SIUnit.power       1e+0
coulomb = quantity SIUnit.charge      1e+0
ampere  = quantity SIUnit.current     1e+0
volt    = quantity SIUnit.voltage     1e+0
ohm     = quantity SIUnit.resistance  1e+0
farad   = quantity SIUnit.capacitance 1e+0
kelvin  = quantity SIUnit.temperature 1e+0
bit     = quantity SIUnit.information 1e+0
byte    = quantity SIUnit.information bytesize
baud    = quantity SIUnit.dataRate    1e+0

inch           = quantity SIUnit.length SIUnit.meterPerInch
foot           = quantity SIUnit.length SIUnit.meterPerFoot
yard           = quantity SIUnit.length SIUnit.meterPerYard
astronomicUnit = quantity SIUnit.length SIUnit.meterPerAstronomicUnit
parsec         = quantity SIUnit.length SIUnit.meterPerParsec

accelerationOfEarthGravity
             = quantity SIUnit.acceleration SIUnit.accelerationOfEarthGravity
mach         = quantity SIUnit.speed        SIUnit.mach
speedOfLight = quantity SIUnit.speed        SIUnit.speedOfLight
electronVolt = quantity SIUnit.energy       SIUnit.electronVolt
calorien     = quantity SIUnit.energy       SIUnit.calorien
horsePower   = quantity SIUnit.power        SIUnit.horsePower



instance (P.Num v) => P.Num (T a v) where
   fromInteger = fromScalarSingle . P.fromInteger
   negate = W98.unliftF1 Additive.negate
   (+)    = W98.unliftF2 (Additive.+)
   (*)    = W98.unliftF2 (Ring.*)
   abs    = W98.unliftF1 Absolute.abs
   signum = W98.unliftF1 Absolute.signum

instance (P.Fractional v) => P.Fractional (T a v) where
   fromRational = fromScalarSingle . P.fromRational
   (/) = W98.unliftF2 (Field./)
