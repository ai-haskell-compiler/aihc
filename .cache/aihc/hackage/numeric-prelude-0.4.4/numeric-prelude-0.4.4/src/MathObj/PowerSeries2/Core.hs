{-# LANGUAGE RebindableSyntax #-}
module MathObj.PowerSeries2.Core where

import qualified MathObj.PowerSeries as PS
import qualified MathObj.PowerSeries.Core as PSCore

import qualified Algebra.Differential   as Differential
import qualified Algebra.Vector         as Vector
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import NumericPrelude.Base


type T a = [[a]]


lift0fromPowerSeries :: [PS.T a] -> T a
lift0fromPowerSeries = map PS.coeffs

lift1fromPowerSeries ::
   ([PS.T a] -> [PS.T a]) -> (T a -> T a)
lift1fromPowerSeries f x0 =
   map PS.coeffs (f (map PS.fromCoeffs x0))

lift2fromPowerSeries ::
   ([PS.T a] -> [PS.T a] -> [PS.T a]) -> (T a -> T a -> T a)
lift2fromPowerSeries f x0 x1 =
   map PS.coeffs (f (map PS.fromCoeffs x0) (map PS.fromCoeffs x1))


{- * Series arithmetic -}

add, sub :: (Additive.C a) => T a -> T a -> T a
add = PSCore.add
sub = PSCore.sub

negate :: (Additive.C a) => T a -> T a
negate = PSCore.negate


scale :: Ring.C a => a -> T a -> T a
scale = map . (Vector.*>)

mul :: Ring.C a => T a -> T a -> T a
mul = lift2fromPowerSeries PSCore.mul


divide :: (Field.C a) =>
   T a -> T a -> T a
divide = lift2fromPowerSeries PSCore.divide


sqrt :: (Field.C a) =>
   (a -> a) -> T a -> T a
sqrt fSqRt =
   lift1fromPowerSeries $
   PSCore.sqrt (PS.const . (\[x] -> fSqRt x) . PS.coeffs)

pow :: (Field.C a) =>
   (a -> a) -> a -> T a -> T a
pow fPow expon =
   lift1fromPowerSeries $
   PSCore.pow (PS.const . (\[x] -> fPow x) . PS.coeffs) (PS.const expon)


swapVariables :: T a -> T a
swapVariables = map reverse


differentiate0 :: (Ring.C a) => T a -> T a
differentiate0 =
   swapVariables . differentiate1 . swapVariables

differentiate1 :: (Ring.C a) => T a -> T a
differentiate1 = lift1fromPowerSeries $ map Differential.differentiate

integrate0 :: (Field.C a) => [a] -> T a -> T a
integrate0 cs =
   swapVariables . integrate1 cs . swapVariables

integrate1 :: (Field.C a) => [a] -> T a -> T a
integrate1 = zipWith PSCore.integrate



{- |
Since the inner series must start with a zero,
the first term is omitted in y.
-}
compose :: (Ring.C a) => [a] -> T a -> T a
compose = lift1fromPowerSeries . PSCore.compose . map PS.const
