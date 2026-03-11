module Test.Sound.Synthesizer.Basic.ToneModulation where

import qualified Synthesizer.Interpolation  as Interpolation
import Synthesizer.Interpolation (margin, )

import qualified Synthesizer.Basic.Phase          as Phase
import qualified Synthesizer.Basic.ToneModulation as ToneMod

import qualified Test.Sound.Synthesizer.Plain.Interpolation as InterpolationTest

import Test.QuickCheck (quickCheck, Property, (==>), Testable, )

import qualified Number.NonNegative       as NonNeg

import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


untangleShapePhase :: (Field.C a, Eq a) =>
   Int -> a -> (a, a) -> Property
untangleShapePhase periodInt period c =
   period /= zero ==>
      ToneMod.untangleShapePhase periodInt period c ==
      ToneMod.untangleShapePhaseAnalytic periodInt period c

flattenShapePhase :: (RealField.C a) =>
   Int -> a -> (a, Phase.T a) -> Property
flattenShapePhase periodInt period c =
   period /= zero ==>
      ToneMod.flattenShapePhase periodInt period c ==
      ToneMod.flattenShapePhaseAnalytic periodInt period c


-- * auxiliary quickCheck functions

{-
Although that looks like a too small value, it is actually right,
because numberLeap counts intervals of size periodInt, not single elements.
So numberLeap=2 like in linear interpolation means 2*periodInt.
-}
minLength ::
   Interpolation.T a v ->
   Interpolation.T a v ->
   Int -> NonNeg.Int -> Int
minLength ipLeap ipStep =
   minLengthMargin (margin ipLeap) (margin ipStep)

minLengthMargin ::
   Interpolation.Margin ->
   Interpolation.Margin ->
   Int -> NonNeg.Int -> Int
minLengthMargin marginLeap marginStep periodInt ext =
   ToneMod.interpolationNumber
      marginLeap marginStep periodInt +
   NonNeg.toNumber ext



shapeLimits ::
   Interpolation.T a v ->
   Interpolation.T a v ->
   Int -> Int -> (Int, Int)
shapeLimits ipLeap ipStep periodInt len =
   ToneMod.shapeLimits
      (margin ipLeap) (margin ipStep)
      periodInt len



testRationalLineIp :: Testable quickCheck =>
   (InterpolationTest.LinePreserving Rational Rational -> quickCheck) -> IO ()
testRationalLineIp f  =  quickCheck f

testRationalIp :: Testable quickCheck =>
   (InterpolationTest.T Rational Rational -> quickCheck) -> IO ()
testRationalIp f  =  quickCheck f


tests :: [(String, IO ())]
tests =
   ("untangleShapePhase",
      quickCheck $ \periodInt period ->
         untangleShapePhase periodInt (period :: Rational)) :
   ("flattenShapePhase",
      quickCheck $ \periodInt period ->
         flattenShapePhase periodInt (period :: Rational)) :
   []
