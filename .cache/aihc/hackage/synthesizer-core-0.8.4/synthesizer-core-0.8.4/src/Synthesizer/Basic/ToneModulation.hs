{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Basic.ToneModulation (
   untangleShapePhase, untangleShapePhaseAnalytic,
   flattenShapePhase, flattenShapePhaseAnalytic,
   shapeLimits,
   interpolationOffset, interpolationNumber,
   Coords, Skip,
   ) where

import qualified Synthesizer.Basic.Phase as Phase

import Synthesizer.Interpolation (Margin, marginOffset, marginNumber, )

import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
Convert from the (shape,phase) parameter pair
to the index within a wave (step) and the index of a wave (leap)
in the sampled prototype tone.

For this routine it would be simpler,
if @shape@ would measure in multiples of @period@
(we would only need a Ring instance),
but for 'shapeLimit' it is better the way it is.
-}
{-# INLINE untangleShapePhase #-}
untangleShapePhase :: (Field.C a) =>
   Int -> a -> (a, a) -> (a, a)
untangleShapePhase periodInt period (shape,phase) =
   let leap = shape/period - phase
       step = shape - leap * fromIntegral periodInt
   in  (leap, step)

untangleShapePhaseAnalytic :: (Field.C a) =>
   Int -> a -> (a, a) -> (a, a)
untangleShapePhaseAnalytic periodInt period (shape,phase) =
   let periodRound = fromIntegral periodInt
       vLeap = (periodRound, periodRound-period)
       vStep = (1,1)
   in  solveSLE2 (vLeap,vStep) (shape,period*phase)

{-
Cramer's rule

see HTam/Numerics/ZeroFinder/Root, however the matrix is transposed
-}
solveSLE2 :: Field.C a => ((a,a), (a,a)) -> (a,a) -> (a,a)
solveSLE2 a@(a0,a1) b =
   let det = det2 a
   in  (det2 (b, a1) / det,
        det2 (a0, b) / det)

det2 :: Ring.C a => ((a,a), (a,a)) -> a
det2 ((a00,a10),(a01,a11)) =
   a00*a11 - a10*a01

{-
transpose :: ((a,a), (a,a)) -> ((a,a), (a,a))
transpose ((a00,a10),(a01,a11)) = ((a00,a01),(a10,a11))
-}


{-# INLINE flattenShapePhase #-}
flattenShapePhase, flattenShapePhaseAnalytic :: RealField.C a =>
      Int
   -> a
   -> (a, Phase.T a)
   -> (Int, (a, a))
flattenShapePhase periodInt period (shape,phase) =
   let xLeap = shape/period - Phase.toRepresentative phase
       qLeap = fraction xLeap
       xStep = shape - qLeap * fromIntegral periodInt
       (n,qStep) = splitFraction xStep
   in  (n,(qLeap,qStep))

flattenShapePhaseAnalytic periodInt period (shape,phase) =
   let (xLeap,xStep) =
          untangleShapePhase periodInt period (shape, Phase.toRepresentative phase)
       (nLeap,qLeap) = splitFraction xLeap
       (nStep,qStep) = splitFraction xStep
       {- reverse solveSLE2 for the shape parameter
          with respect to the rounded (wave,shape) coordinates -}
       n = nStep + nLeap * periodInt
   in  (n,(qLeap,qStep))


shapeLimits :: Ring.C t =>
   Margin ->
   Margin ->
   Int ->
   t ->
   (t, t)
shapeLimits marginLeap marginStep periodInt len =
   let minShape =
          fromIntegral $
          interpolationOffset marginLeap marginStep periodInt +
          periodInt
       maxShape =
          minShape + len -
          fromIntegral (interpolationNumber marginLeap marginStep periodInt)
   in  (minShape, maxShape)

interpolationOffset ::
   Margin ->
   Margin ->
   Int ->
   Int
interpolationOffset marginLeap marginStep periodInt =
   marginOffset marginStep +
   marginOffset marginLeap * periodInt

interpolationNumber ::
   Margin ->
   Margin ->
   Int ->
   Int
interpolationNumber marginLeap marginStep periodInt =
   marginNumber marginStep +
   marginNumber marginLeap * periodInt



type Coords t = (Int,(Int,(t,t)))
type Skip   t = (Int, (t, Phase.T t))
