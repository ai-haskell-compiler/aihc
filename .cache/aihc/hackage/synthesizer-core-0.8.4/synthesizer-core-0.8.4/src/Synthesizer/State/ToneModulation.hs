module Synthesizer.State.ToneModulation (
   Cell,
   makeCell,
   interpolateCell,

   Prototype,
   makePrototype,
   sampledToneCell,

   oscillatorCells,

   -- needed in Causal.ToneModulation
   checkNonNeg,

   -- for testing
   oscillatorCoords,
   limitRelativeShapes,
   limitMinRelativeValues,
   ) where

import qualified Synthesizer.Basic.ToneModulation as ToneMod
import qualified Synthesizer.Causal.Oscillator.Core as Osci

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Interpolation as Interpolation

import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.State.Signal as SigS

import qualified Synthesizer.Basic.Phase as Phase

import qualified Algebra.RealField             as RealField
import qualified Algebra.Additive              as Additive

import Data.Ord.HT (limit, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


type Cell sig y = SigS.T (sig y)

{- |
cells are organised in a transposed style,
when compared with Plain.ToneModulation
-}
{-# INLINE interpolateCell #-}
interpolateCell ::
   (SigG.Read sig y) =>
   Interpolation.T a y ->
   Interpolation.T b y ->
   (a, b) ->
   Cell sig y -> y
interpolateCell ipLeap ipStep (qLeap,qStep) =
   Interpolation.func ipLeap qLeap .
   SigS.map (Interpolation.func ipStep qStep . SigG.toState)



data Prototype sig a v =
   Prototype {
      protoMarginLeap,
      protoMarginStep  :: Interpolation.Margin,
      protoIpOffset    :: Int,
      protoPeriod      :: a,
      protoPeriodInt   :: Int,
      protoShapeLimits :: (a,a),
      protoSignal      :: sig v
   }


makePrototype ::
   (RealField.C a, SigG.Read sig v) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   a -> sig v -> Prototype sig a v
makePrototype marginLeap marginStep period tone =
   let periodInt = round period
       ipOffset =
          ToneMod.interpolationOffset marginLeap marginStep periodInt
       len = SigG.length tone
       (lower,upper) =
          ToneMod.shapeLimits marginLeap marginStep periodInt len
       limits =
          if lower > upper
            then error "min>max"
            else (fromIntegral lower, fromIntegral upper)

   in  Prototype {
          protoMarginLeap  = marginLeap,
          protoMarginStep  = marginStep,
          protoIpOffset    = ipOffset,
          protoPeriod      = period,
          protoPeriodInt   = periodInt,
          protoShapeLimits = limits,
          protoSignal      = tone
       }

{-# INLINE sampledToneCell #-}
sampledToneCell ::
   (RealField.C a, SigG.Transform sig v) =>
   Prototype sig a v -> a -> Phase.T a -> ((a,a), Cell sig v)
sampledToneCell p shape phase =
   let (n, q) =
          ToneMod.flattenShapePhase (protoPeriodInt p) (protoPeriod p)
             (limit (protoShapeLimits p) shape, phase)
   in  (q,
        SigS.iterate (SigG.drop (protoPeriodInt p)) $
        SigG.drop (n - protoIpOffset p) $
        protoSignal p)


-- * lazy oscillator

{-# DEPRECATED oscillatorCells "This function recomputes the shape and phase signals. Better use Causal.ToneModulation.oscillatorCells" #-}
{- |
This function should not be used,
since it requires recomputation of @shapes@ and @freqs@ lists.
-}
oscillatorCells :: (RealField.C t, SigG.Transform sig y) =>
    Interpolation.Margin ->
    Interpolation.Margin ->
    t -> sig y -> (t, SigS.T t) -> (Phase.T t, SigS.T t) ->
    SigS.T ((t,t), Cell sig y)
oscillatorCells
       marginLeap marginStep period sampledTone shapes freqs =
    let periodInt = round period
        margin =
           ToneMod.interpolationNumber marginLeap marginStep periodInt
        ipOffset =
           ToneMod.interpolationOffset marginLeap marginStep periodInt
        (skips,coords) =
           -- unzip requires recomputation
           SigS.unzip $
           oscillatorCoords periodInt period
              (limitRelativeShapes marginLeap marginStep periodInt shapes)
              freqs
    in  SigS.zipWith
           {-
           n will be zero within the data body.
           It's only needed for extrapolation at the end.
           Is it really needed?
           -}
           (\(k,q) (_n,ptr) ->
               (q, makeCell periodInt $
                      SigG.drop (checkNonNeg $ periodInt+k) ptr))
           coords $
        SigS.switchL (error "list of pointers must not be empty") (flip const) $
        SigS.scanL
           (\ (n,ptr) d -> SigG.dropMarginRem margin (n+d) ptr)
           (0, sampledTone)
           (SigS.switchL skips
               (\s -> SigS.cons (s - (ipOffset + periodInt)))
               skips)
{-
*Synthesizer.Generic.ToneModulation> let shapes = [0.3,0.4,0.2::Double]; phases = [0.43,0.72,0.91::Double]
*Synthesizer.Generic.ToneModulation> let marginLeap = Interpolation.Margin 1 3; marginStep = Interpolation.Margin 2 2
*Synthesizer.Generic.ToneModulation> List.map (Data.Tuple.HT.mapSnd List.transpose) $ ToneMod.oscillatorCells marginLeap marginStep 9 ['a'..'z'] (2.3,shapes) (Phase.fromRepresentative 0.6, phases)
[((0.28888888888888875,0.40000000000000124),["ghijklmnopqrstuvwxyz","pqrstuvwxyz","yz"]),((0.8588888888888888,0.27000000000000046),["bcdefghijklmnopqrstuvwxyz","klmnopqrstuvwxyz","tuvwxyz"]),((0.13888888888888884,0.7500000000000004),["hijklmnopqrstuvwxyz","qrstuvwxyz","z"]),((0.2288888888888887,0.9400000000000017),["ghijklmnopqrstuvwxyz","pqrstuvwxyz","yz"])]
*Synthesizer.Generic.ToneModulation> oscillatorCells marginLeap marginStep 9 ['a'..'z'] (2.3, SigS.fromList shapes) (Phase.fromRepresentative 0.6, SigS.fromList phases)
StateSignal.fromList [((0.4,0.3999999999999999),StateSignal.fromList ["fghijklmnopqrstuvwxyz","opqrstuvwxyz","xyz"]),((0.97,0.2699999999999996),StateSignal.fromList ["abcdefghijklmnopqrstuvwxyz","jklmnopqrstuvwxyz","stuvwxyz"]),((0.25,0.75),StateSignal.fromList ["ghijklmnopqrstuvwxyz","pqrstuvwxyz","yz"])]

They do only match when input list is large enough
-}

checkNonNeg :: (Ord a, Additive.C a, Show a) => a -> a
checkNonNeg x =
   if x<zero
     then error ("unexpected negative number: " ++ show x)
     else x

makeCell :: (SigG.Transform sig y) => Int -> sig y -> Cell sig y
makeCell periodInt =
   SigS.takeWhile (not . SigG.null) .
   SigS.iterate (SigG.drop periodInt)


oscillatorCoords :: (RealField.C t) =>
    Int -> t ->
    (t, SigS.T t) -> (Phase.T t, SigS.T t) ->
    SigS.T (ToneMod.Coords t)
oscillatorCoords periodInt period
       (shape0, shapes) (phase, freqs) =
    let shapeOffsets =
           SigS.scanL
              (\(_,s) c -> splitFraction (s+c))
              (splitFraction shape0) shapes
        phases =
           -- FIXME: could be made without the dangerous irrefutable pattern
           let Just (s,ss) =
                  SigS.viewL $
                  SigS.map (\(n,_) -> fromIntegral n / period) $
                  shapeOffsets
           in  Osci.freqMod
                  (Phase.decrement s phase)  -- phase - s
               `Causal.apply`
                  (SigS.zipWith (-) freqs ss)
    in  SigS.zipWith
           (\(d,s) p -> (d, ToneMod.flattenShapePhase periodInt period (s,p)))
           shapeOffsets
           phases

limitRelativeShapes :: (RealField.C t) =>
    Interpolation.Margin ->
    Interpolation.Margin ->
    Int -> (t, SigS.T t) -> (t, SigS.T t)
limitRelativeShapes marginLeap marginStep periodInt =
    limitMinRelativeValues $ fromIntegral $
    ToneMod.interpolationOffset marginLeap marginStep periodInt + periodInt

limitMinRelativeValues :: (Additive.C t, Ord t) =>
   t -> (t, SigS.T t) -> (t, SigS.T t)
limitMinRelativeValues xMin (x0, xs) =
   let x1 = xMin-x0
   in  if x1<=zero
         then (x0, xs)
         else (xMin,
               SigS.crochetL
                  (\x lim ->
                     let d = x-lim
                     in  Just $ if d>=zero
                           then (d,zero) else (zero, negate d)) x1 xs)
{-
Test.QuickCheck.test (\x (y,zi) -> let z=List.map abs zi in  Data.Tuple.HT.mapSnd SigS.toList (limitMinRelativeValues x (y, SigS.fromList z)) == ToneMod.limitMinRelativeValues (x::Int) y z)
-}
