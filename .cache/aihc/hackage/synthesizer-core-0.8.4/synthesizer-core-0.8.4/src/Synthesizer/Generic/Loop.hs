{-# LANGUAGE NoImplicitPrelude #-}
{- |
Several functions that add a loop to a sampled sound.
This way you can obtain an infinite sound
that consumes only finite space.
-}
module Synthesizer.Generic.Loop (
   simple,
   fade,

   timeReverse,
   TimeControl,
   timeControlSine,
   timeControlZigZag,
   ) where

import qualified Synthesizer.Generic.Signal    as SigG
import qualified Synthesizer.Generic.Cut       as CutG
import qualified Synthesizer.Generic.Wave      as WaveG

import qualified Synthesizer.Basic.Wave        as Wave
import qualified Synthesizer.Basic.Phase       as Phase

import qualified Synthesizer.State.Signal      as SigS
import qualified Synthesizer.State.Control     as CtrlS
import qualified Synthesizer.State.Oscillator  as OsciS

import qualified Synthesizer.Interpolation as Interpolation

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.RealRing       as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
Most simple of looping:
You give start and length of the loop body
and this part is repeated.
The data behind start+length is ignored.
-}
simple :: (CutG.Transform sig) => Int -> Int -> sig -> sig
simple len start xs =
   let (prefix, suffix) = CutG.splitAt start xs
       loopBody = CutG.take len suffix
   in  CutG.append prefix (CutG.cycle loopBody)

{- |
Create a smooth loop by cross-fading a part
with delayed versions of itself.
The loop length will be rounded to the next smaller even number.
-}
fade :: (SigG.Transform sig yv, Trans.C y, Module.C y yv) =>
   y -> Int -> Int -> sig yv -> sig yv
fade dummy loopLen2 start xs =
   let loopLen = div loopLen2 2
       (prefix, loopOut) = CutG.splitAt (start+loopLen) xs
       loopIn = CutG.drop start prefix
       loopBody =
          SigG.zipWithState3
             (\s x y ->
                let s2 = 0.5*s `asTypeOf` dummy
                in  (0.5-s2)*>x + (0.5+s2)*>y)
             (CtrlS.cosine 0 (fromIntegral loopLen))
             (SigG.toState loopIn)
             loopOut
   in  CutG.append prefix (CutG.cycle loopBody)

{- |
Resample a sampled sound with a smooth loop
using our time manipulation algorithm.
Time is first controlled linearly,
then switches to a sine or triangular control.
Loop start must be large enough in order provide enough spare data
for interpolation at the beginning
and loop start plus length must preserve according space at the end.
One period is enough space for linear interpolation.

In order to get a loopable sound with finite space
we have to reduce the loop length to a multiple of a wave period.
We will also modify the period a little bit,
such that in our loop body there is an integral number of periods.

We return the modified period and the looped sound.
-}
{-# INLINE timeReverse #-}
timeReverse ::
   (SigG.Write sig yv, RealField.C q, Module.C q yv) =>
   SigG.LazySize ->
   Interpolation.T q yv ->
   Interpolation.T q yv ->
   TimeControl q ->
   q -> q -> (q, sig yv) -> (q, sig yv)
timeReverse lazySize ipLeap ipStep
      timeCtrlWave loopLen loopStart (period0, sample) =
   let (period, timeCtrl) =
          timeControl timeCtrlWave period0 (loopLen/2)
       wave = WaveG.sampledTone ipLeap ipStep period sample
       loopCenter = round $ loopStart + loopLen/2
       loop =
          SigG.fromState lazySize $
          OsciS.shapeFreqMod wave
             (Phase.fromRepresentative $ fromIntegral loopCenter / period)
             (SigS.map (fromIntegral loopCenter +) timeCtrl)
             (SigS.repeat (recip period))
   in  (period,
        CutG.append
           (CutG.take loopCenter sample)
           (CutG.cycle loop))

timeControl ::
   (RealField.C a) =>
   TimeControl a ->
   a -> a -> (a, SigS.T a)
timeControl (TimeControl slope wave) period0 loopDepth0 =
   let numberOfWaves =
          fromIntegral $
          (floor(slope*loopDepth0/period0) :: Int)
       loopLenInt = floor (numberOfWaves * period0)
       loopLen = fromIntegral loopLenInt
       period = loopLen / numberOfWaves
       loopDepth = loopLen / slope
   in  (period,
        SigS.take loopLenInt $
        SigS.map (loopDepth *) $
        OsciS.static wave zero (recip loopLen))


data TimeControl a = TimeControl a (Wave.T a a)

timeControlSine :: (Trans.C a) => TimeControl a
timeControlSine = TimeControl (2*pi) Wave.sine

timeControlZigZag :: (RealRing.C a) => TimeControl a
timeControlZigZag = TimeControl 4 Wave.triangle
