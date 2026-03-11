module Synthesizer.Generic.Wave where

import qualified Synthesizer.State.ToneModulation as ToneMod
import qualified Synthesizer.Basic.Wave as Wave

import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Interpolation as Interpolation

import qualified Algebra.RealField            as RealField
import qualified Algebra.RealRing             as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


sample ::
   (RealRing.C a, SigG.Transform sig v) =>
   Interpolation.T a v -> sig v -> Wave.T a v
sample ip wave =
   let len = SigG.length wave
       cycleWave = SigG.cycle wave
   in  Wave.fromFunction $ \ phase ->
           let (n,q) = RealRing.splitFraction (phase * fromIntegral len)
           in  Interpolation.func ip q $
               SigG.toState $
               SigG.drop n cycleWave


{- |
Interpolate first within waves and then across waves,
which is simpler but maybe less efficient for lists.
However for types with fast indexing/drop like StorableVector this is optimal.
-}
sampledTone ::
   (RealField.C a, SigG.Transform sig v) =>
   Interpolation.T a v ->
   Interpolation.T a v ->
   a -> sig v -> a -> Wave.T a v
sampledTone ipLeap ipStep period tone shape = Wave.Cons $ \phase ->
--   uncurry (ToneMod.interpolateCell ipStep ipLeap . swap) $
   uncurry (ToneMod.interpolateCell ipLeap ipStep) $
   ToneMod.sampledToneCell
      (ToneMod.makePrototype
          (Interpolation.margin ipLeap) (Interpolation.margin ipStep)
          period tone)
      shape phase

