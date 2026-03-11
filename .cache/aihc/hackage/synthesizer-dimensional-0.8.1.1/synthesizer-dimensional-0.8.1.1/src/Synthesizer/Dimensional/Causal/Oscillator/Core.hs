{- |
Turn frequency information into signals of phases.
This is mainly the fundament for implementation of oscillators
but you may also use it for generating coherent waves of different form.
-}
module Synthesizer.Dimensional.Causal.Oscillator.Core where

import qualified Synthesizer.Causal.Oscillator.Core as Osci
import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import Synthesizer.Causal.Filter.NonRecursive (amplify, )
import Control.Arrow ((<<<), second, )

import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Basic.Phase  as Phase
import Synthesizer.Dimensional.Wave (SamplePhase, )

import qualified Synthesizer.State.Signal as Sig

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (toFrequencyScalar, )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
-- import Number.DimensionTerm ((&*&))

import qualified Algebra.RealField          as RealField
-- import qualified Algebra.Field              as Field
-- import qualified Algebra.Ring               as Ring

-- import NumericPrelude.Numeric
-- import NumericPrelude.Base as P


type Frequency u t = Amp.Numeric (DN.T (Dim.Recip u) t)
type SampleFrequency u t = Sample.T (Frequency u t) t


{-# INLINE static #-}
static ::
   (RealField.C t, Dim.C u) =>
      Phase.T t    {- ^ start phase -}
   -> DN.T (Dim.Recip u) t
                   {- ^ frequency -}
   -> Proc.T s u t
         (SigA.T (Rate.Phantom s) Amp.Abstract (Sig.T (Phase.T t)))
--         (Signal s Amp.Abstract (Phase.T t))
static phase freq =
   flip fmap (toFrequencyScalar freq) $ \f ->
   SigA.Cons Rate.Phantom Amp.Abstract $
   Osci.static phase f

{-# INLINE phaseMod #-}
phaseMod :: (RealField.C t, Dim.C u) =>
      DN.T (Dim.Recip u) t    {- ^ frequency -}
   -> Proc.T s u t
         (CausalD.T s (Sample.Flat t) (SamplePhase t))
phaseMod freq =
   flip fmap (toFrequencyScalar freq) $ \f ->
   CausalD.consFlip $ \Amp.Flat ->
      (Amp.Abstract, Osci.phaseMod f)

{-# INLINE freqMod #-}
freqMod :: (RealField.C t, Dim.C u) =>
      Phase.T t    {- ^ phase -}
   -> Proc.T s u t
         (CausalD.T s (SampleFrequency u t) (SamplePhase t))
freqMod phase =
   flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
   CausalD.consFlip $ \(Amp.Numeric freqAmp) ->
      (Amp.Abstract,
       Osci.freqMod phase <<< amplify (toFreq freqAmp))

{-# INLINE phaseFreqMod #-}
phaseFreqMod :: (RealField.C t, Dim.C u) =>
   Proc.T s u t
      (CausalD.T s (Sample.Flat t, SampleFrequency u t) (SamplePhase t))
phaseFreqMod =
   flip fmap (Proc.withParam toFrequencyScalar) $ \toFreq ->
   CausalD.consFlip $ \(Amp.Flat, Amp.Numeric freqAmp) ->
      (Amp.Abstract,
       Osci.phaseFreqMod <<< second (amplify (toFreq freqAmp)))
