{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Basics for building tone generators.
They generate signals of phases
and these signals can be converted to arbitrary waveforms
by mapping them via @Wave@ objects.
This is also the fundament for dimensional oscillators.
-}
module Synthesizer.Causal.Oscillator.Core where

import qualified Synthesizer.Basic.Phase as Phase

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.State.Signal as Sig

import Control.Arrow ((^<<), (&&&), second, returnA, )

import qualified Algebra.RealRing             as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base



{-# INLINE static #-}
static :: RealRing.C a =>
   Phase.T a -> a -> Sig.T (Phase.T a)
static phase freq =
   Sig.iterate (Phase.increment freq) phase


{-# INLINE phaseMod #-}
{- | oscillator with modulated phase -}
phaseMod :: (RealRing.C a) =>
   a -> Causal.T a (Phase.T a)
phaseMod freq =
   uncurry Phase.increment ^<<
      Causal.feedSnd (static zero freq)

{-# INLINE shapeMod #-}
{- | oscillator with modulated shape -}
shapeMod :: (RealRing.C a) =>
   Phase.T a -> a -> Causal.T c (c, Phase.T a)
shapeMod phase freq =
   Causal.feedSnd (static phase freq)

{-# INLINE freqMod #-}
{- |
Convert a list of phase steps into a list of momentum phases.
phase is a number in the interval [0,1).
freq contains the phase steps.
The last element is omitted.
-}
freqMod :: RealRing.C a =>
   Phase.T a -> Causal.T a (Phase.T a)
freqMod =
   Causal.scanL (flip Phase.increment)

{- |
Like 'freqMod' but the first element is omitted.
-}
{-# INLINE freqModSync #-}
freqModSync :: RealRing.C a =>
   Phase.T a -> Causal.T a (Phase.T a)
freqModSync =
   Causal.crochetL
      (\f p0 -> let p1 = Phase.increment f p0 in Just (p1,p1))


{-# INLINE freqModAntiAlias #-}
{- | oscillator with modulated frequency -}
freqModAntiAlias :: (RealRing.C a) =>
   Phase.T a -> Causal.T a (a, Phase.T a)
freqModAntiAlias phase =
   returnA &&& freqMod phase

{-# INLINE phaseFreqMod #-}
{- | oscillator with both phase and frequency modulation -}
phaseFreqMod :: (RealRing.C a) =>
   Causal.T (a,a) (Phase.T a)
phaseFreqMod =
   uncurry Phase.increment ^<<
   second (freqMod zero)

{-# INLINE shapeFreqMod #-}
{- | oscillator with both shape and frequency modulation -}
shapeFreqMod :: (RealRing.C a) =>
   Phase.T a -> Causal.T (c,a) (c, Phase.T a)
shapeFreqMod phase =
   second (freqMod phase)
