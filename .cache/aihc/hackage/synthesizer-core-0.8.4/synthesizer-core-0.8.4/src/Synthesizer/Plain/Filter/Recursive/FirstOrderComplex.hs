{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

First order lowpass and highpass with complex valued feedback.
The complex feedback allows resonance.
It is often called complex resonator.
-}
module Synthesizer.Plain.Filter.Recursive.FirstOrderComplex (
   Parameter,
   parameter,
   parameterFromPeakWidth,
   parameterFromPeakToDCRatio,
   step,
   modifierInit,
   modifier,
   causal,
   runInit,
   run,
   ) where

import Synthesizer.Plain.Filter.Recursive (Pole(..))
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Synthesizer.Basic.ComplexModule as CM
import qualified Number.Complex as Complex

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.Ring                  as Ring

import Control.Monad.Trans.State (State, state, )

import NumericPrelude.Numeric
import NumericPrelude.Base


data Parameter a =
        Parameter {c, amp :: !(Complex.T a)}
   deriving Show


instance Interpol.C a v => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = Interpol.makeMac2 Parameter c amp



{-
y0 = u0 + k * cis w * y1

transfer function:
   1/(1 - k * cis w * z)

frequency 0 amplified by @recip (1 - k * cis w)@.
resonance frequency amplified by 1+k+k^2+..., which equals @recip (1-k)@.
resonance frequency + half sample rate is amplified by @recip (1+k)@.
-}


{-|
The internal parameters are computed such that:

* At the resonance frequency
  the filter amplifies by the factor @resonance@
  with no phase shift.

* At resonance frequency plus half sample rate
  the filter amplifies by facter @recip $ 2 - recip resonance@
  with no phase shift,
  but you cannot observe this immediately,
  because it is outside the Nyquist band.
-}
{-# INLINE parameter #-}
parameter :: Trans.C a => Pole a -> Parameter a
parameter (Pole resonance frequency) =
    let cisw   = Complex.cis (2*pi*frequency)
        k      = 1 - recip resonance
        kcisw  = Complex.scale k cisw
    in  Parameter kcisw one


{-
Let resonance be the ratio
of the resonance amplification
to the amplification at another frequency, encoded by z.

resonance = abs(1 - k * cis w * z) / (1 - k)

Solution: Substitute @cis w * z@ by @cis a@
and proceed as in parameterFromPeakToDCRatio.
-}
{-|
The internal parameters are computed such that:

* At the resonance frequency
  the filter amplifies by the factor @resonance@
  with no phase shift.

* At resonance frequency plus and minus band width
  the filter amplifies by facter 1 with a non-zero phase shift.
-}
{-# INLINE parameterFromPeakWidth #-}
parameterFromPeakWidth :: Trans.C a => a -> Pole a -> Parameter a
parameterFromPeakWidth width (Pole resonance frequency) =
    let cisw   = Complex.cis (2*pi*frequency)
        k      = solveRatio resonance (cos (2*pi*width))
        kcisw  = Complex.scale k cisw
        amp_   = Complex.fromReal ((1-k)*resonance)
    in  Parameter kcisw amp_

{-|
The internal parameters are computed such that:

* At the resonance frequency
  the filter amplifies by the factor @resonance@
  with a non-zero phase shift.

* The filter amplifies the direct current (frequency zero) by factor 1
  with no phase shift.

* The real component is a lowpass,
  the imaginary component is a highpass.
  You can interpolate between them using other complex projections.
-}
{-
If we want to interpret the resonance
as ratio of the peak height to direct current amplification,
we get:

resonance = abs ((1 - k * cis w) / (1-k))
resonance^2 * (1-k)^2
   = (1 - k * cis w) * (1 - k * cis (-w))
   = 1 + k^2 - 2*k*cos w
0 = 1-resonance^2 + 2 * (resonance^2 - cos w) * k + (1-resonance^2) * k^2
0 = 1 + 2 * (resonance^2 - cos w) / (1-resonance^2) * k + k^2
-}
{-# INLINE parameterFromPeakToDCRatio #-}
parameterFromPeakToDCRatio :: Trans.C a => Pole a -> Parameter a
parameterFromPeakToDCRatio (Pole resonance frequency) =
    let cisw   = Complex.cis (2*pi*frequency)
        k      = solveRatio resonance (Complex.real cisw)
        kcisw  = Complex.scale k cisw
        amp_   = one - kcisw
    in  Parameter kcisw amp_

solveRatio :: (Algebraic.C a) =>
    a -> a -> a
solveRatio resonance cosine =
    let r2 = resonance^2
        p  = (r2 - cosine) / (r2 - 1)
        {- no cancelation for p close to 1,
           that is, big resonance or cosine close to 1 -}
    in  recip $ p + sqrt (p^2 - 1)

{-
solveRatioAnalytic :: (Algebraic.C a) =>
    a -> a -> a
solveRatioAnalytic resonance cosine =
    let r2 = resonance^2
        p  = (r2 - cosine) / (r2 - 1)
    in  p - sqrt (p^2 - 1)
-}


{- |
We use complex numbers as result types,
since the particular filter type is determined by the parameter generator.
-}
type Result = Complex.T


{-| Universal filter: Computes high pass, band pass, low pass in one go -}
{-# INLINE step #-}
step :: (Module.C a v) =>
   Parameter a -> v -> State (Complex.T v) (Result v)
step p u =
   state $ \s ->
      let y = CM.scale (amp p) u + CM.mul (c p) s
      in  (y, y)
--      in (Result (Complex.imag y) (Complex.real y), y)


{-# INLINE modifierInit #-}
modifierInit :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized (Complex.T v) (Complex.T v) (Parameter a) v (Result v)
modifierInit =
   Modifier.Initialized id step

{-# INLINE modifier #-}
modifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple (Complex.T v) (Parameter a) v (Result v)
modifier = Sig.modifierInitialize modifierInit zero

{-# INLINE causal #-}
causal ::
   (Ring.C a, Module.C a v) =>
   Causal.T (Parameter a, v) (Result v)
causal =
   Causal.fromSimpleModifier modifier


{-# INLINE runInit #-}
runInit :: (Ring.C a, Module.C a v) =>
   Complex.T v -> Sig.T (Parameter a) -> Sig.T v -> Sig.T (Result v)
runInit = Sig.modifyModulatedInit modifierInit

{-# INLINE run #-}
run :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T (Result v)
run = runInit zero
