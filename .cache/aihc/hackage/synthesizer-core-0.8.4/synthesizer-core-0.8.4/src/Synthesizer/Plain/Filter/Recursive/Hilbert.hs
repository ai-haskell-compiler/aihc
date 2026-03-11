{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Two allpasses that approach a relative phase difference of 90 degree
over a large range of frequencies.

ToDo:
More parameters for controling the affected frequency range.
-}
module Synthesizer.Plain.Filter.Recursive.Hilbert (
   Parameter (Parameter, parameterCosine, parameterSine),
   polesCosine, polesSine,
   parameter,
   step2,
   modifierInit2,
   runInit2, run2,
   cascade,
   causal2, causalComplex2,
   causal,  causalComplex,

   lowpassStream,
   lowpassMaintainPhase,
   ) where

import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Basic.ComplexModule as CM

{-
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Graphics.Gnuplot.Simple as Gnuplot
-}

import Control.Arrow ((<<<), (>>>), (&&&), (>>^), )
import Control.Monad.Trans.State (State, state, runState, )

import qualified Data.List.Match as Match

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring

import qualified Number.Complex as Complex
import Number.Complex ((+:), )

import NumericPrelude.Numeric
import NumericPrelude.Base



data Parameter a =
     Parameter {parameterCosine, parameterSine :: [Allpass.Parameter a]}
   deriving Show


-- taken from Bernie Hutchins, "Musical Engineer's Handbook"
polesCosine, polesSine :: Field.C a => [a]
polesCosine = [1.2524, 5.5671, 22.3423, 89.6271, 364.7914, 2770.1114]
polesSine   = [0.3609, 2.7412, 11.1573, 44.7581, 179.6242, 798.4578]


{-| Convert sample rate to allpass parameters. -}
{-# INLINE parameter #-}
parameter :: Trans.C a => a -> Parameter a
parameter rate =
   Parameter
      (map (Allpass.parameterApprox (-1/(15*pi)) . (/rate)) polesCosine)
      (map (Allpass.parameterApprox (-1/(15*pi)) . (/rate)) polesSine)


{-# INLINE step2 #-}
step2 :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> State [Complex.T v] (Complex.T v)
step2 param x = state $ \s ->
   let mr = Allpass.cascadeDiverseStep (parameterCosine param) x
       mi = Allpass.cascadeDiverseStep (parameterSine   param) x
       (r,sr) = runState mr (map Complex.real s)
       (i,si) = runState mi (map Complex.imag s)
   in  (r+:i, zipWith (+:) sr si)

{-# INLINE modifierInit2 #-}
modifierInit2 :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized [Complex.T v] [Complex.T v] (Parameter a) v (Complex.T v)
modifierInit2 =
   Modifier.Initialized id step2

{-
It would need an order parameter which is ugly.

{-# INLINE modifier2 #-}
modifier2 :: (Ring.C a, Module.C a v) =>
   Int -> Modifier.Simple [Complex.T v] (Parameter a) v (Complex.T v)
modifier2 order =
   Sig.modifierInitialize modifierInit2 (replicate (succ order) zero)
-}


cascade ::
   (Ring.C a, Module.C a v) =>
   [Allpass.Parameter a] -> Causal.T v v
cascade ps =
   foldl1 (>>>)
      (map (\p -> Allpass.firstOrderCausal <<< Causal.feedConstFst p) ps)

{- |
Although we get (almost) only the right-rotating part of the real input signal,
the amplitude is as large as the input amplitude.
That is, the amplitude actually doubled.
-}
{-# INLINE causal2 #-}
causal2 ::
   (Ring.C a, Module.C a v) =>
   Parameter a -> Causal.T v (Complex.T v)
causal2 param =
   (cascade (parameterCosine param) &&&
    cascade (parameterSine param))
       >>^ uncurry (+:)

{-# INLINE causalComplex2 #-}
causalComplex2 ::
   (Ring.C a, Module.C a v) =>
   Parameter a -> Causal.T (Complex.T v) (Complex.T v)
causalComplex2 param =
   (cascade (parameterCosine param) &&&
    cascade (parameterSine param))
       >>^ (\(c,s) -> c + Complex.quarterLeft s)


{-# INLINE scaleWithParamType #-}
scaleWithParamType ::
   (Module.C a v) =>
   Parameter a -> a -> v -> v
scaleWithParamType _ k v =
   k *> v


{-# INLINE causal #-}
causal ::
   (Field.C a, Module.C a v) =>
   Parameter a -> Causal.T v (Complex.T v)
causal param =
   causal2 param >>^ scaleWithParamType param 0.5

{-# INLINE causalComplex #-}
causalComplex ::
   (Field.C a, Module.C a v) =>
   Parameter a -> Causal.T (Complex.T v) (Complex.T v)
causalComplex param =
   causalComplex2 param >>^ scaleWithParamType param 0.5


{-# INLINE runInit2 #-}
runInit2 :: (Ring.C a, Module.C a v) =>
   [Complex.T v] -> Parameter a -> Sig.T v -> Sig.T (Complex.T v)
runInit2 =
   Sig.modifyStaticInit modifierInit2

{-# INLINE run2 #-}
run2 :: (Ring.C a, Module.C a v) =>
   Parameter a -> Sig.T v -> Sig.T (Complex.T v)
run2 param =
   runInit2 (Match.replicate (undefined : parameterCosine param) zero) param


{- |
Approximation to perfect lowpass.
However in the low frequencies the above filter
is far away from being a perfect Hilbert filter,
thus the cut is not straight as expected.
This implementation is lazy, but shifts phases.
-}
lowpassStream ::
   (Trans.C a, RealField.C a, Module.C a v) =>
   a -> a -> Sig.T v -> Sig.T v
lowpassStream freq cutoff =
   let clearLeft = Causal.apply (causalComplex (parameter freq))
   in  zipWith CM.project
          (Osci.static Wave.helix zero (cutoff/freq)) .
       map Complex.conjugate .
       clearLeft .
       map Complex.conjugate .
       zipWith CM.mul
          (Osci.static Wave.helix zero (-2*cutoff/freq)) .
       clearLeft .
       zipWith CM.scale
          (Osci.static Wave.helix zero (cutoff/freq))


{- |
If we could achieve lowpass filtering while maintaining phases,
we could do approximate Whittaker interpolation.
Here we try to do this by filtering forward and backward.
However, this does not work since we move the spectrum
between two Hilbert transforms
and thus the phase distortions do not match.
It does not even yield a fine lowpass,
since reversing the signal does not reverse the spectrum.
-}
lowpassMaintainPhase ::
   (Trans.C a, RealField.C a, Module.C a v) =>
   a -> a -> Sig.T v -> Sig.T v
lowpassMaintainPhase freq cutoff =
   let clearLeft = Causal.apply (causalComplex (parameter freq))
   in  zipWith CM.project
          (Osci.static Wave.helix zero (cutoff/freq)) .
       reverse .
       clearLeft .
       reverse .
       zipWith CM.mul
          (Osci.static Wave.helix zero (-2*cutoff/freq)) .
       clearLeft .
       zipWith CM.scale
          (Osci.static Wave.helix zero (cutoff/freq))


{-
exampleHilbert :: IO ()
exampleHilbert =
   let -- xs = take 10000 $ Osci.staticSine 0 (0.001::Double)
       xs = Osci.freqModSine 0 $ Ctrl.line 10000 (0, 0.1::Double)
       ys = Causal.apply (causal2 (parameter (44100::Double))) xs
       zs = run2 (parameter (44100::Double)) xs
   in  Gnuplot.plotLists [] $
          xs :
          map Complex.real ys : map Complex.imag ys :
          map Complex.magnitude ys :
          map Complex.real zs : map Complex.imag zs :
          map Complex.magnitude zs :
          []


exampleLowpass :: IO ()
exampleLowpass =
   let xs = Osci.freqModSine 0 $ Ctrl.line 22050 (0, 0.05::Double)
       ys = lowpassStream (44100::Double) 882 xs
       zs = lowpassMaintainPhase (44100::Double) 882 xs
   in  Gnuplot.plotLists [] [xs, ys, zs]
-}
