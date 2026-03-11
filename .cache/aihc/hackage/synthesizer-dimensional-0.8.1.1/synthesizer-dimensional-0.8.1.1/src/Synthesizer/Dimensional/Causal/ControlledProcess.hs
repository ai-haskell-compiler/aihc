{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes (Flat)


Basic definitions for causal signal processors
that are controlled by another signal.
E.g. a Moog lowpass filter is controlled
by the cut-off frequency and the resonance.
However internally the Moog filter uses some feed-back factors.
The translation from cut-off frequency and resonance
(we call them external parameters)
to the feed-back factors
(we call them internal parameters)
depends on the sampling rate.
The problem we want to tackle is,
that computation of internal filter parameters
is expensive, but application of filters is not.
Thus we wish to compute internal filter parameters at a lower rate
than the sampling rate of the input and output
(refered to as audio rate, here).

Other digital sound synthesis systems solve it this way:

* Csound, SuperCollider:
  They distinguish between audio rate (say 44100 Hz),
  control rate (say 4410 Hz)
  and note rate (irregular, but usually less then 100 Hz).
  The control rate is globally equal and constant.

* ChucK: It updates internal filter parameters when external filter parameters change,
  that is, it updates by demand.
  In terms of control rates this means,
  that multiple control rates exist and they can be irregular.

After playing around with several approaches in this library,
the following one appeals me most:
We reveal the existence of internal filter parameters to the user,
but we hide the details of that parameters.
For every filter, we provide two functions:
One that computes internal filter parameters from external ones
and one for actual filtering of the audio data.
We provide a type class that selects a filter
according to the type of the internal filter parameters.
That is, the user only has to choose a filter parameter computation,
as found in "Synthesizer.Dimensional.Causal.FilterParameter".
For globally constant filter parameters,
such as the filter order, we use the signal amplitude.
You might call this abuse, but in future we may revise the notion
of amplitude to that of a global signal parameter.

Additionally we provide functions that perform the full filtering process
given only the filter parameter generator. There are two modes:

* Synchronous:
  The filter parameters are computed at audio rate.

* Asynchronous:
  The filter parameters are computed at a rate that can differ from audio rate.
  You can choose the control rate individually for every filter application.


This approach has several advantages:

* A filter only has to treat inputs of the same sampling rate.
  We do not have to duplicate the code for coping with input
  at rates different from the sample rate.

* We can provide different ways of specifying filter parameters,
  e.g. the resonance of a lowpass filter can be controlled
  either by the slope or by the amplification of the resonant frequency.

* We can use different control rates in the same program.

* We can even adapt the speed of filter parameter generation
  to the speed of changes in the control signal.

* For a sinusoidal controlled filter sweep we can setup a table
  of filter parameters for logarithmically equally spaced cut-off frequencies
  and traverse this table at varying rates according to arcus sine.

* Classical handling of control rate filter parameter computation
  can be considered as resampling of filter parameters with constant interpolation.
  If there is only a small number of internal filter parameters
  then we may resample with linear interpolation of the filter parameters.
-}
module Synthesizer.Dimensional.Causal.ControlledProcess (
   C(process),
   RateDep(RateDep, unRateDep),

   runSynchronous1,
   runSynchronous2,

   runAsynchronous1,
   runAsynchronousBuffered1,
   processAsynchronous1,

   runAsynchronous2,
   processAsynchronous2,
   processAsynchronousBuffered2,
   ) where

import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Causal.Process as CausalD
import qualified Synthesizer.Dimensional.Arrow as ArrowD
import qualified Synthesizer.Dimensional.Map as MapD
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Causal.Process       as Causal
import qualified Synthesizer.Causal.Interpolation as Interpolation
import qualified Synthesizer.Interpolation.Class as Interpol
import qualified Synthesizer.State.Signal as Sig
import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.RealField      as RealField

import Control.Applicative (liftA2, )

import Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable(..))

import NumericPrelude.Numeric
-- import NumericPrelude.Base as P


{- |
Select a filter process according to the filter parameter type.
-}
{-
Constraint @(Amp.Primitive global)@ makes no sense,
since many global parameters actually contain non-constant data.
E.g. two signals of Moog lowpass parameters can only be appended
if the filter of both signals matches.
That is, Moog lowpass' global parameter is not primitive.
-}
class
   Amp.C global =>
      C global parameter a b |
         global parameter a -> b,
         global parameter b -> a where
   process ::
      (Dim.C u) =>
      Proc.T s u t
         (CausalD.T s
            (Sample.T global (RateDep s parameter), a) b)


{- |
This type tags an internal filter parameter
with the sampling rate for which it was generated.
Be aware, that in asynchronous application
the internal filter parameters are computed at control rate,
but the internal filter parameters must correspond
to the sampling rate of the target audio signal.
The type parameter @s@ corresponds to that target audio rate.
-}
newtype RateDep s ic = RateDep {unRateDep :: ic}


instance Interpol.C a ic => Interpol.C a (RateDep s ic) where
   scaleAndAccumulate =
      Interpol.makeMac RateDep unRateDep

instance Storable ic => Storable (RateDep s ic) where
   sizeOf = Store.sizeOf unRateDep
   alignment = Store.alignment unRateDep
   peek = Store.peek RateDep
   poke = Store.poke unRateDep

type Signal s ecAmp ec =
   SigA.T (Rate.Phantom s) ecAmp (Sig.T ec)


{-# INLINE runSynchronous1 #-}
runSynchronous1 ::
   (C global parameter sampleIn sampleOut, Dim.C u,
    Amp.C ecAmp) =>
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp ec)
         (Sample.T global (RateDep s parameter))) ->
   Proc.T s u t
      (Signal s ecAmp ec ->
       CausalD.T s sampleIn sampleOut)
runSynchronous1 =
   liftA2
      (\proc causal ->
         CausalD.applyFst proc .
         ArrowD.apply causal)
      process


{-# INLINE runSynchronous2 #-}
runSynchronous2 ::
   (C global parameter sampleIn sampleOut, Dim.C u,
    Amp.C ecAmp0, Amp.C ecAmp1) =>
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp0 ec0, Sample.T ecAmp1 ec1)
         (Sample.T global (RateDep s parameter))) ->
   Proc.T s u t
      (Signal s ecAmp0 ec0 ->
       Signal s ecAmp1 ec1 ->
       CausalD.T s sampleIn sampleOut)
runSynchronous2 causalp =
   liftA2
      (\proc causal x y ->
         CausalD.applyFst proc $
         ArrowD.apply causal $
         SigA.zip x y)
      process causalp



resample ::
   (Amp.C amp, Dim.C u, RealField.C t) =>
   Interpolation.T t y ->
   SigA.T (Rate.Dimensional u t) amp (Sig.T y) ->
   Proc.T s u t
      (SigA.T (Rate.Phantom s) amp (Sig.T y))
resample ip sig =
   fmap
      (\k ->
         SigA.Cons
            Rate.Phantom
            (SigA.amplitude sig)
            (Causal.applyConst
               (Interpolation.relativeConstantPad ip zero (SigA.body sig)) k))
      (Proc.toFrequencyScalar (SigA.actualSampleRate sig))


{-# INLINE zipRate #-}
zipRate ::
   (Amp.C amp0, Amp.C amp1, Eq t) =>
   SigA.T (Rate.Dimensional u t) amp0 (Sig.T y0) ->
   SigA.T (Rate.Dimensional u t) amp1 (Sig.T y1) ->
   SigA.T (Rate.Dimensional u t) (amp0,amp1) (Sig.T (y0,y1))
zipRate x y =
   SigA.Cons
      (Rate.Actual $
       Rate.common "ControlledProcess.zipRate"
         (SigA.actualSampleRate x) (SigA.actualSampleRate y))
      (SigA.amplitude x, SigA.amplitude y)
      (Sig.zip (SigA.body x) (SigA.body y))


{-# INLINE runAsynchronous #-}
runAsynchronous ::
   (C global ic sampleIn sampleOut,
    Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   SigA.T (Rate.Dimensional u t) global (Sig.T (RateDep s ic)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
runAsynchronous ip sig =
   liftA2 CausalD.applyFst process (resample ip sig)

{-# INLINE runAsynchronousBuffered #-}
runAsynchronousBuffered ::
   (C global ic sampleIn sampleOut,
    Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   SigA.T (Rate.Dimensional u t) global (Sig.T (RateDep s ic)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
runAsynchronousBuffered ip =
   runAsynchronous ip .
   SigA.processBody (Sig.fromList . Sig.toList)


{-# INLINE runAsynchronous1 #-}
runAsynchronous1 ::
   (C global ic sampleIn sampleOut,
    Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
--      (ArrowD.T arrow
         (Sample.T ecAmp ec)
         (Sample.T global (RateDep s ic))) ->
   SigA.T (Rate.Dimensional u t) ecAmp (Sig.T ec) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
runAsynchronous1 ip cp sig =
   cp >>= \p -> runAsynchronous ip (ArrowD.apply p sig)

{-# INLINE runAsynchronousBuffered1 #-}
runAsynchronousBuffered1 ::
   (C global ic sampleIn sampleOut,
    Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
--      (ArrowD.T arrow
         (Sample.T ecAmp ec)
         (Sample.T global (RateDep s ic))) ->
   SigA.T (Rate.Dimensional u t) ecAmp (Sig.T ec) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
runAsynchronousBuffered1 ip cp sig =
   cp >>= \p -> runAsynchronousBuffered ip (ArrowD.apply p sig)

{-# INLINE processAsynchronous1 #-}
processAsynchronous1 ::
   (-- ArrowD.Applicable arrow (Rate.Phantom s1),
    C global ic sampleIn sampleOut,
    Amp.C ecAmp, Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
--      (ArrowD.T arrow
         (Sample.T ecAmp ec)
         (Sample.T global (RateDep s ic))) ->
   DN.T (Dim.Recip u) t ->
   (forall r. Proc.T r u t (Signal r ecAmp ec)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
processAsynchronous1 ip cp rate sig =
   cp >>= \p -> runAsynchronous ip (SigA.render rate (fmap (ArrowD.apply p) sig))


{- |
Using two @SigP.T@'s as input has the disadvantage
that their rates must be compared dynamically.
It is not possible with our data structures
to use one rate for multiple signals.
We could also allow the input of a Rate.T and two Proc.T's,
since this is the form we get from the computation routines.
But this way we lose sharing.
-}
{-# INLINE runAsynchronous2 #-}
runAsynchronous2 ::
   (C global ic sampleIn sampleOut,
    Amp.C ecAmp0, Amp.C ecAmp1, Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp0 ec0, Sample.T ecAmp1 ec1)
         (Sample.T global (RateDep s ic))) ->
   SigA.T (Rate.Dimensional u t) (ecAmp0) (Sig.T ec0) ->
   SigA.T (Rate.Dimensional u t) (ecAmp1) (Sig.T ec1) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
runAsynchronous2 ip cp x y =
   cp >>= \p ->
      runAsynchronous ip $ ArrowD.apply p $ zipRate x y



{- |
This function will be more commonly used than 'runAsynchronous2',
but it disallows sharing of control signals.
It can be easily defined in terms of 'runAsynchronous2' and 'SigA.render',
but the implementation here does not need the check for equal sample rates.
-}
{-# INLINE processAsynchronous2 #-}
processAsynchronous2 ::
   (C global ic sampleIn sampleOut,
    Amp.C ecAmp0, Amp.C ecAmp1, Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp0 ec0, Sample.T ecAmp1 ec1)
         (Sample.T global (RateDep s ic))) ->
   DN.T (Dim.Recip u) t ->
   (forall r. Proc.T r u t (Signal r ecAmp0 ec0)) ->
   (forall r. Proc.T r u t (Signal r ecAmp1 ec1)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
processAsynchronous2 ip cp rate x y =
   cp >>= \p ->
      runAsynchronous ip
         (SigA.render rate (fmap (ArrowD.apply p) $ liftA2 SigA.zip x y))


{-# INLINE _processAsynchronousNaive2 #-}
_processAsynchronousNaive2 ::
   (C global ic sampleIn sampleOut,
    Amp.C ecAmp0, Amp.C ecAmp1, Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp0 ec0, Sample.T ecAmp1 ec1)
         (Sample.T global (RateDep s ic))) ->
   DN.T (Dim.Recip u) t ->
   (forall r. Proc.T r u t (Signal r ecAmp0 ec0)) ->
   (forall r. Proc.T r u t (Signal r ecAmp1 ec1)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
_processAsynchronousNaive2 ip cp rate x y =
   runAsynchronous2 ip cp
      (SigA.render rate x) (SigA.render rate y)


{-
This uses lazy StorableVector for buffering
of the internal control parameters.
This increases laziness granularity,
but it should be faster, since interpolation needs frequent look-ahead,
and this is faster on a Storable signal than on a plain stateful signal generator.
Since the look-ahead is constant,
it is interesting whether interpolation can be made more efficient
without Storable.

{-# INLINE processAsynchronousStorable2 #-}
processAsynchronousStorable2 ::
   (Dim.C u, Amp.C ecAmp0, Amp.C ecAmp1, Storable ic, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
         (Sample.T ecAmp ec)
         (Sample.T global (RateDep s ic))) ->
   DN.T (Dim.Recip u) t ->
   (forall r. Proc.T r u t (Signal r ecAmp0 ec0)) ->
   (forall r. Proc.T r u t (Signal r ecAmp1 ec1)) ->
   Proc.T s u t
      (CausalD.T s sampleIn sampleOut)
processAsynchronousStorable2 ip cp rate x y =
   let sigX = SigA.render rate x
       sigY = SigA.render rate y
   in  cp >>= \p ->
          runAsynchronous ip cp
             (applyConverter2 const (converter p) sigX sigY)
-}

{- |
This buffers internal control parameters before interpolation.
This should be faster, since interpolation needs frequent look-ahead,
and this is faster on a buffered signal than on a plain stateful signal generator.

Since the look-ahead is constant,
it is interesting whether interpolation can be made more efficient
without the inefficient intermediate list structure.
-}
{-# INLINE processAsynchronousBuffered2 #-}
processAsynchronousBuffered2 ::
   (-- ArrowD.Applicable arrow (Rate.Phantom s1),
    C global ic sampleIn sampleOut,
    Amp.C ecAmp0, Amp.C ecAmp1, Dim.C u, RealField.C t) =>
   Interpolation.T t (RateDep s ic) ->
   Proc.T s u t
      (MapD.T
--      (ArrowD.T arrow
         (Sample.T ecAmp0 ec0, Sample.T ecAmp1 ec1)
         (Sample.T global (RateDep s ic))) ->
   DN.T (Dim.Recip u) t ->
   (forall r. Proc.T r u t (Signal r ecAmp0 ec0)) ->
   (forall r. Proc.T r u t (Signal r ecAmp1 ec1)) ->
   Proc.T s u t (CausalD.T s sampleIn sampleOut)
processAsynchronousBuffered2 ip cp rate x y =
   cp >>= \p ->
      runAsynchronousBuffered ip
         (SigA.render rate (fmap (ArrowD.apply p) $ liftA2 SigA.zip x y))
