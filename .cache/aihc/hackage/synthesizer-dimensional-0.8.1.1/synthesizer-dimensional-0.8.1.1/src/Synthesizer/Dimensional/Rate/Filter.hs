{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Rate.Filter (
   {- * Non-recursive -}

   {- ** Amplification -}
   negate,
   envelope,
   envelopeVector,
   convolveVector,

   {- ** Smooth -}
   mean,
   meanStatic,

   {- ** Delay -}
   delay,
   phaseModulation,
   phaser,
   phaserStereo,
   frequencyModulation,
   frequencyModulationDecoupled,


   {- * Recursive -}

   {- ** Without resonance -}
   firstOrderLowpass,
   firstOrderHighpass,
   butterworthLowpass,
   butterworthHighpass,
   chebyshevALowpass,
   chebyshevAHighpass,
   chebyshevBLowpass,
   chebyshevBHighpass,
   {- ** With resonance -}
   universal,
   highpassFromUniversal,
   bandpassFromUniversal,
   lowpassFromUniversal,
   bandlimitFromUniversal,
   moogLowpass,

   {- ** Allpass -}
   allpassCascade,
   allpassFlangerPhase,

   {- ** Reverb -}
   comb,

   {- * Helper functions -}
   interpolateMultiRelativeZeroPad,
) where

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat

import qualified Synthesizer.Dimensional.Amplitude.Filter       as FiltV
import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import Synthesizer.Dimensional.Process
   (toTimeScalar, toFrequencyScalar, )

-- import Synthesizer.Dimensional.Process ((.:), (.^), )

import qualified Synthesizer.Dimensional.Signal.Private  as SigA
import qualified Synthesizer.State.Signal as Sig
import Synthesizer.Plain.Signal (Modifier, )

import qualified Synthesizer.Causal.Process       as Causal
import qualified Synthesizer.Causal.Interpolation as Interpolation
import qualified Synthesizer.State.Displacement as Disp
import qualified Synthesizer.State.Filter.Delay as Delay
import qualified Synthesizer.State.Filter.Recursive.MovingAverage as MA
import qualified Synthesizer.State.Filter.NonRecursive as FiltNR

import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder  as Filt1
import qualified Synthesizer.Plain.Filter.Recursive.Allpass     as Allpass
import qualified Synthesizer.Plain.Filter.Recursive.Universal   as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive.Moog        as Moog
import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butter
import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev   as Cheby
import qualified Synthesizer.Plain.Filter.Recursive             as FiltRec

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Filter.Recursive.Comb as Comb

-- import qualified Synthesizer.Generic.Interpolation as InterpolationG
import qualified Synthesizer.Generic.Filter.Recursive.MovingAverage as MAG
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Filter.Delay as DelayG
import qualified Synthesizer.Generic.Signal  as SigG

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Number.NonNegative     as NonNeg

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
-- import qualified Algebra.VectorSpace    as VectorSpace
import qualified Algebra.Module         as Module

import Foreign.Storable (Storable, )

-- import qualified Data.List as List

-- import Control.Monad(liftM2)

import NumericPrelude.Numeric hiding (negate)
import NumericPrelude.Base as P
import Prelude ()


type Signal s amp yv =
   SigA.T (Rate.Phantom s) amp (Sig.T yv)

{-# INLINE negate #-}
negate :: (Additive.C yv, Dim.C u) =>
      Proc.T s u t (
        Signal s amp yv
     -> Signal s amp yv)
negate = Proc.pure FiltV.negate


{-# INLINE envelope #-}
envelope :: (Flat.C y0 flat, Ring.C y0, Dim.C u) =>
      Proc.T s u t (
        Signal s flat y0        {- v the envelope -}
     -> Signal s amp y0         {- v the signal to be enveloped -}
     -> Signal s amp y0)
envelope = Proc.pure FiltV.envelope

{-# INLINE envelopeVector #-}
envelopeVector ::
   (Flat.C y0 flat, Module.C y0 yv, Dim.C u) =>
      Proc.T s u t (
        Signal s flat y0        {- v the envelope -}
     -> Signal s amp yv         {- v the signal to be enveloped -}
     -> Signal s amp yv)
envelopeVector = Proc.pure FiltV.envelopeVector

{-# INLINE convolveVector #-}
convolveVector ::
   (Module.C q yv, Field.C q, Dim.C u) =>
      Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
                              {- v the filter window -}
     -> Signal s amp yv         {- v the signal to be enveloped -}
     -> Signal s amp yv)
convolveVector =
   do toFreq <- Proc.withParam toFrequencyScalar
      return $ \ window ->
         SigA.processBody
            (FiltNR.generic (SigA.scalarSamples toFreq window))


{- | needs a better handling of boundaries, yet -}
{-# INLINE meanStatic #-}
meanStatic :: (Additive.C yv, RealField.C q,
         Module.C q yv, Dim.C u) =>
      DN.T (Dim.Recip u) q    {- ^ cut-off frequency -}
   -> Proc.T s u q (
        Signal s amp yv
     -> Signal s amp yv)
meanStatic freq =
   do f <- toFrequencyScalar freq
      return $
         let tInt  = round ((recip f - 1)/2)
             width = tInt*2+1
         in  SigA.processBody
                ((asTypeOf (recip (fromIntegral width)) f *> ) .
                 Delay.staticNeg tInt .
                 MA.sumsStaticInt width)

{- | needs a better handling of boundaries, yet -}
{-# INLINE mean #-}
mean :: (Additive.C yv, RealField.C q,
         Module.C q yv, Dim.C u, Storable q, Storable yv) =>
      DN.T (Dim.Recip u) q    {- ^ minimum cut-off frequency -}
   -> Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
                              {- v cut-off frequencies -}
     -> Signal s amp yv
     -> Signal s amp yv)
mean minFreq =
   do mf <- toFrequencyScalar minFreq
      frequencyControl $ \ freqs ->
         let tMax   = ceiling (recip (2*mf))
             err    = error "Filter.mean: frequencies must be positive"
             widths = Sig.map (\f -> if f>0 then recip (2*f) else err) freqs
         in  SigA.processBody
                (fromStorable .
--                 MAG.sumsStaticInt tMax .
                 MAG.modulatedFrac tMax (toStorable widths) .
                 toStorable)

{-# INLINE delay #-}
delay :: (Additive.C yv, RealRing.C t, Dim.C u, SigG.Write sig yv) =>
      DN.T u t
   -> Proc.T s u t (
        SigA.T (Rate.Phantom s) amp (sig yv)
     -> SigA.T (Rate.Phantom s) amp (sig yv))
delay time =
   flip fmap (toTimeScalar time) $
   \t -> SigA.processBody (DelayG.static (round t))


{-# INLINE toStorable #-}
toStorable :: (Storable a) => Sig.T a -> SigSt.T a
toStorable = Sig.toStorableSignal SigSt.defaultChunkSize

{-# INLINE fromStorable #-}
fromStorable :: (Storable a) => SigSt.T a -> Sig.T a
fromStorable = Sig.fromStorableSignal

{-# INLINE phaseModulation #-}
phaseModulation ::
   (Additive.C yv, RealField.C q, Dim.C u,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q
          {- ^ minimal deviation from current time, usually negative -}
   -> DN.T u q
          {- ^ maximal deviation, it must be @minDev <= maxDev@
               and the modulation must always be
               in the range [minDev,maxDev]. -}
   -> Proc.T s u q (
        SigA.R s u q q
          {- v deviation control,
               positive numbers meanStatic prefetch,
               negative numbers meanStatic delay -}
     -> Signal s amp yv
     -> Signal s amp yv)
phaseModulation ip minDev maxDev =
   fmap
      (\f devs ->
         SigA.processBody
            (fromStorable .
             f (SigA.processBody toStorable devs) .
             toStorable))
      (phaseModulationGeneric ip minDev maxDev)

{-# INLINE phaseModulationGeneric #-}
phaseModulationGeneric ::
   (Additive.C yv, RealField.C q, Dim.C u,
    SigG.Transform sig q, SigG.Transform sig yv, SigG.Write sig yv) =>
      Interpolation.T q yv
   -> DN.T u q
          {- ^ minimal deviation from current time, usually negative -}
   -> DN.T u q
          {- ^ maximal deviation, it must be @minDev <= maxDev@
               and the modulation must always be
               in the range [minDev,maxDev]. -}
   -> Proc.T s u q (
        SigA.T (Rate.Phantom s) (Amp.Dimensional u q) (sig q)
          {- v deviation control,
               positive numbers meanStatic prefetch,
               negative numbers meanStatic delay -}
     -> sig yv
     -> sig yv)
phaseModulationGeneric ip minDev _maxDev =
   fmap
      (\toTime devs ->
          let t0    = toTime minDev
              tInt0 = floor t0
          in  DelayG.modulated ip tInt0
                 (SigG.map (max t0) (SigA.scalarSamples toTime devs)))
      (Proc.withParam toTimeScalar)


{-
FIXME: move to Dimensional.Straight
-}
{-# INLINE frequencyModulation #-}
frequencyModulation ::
   (Flat.C t flat,
    Additive.C yv, RealRing.C t, Dim.C u) =>
      Interpolation.T t yv
   -> Proc.T s u t (
        Signal s flat t    {- v frequency factors -}
     -> Signal s amp yv
     -> Signal s amp yv)
frequencyModulation ip =
   Proc.pure $
      \ factors ->
          SigA.processBody
             (interpolateMultiRelativeZeroPad ip (Flat.toSamples factors))

{- |
Frequency modulation where the input signal can have a sample rate
different from the output.
(The sample rate values can differ, the unit must be the same.
We could lift that restriction,
but then the unit handling becomes more complicated,
and I didn't have a use for it so far.)

The function can be used for resampling.
-}
{-# INLINE frequencyModulationDecoupled #-}
frequencyModulationDecoupled ::
   (Flat.C t flat,
    Additive.C yv, RealField.C t, Dim.C u) =>
      Interpolation.T t yv
   -> SigA.T (Rate.Dimensional u t) amp (Sig.T yv)
                   {- ToDo: We could also allow any signal from Generic.Read class. -}
   -> Proc.T s u t (
        Signal s flat t {- v frequency factors -}
     -> Signal s amp yv)
frequencyModulationDecoupled ip y =
   fmap
      (\toFreq factors ->
         SigA.Cons Rate.Phantom (SigA.amplitude y) $
         ($ SigA.body y) $
         interpolateMultiRelativeZeroPad ip $
         SigA.scalarSamples toFreq $
         SigA.fromBody (SigA.actualSampleRate y) $
         Flat.toSamples factors)
      (Proc.withParam Proc.toFrequencyScalar)



{-# INLINE interpolateMultiRelativeZeroPad #-}
interpolateMultiRelativeZeroPad ::
    (RealRing.C q, Additive.C yv) =>
    Interpolation.T q yv
    -> Sig.T q
    -> Sig.T yv
    -> Sig.T yv
interpolateMultiRelativeZeroPad ip k x =
    Causal.apply (Interpolation.relativeZeroPad zero ip zero x) k

{- | symmetric phaser -}
{-# INLINE phaser #-}
phaser ::
   (Additive.C yv, RealField.C q,
    Module.C q yv, Dim.C u,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q  {- ^ maxDev, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                {- v delay control -}
     -> Signal s amp yv
     -> Signal s amp yv)
phaser ip maxDev =
   fmap
      (\p devs ->
         SigA.processBody
            (FiltNR.amplifyVector (SigA.asTypeOfAmplitude 0.5 devs) .
             uncurry Disp.mix . p devs))
      (phaserCore ip maxDev)

{-# INLINE phaserStereo #-}
phaserStereo ::
   (Additive.C yv, RealField.C q,
    Module.C q yv, Dim.C u,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q   {- ^ maxDev, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                 {- v delay control -}
     -> Signal s amp yv
     -> Signal s amp (Stereo.T yv))
phaserStereo ip maxDev =
   fmap
      (\p devs ->
            SigA.processBody (uncurry (Sig.zipWith Stereo.cons) . p devs))
      (phaserCore ip maxDev)

{-# INLINE phaserCore #-}
phaserCore ::
   (Additive.C yv, RealField.C q,
    Module.C q yv, Dim.C u,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q   {- ^ maxDev, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                 {- v delay control -}
     -> Sig.T yv
     -> (Sig.T yv, Sig.T yv))
phaserCore ip maxDev =
   do let minDev  = Additive.negate maxDev
      pm <- phaseModulationGeneric ip minDev maxDev
      return $ \ devs x ->
         let devsPos = SigA.processBody toStorable devs
             devsNeg = SigA.processBody FiltG.negate devsPos
             xst     = toStorable x
         in  (fromStorable (pm devsPos xst),
              fromStorable (pm devsNeg xst))


{-# INLINE firstOrderLowpass #-}
{-# INLINE firstOrderHighpass #-}
firstOrderLowpass, firstOrderHighpass ::
   (Trans.C q, Module.C q yv, Dim.C u) =>
      Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
                    {- v Control signal for the cut-off frequency. -}
     -> Signal s amp yv
                    {- v Input signal -}
     -> Signal s amp yv)
firstOrderLowpass  = firstOrderGen Filt1.lowpassModifier
firstOrderHighpass = firstOrderGen Filt1.highpassModifier

{-# INLINE firstOrderGen #-}
firstOrderGen ::
   (Trans.C q, Module.C q yv, Dim.C u) =>
      (Modifier yv (Filt1.Parameter q) yv yv)
   -> Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
     -> Signal s amp yv
     -> Signal s amp yv)
firstOrderGen modif =
   frequencyControl $ \ freqs ->
      modifyModulated Filt1.parameter modif freqs


{-# INLINE butterworthLowpass #-}
{-# INLINE butterworthHighpass #-}
{-# INLINE chebyshevALowpass #-}
{-# INLINE chebyshevAHighpass #-}
{-# INLINE chebyshevBLowpass #-}
{-# INLINE chebyshevBHighpass #-}

butterworthLowpass, butterworthHighpass,
   chebyshevALowpass, chebyshevAHighpass,
   chebyshevBLowpass, chebyshevBHighpass ::
      (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u) =>
      NonNeg.Int   {- ^ Order of the filter, must be even,
                        the higher the order, the sharper is the separation of frequencies. -}
   -> Proc.T s u q (
        Signal s flat q {- v The attenuation at the cut-off frequency.
                           Should be between 0 and 1. -}
     -> SigA.R s (Dim.Recip u) q q
                      {- v Control signal for the cut-off frequency. -}
     -> Signal s amp yv {- v Input signal -}
     -> Signal s amp yv)

butterworthLowpass  = higherOrderNoResoGen Butter.lowpassPole
butterworthHighpass = higherOrderNoResoGen Butter.highpassPole
chebyshevALowpass   = higherOrderNoResoGen Cheby.lowpassAPole
chebyshevAHighpass  = higherOrderNoResoGen Cheby.highpassAPole
chebyshevBLowpass   = higherOrderNoResoGen Cheby.lowpassBPole
chebyshevBHighpass  = higherOrderNoResoGen Cheby.highpassBPole


-- ToDo: switch from list to more efficient data structure
{-# INLINE higherOrderNoResoGen #-}
higherOrderNoResoGen ::
   (Flat.C q flat, Field.C q, Dim.C u) =>
      (Int -> [q] -> [q] -> [yv] -> [yv])
   -> NonNeg.Int
   -> Proc.T s u q (
        Signal s flat q
     -> SigA.R s (Dim.Recip u) q q
     -> Signal s amp yv
     -> Signal s amp yv)
higherOrderNoResoGen filt order =
   fmap flip $ frequencyControl $ \ freqs ratios ->
      SigA.processBody
         (Sig.fromList .
          filt (NonNeg.toNumber order)
             (Sig.toList (Flat.toSamples ratios)) (Sig.toList freqs) .
          Sig.toList)



{-# INLINE highpassFromUniversal #-}
{-# INLINE bandpassFromUniversal #-}
{-# INLINE lowpassFromUniversal #-}
{-# INLINE bandlimitFromUniversal #-}
highpassFromUniversal, lowpassFromUniversal,
  bandpassFromUniversal, bandlimitFromUniversal ::
        Signal s amp (UniFilter.Result yv)
     -> Signal s amp yv
{-
   (Dim.C u) =>
      Proc.T s u q (
        Signal s amp (UniFilter.Result yv)
     -> Signal s amp yv)
-}
highpassFromUniversal  = homogeneousMap UniFilter.highpass
bandpassFromUniversal  = homogeneousMap UniFilter.bandpass
lowpassFromUniversal   = homogeneousMap UniFilter.lowpass
bandlimitFromUniversal = homogeneousMap UniFilter.bandlimit

homogeneousMap ::
   (y0 -> y1) ->
   SigA.T rate amp (Sig.T y0) -> SigA.T rate amp (Sig.T y1)
homogeneousMap f =
   SigA.processBody (Sig.map f)

{-
homogeneousMap0 :: (Hom.C sig) =>
   (y0 -> y1) ->
   Signal s amp y0 -> Signal s amp y1
homogeneousMap0 f =
   SigA.processBody (Sig.map f)

homogeneousMap1 :: (Hom.C sig) =>
   (y0 -> y1) ->
   Proc.T s1 u t (Signal s amp y0 -> Signal s amp y1)
homogeneousMap1 f =
   Proc.pure (SigA.processBody (Sig.map f))
-}


{-# INLINE universal #-}
universal ::
   (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u) =>
      Proc.T s u q (
        Signal s flat q
                    {- v signal for resonance,
                         i.e. factor of amplification at the resonance frequency
                         relatively to the transition band. -}
     -> SigA.R s (Dim.Recip u) q q
                    {- v signal for cut off and band center frequency -}
     -> Signal s amp yv
                    {- v input signal -}
     -> Signal s amp (UniFilter.Result yv))
                    {- ^ highpass, bandpass, lowpass filter -}
universal =
   fmap flip $ frequencyControl $ \ freqs reso ->
      let resos = Flat.toSamples reso
      in  modifyModulated
             UniFilter.parameter
             UniFilter.modifier
             (Sig.zipWith FiltRec.Pole resos freqs)

{-# INLINE moogLowpass #-}
moogLowpass :: (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u) =>
      NonNeg.Int
   -> Proc.T s u q (
        Signal s flat q
                   {- v signal for resonance,
                        i.e. factor of amplification at the resonance frequency
                        relatively to the transition band. -}
     -> SigA.R s (Dim.Recip u) q q
                   {- v signal for cut off frequency -}
     -> Signal s amp yv
     -> Signal s amp yv)
moogLowpass order =
   fmap flip $ frequencyControl $ \ freqs reso ->
      let resos = Flat.toSamples reso
          orderInt = NonNeg.toNumber order
      in  modifyModulated
             (Moog.parameter orderInt)
             (Moog.lowpassModifier orderInt)
             (Sig.zipWith FiltRec.Pole resos freqs)


{-# INLINE allpassCascade #-}
allpassCascade :: (Trans.C q, Module.C q yv, Dim.C u) =>
      NonNeg.Int  {- ^ order, number of filters in the cascade -}
   -> q           {- ^ the phase shift to be achieved for the given frequency -}
   -> Proc.T s u q (
        SigA.R s (Dim.Recip u) q q {- v lowest comb frequency -}
     -> Signal s amp yv
     -> Signal s amp yv)
allpassCascade order phase =
   frequencyControl $ \ freqs ->
      let orderInt = NonNeg.toNumber order
      in  modifyModulated
             (Allpass.cascadeParameter orderInt phase)
             (Allpass.cascadeModifier orderInt)
             freqs

{-# INLINE allpassFlangerPhase #-}
allpassFlangerPhase :: Trans.C a => a
allpassFlangerPhase = Allpass.flangerPhase


{- | Infinitely many equi-delayed exponentially decaying echos. -}
{-# INLINE comb #-}
comb :: (RealRing.C t, Module.C y yv, Dim.C u, Storable yv) =>
   DN.T u t -> y -> Proc.T s u t (Signal s amp yv -> Signal s amp yv)
comb time gain =
   do t <- toTimeScalar time
      return $ SigA.processBody
         (fromStorable . Comb.run (round t) gain . toStorable)


-- * auxiliary functions

{-# INLINE frequencyControl #-}
frequencyControl :: (Dim.C u, Field.C y) =>
      (Sig.T y -> t)
   -> Proc.T s u y (
        SigA.R s (Dim.Recip u) y y
     -> t)
frequencyControl f =
   do toFreq <- Proc.withParam toFrequencyScalar
      return $ \ freq -> f (SigA.scalarSamples toFreq freq)


{-# INLINE modifyModulated #-}
modifyModulated ::
   (param -> ctrl) ->
   Modifier state ctrl y0 y1 ->
   Sig.T param ->
   Signal s amp y0 ->
   Signal s amp y1
modifyModulated makeParam modif params =
   SigA.processBody (Sig.modifyModulated modif (Sig.map makeParam params))
