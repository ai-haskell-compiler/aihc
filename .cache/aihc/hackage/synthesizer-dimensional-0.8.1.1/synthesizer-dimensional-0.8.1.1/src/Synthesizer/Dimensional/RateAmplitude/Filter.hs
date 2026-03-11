{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.RateAmplitude.Filter (
   {- * Non-recursive -}

   {- ** Amplification -}
   amplify,
   amplifyDimension,
   negate,
   envelope,
   envelopeVector,
   envelopeVectorDimension,
   {- ** Filter operators from calculus -}
   differentiate,

{-
   {- ** Smooth -}
   meanStatic,
   mean,

   {- ** Delay -}
   delay,
   phaseModulation,
   frequencyModulation,
   frequencyModulationDecoupled,
   phaser,
   phaserStereo,


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
   FiltR.highpassFromUniversal,
   FiltR.bandpassFromUniversal,
   FiltR.lowpassFromUniversal,
   FiltR.bandlimitFromUniversal,
   moogLowpass,

   {- ** Allpass -}
   allpassCascade,
   FiltR.allpassFlangerPhase,
-}

   {- ** Reverb -}
   comb,
   combProc,

   {- ** Filter operators from calculus -}
--   integrate,
) where

import qualified Synthesizer.Dimensional.Rate.Filter as FiltR
import qualified Synthesizer.Dimensional.Amplitude.Filter as FiltV
-- import qualified Synthesizer.Dimensional.ControlledProcess as CProc
import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Rate as Rate

-- import Synthesizer.Dimensional.Process ((.:), (.^), )

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
-- import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.State.Signal as Sig
-- import Synthesizer.Plain.Signal (Modifier)

import Synthesizer.Dimensional.Process
   (DimensionGradient, toTimeScalar, {- toFrequencyScalar, -} )

{-
import qualified Synthesizer.Frame.Stereo as Stereo
-}
import Foreign.Storable (Storable, )

{-
-- import qualified Synthesizer.State.Displacement as Disp
import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.State.Filter.Delay as Delay
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder  as Filt1
import qualified Synthesizer.Plain.Filter.Recursive.Allpass     as Allpass
import qualified Synthesizer.Plain.Filter.Recursive.Universal   as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive.Moog        as Moog
import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butter
import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev   as Cheby
import qualified Synthesizer.State.Filter.Recursive.Integration as Integrate
import qualified Synthesizer.State.Filter.Recursive.MovingAverage as MA
import qualified Synthesizer.Plain.Filter.Recursive    as FiltRec
-}
import qualified Synthesizer.State.Filter.NonRecursive as FiltNR

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Filter.Recursive.Comb as Comb

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&), {- (&/&), -} )

-- import qualified Number.NonNegative     as NonNeg

-- import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute           as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
-- import qualified Algebra.VectorSpace    as VectorSpace
import qualified Algebra.Module         as Module

-- import Control.Monad(liftM2)

import NumericPrelude.Numeric hiding (negate)
import NumericPrelude.Base as P
import Prelude ()


type FlatSignal s flat y0 =
   SigA.T (Rate.Phantom s) flat (Sig.T y0)


{- | The amplification factor must be positive. -}
{-# INLINE amplify #-}
amplify :: (Ring.C y, Dim.C u, Dim.C v) =>
      y
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
amplify volume = Proc.pure $ FiltV.amplify volume

{-# INLINE amplifyDimension #-}
amplifyDimension :: (Ring.C y, Dim.C u, Dim.C v0, Dim.C v1) =>
      DN.T v0 y
   -> Proc.T s u t (
        SigA.R s v1 y yv
     -> SigA.R s (Dim.Mul v0 v1) y yv)
amplifyDimension volume = Proc.pure $ FiltV.amplifyDimension volume


{-# INLINE negate #-}
negate :: (Additive.C yv, Dim.C u, Dim.C v) =>
      Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
negate = Proc.pure FiltV.negate


{-# INLINE envelope #-}
envelope :: (Flat.C y0 flat, Ring.C y0, Dim.C u, Dim.C v) =>
      Proc.T s u t (
        FlatSignal s flat y0   {- v the envelope -}
     -> SigA.R s v y y0  {- v the signal to be enveloped -}
     -> SigA.R s v y y0)
envelope = Proc.pure FiltV.envelope

{-# INLINE envelopeVector #-}
envelopeVector :: (Flat.C y0 flat, Module.C y0 yv, Ring.C y, Dim.C u, Dim.C v) =>
      Proc.T s u t (
        FlatSignal s flat y0   {- v the envelope -}
     -> SigA.R s v y yv  {- v the signal to be enveloped -}
     -> SigA.R s v y yv)
envelopeVector = Proc.pure FiltV.envelopeVector

{-# INLINE envelopeVectorDimension #-}
envelopeVectorDimension ::
   (Module.C y0 yv, Ring.C y, Dim.C u, Dim.C v0, Dim.C v1) =>
      Proc.T s u t (
        SigA.R s v0 y y0  {-  the envelope -}
     -> SigA.R s v1 y yv  {-  the signal to be enveloped -}
     -> SigA.R s (Dim.Mul v0 v1) y yv)
envelopeVectorDimension = Proc.pure FiltV.envelopeVectorDimension


{-# INLINE differentiate #-}
differentiate :: (Additive.C yv, Ring.C q, Dim.C u, Dim.C v) =>
      Proc.T s u q (
        SigA.R s v q yv
     -> SigA.R s (DimensionGradient u v) q yv)
differentiate =
   flip fmap Proc.getSampleRate $ \rate ->
      \ x ->
         SigA.fromBody
            (rate &*& SigA.actualAmplitude x)
            (FiltNR.differentiate (SigA.body x))


{-
{- | needs a good handling of boundaries, yet -}
{-# INLINE meanStatic #-}
meanStatic ::
   (RealRing.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) q   {- ^ cut-off frequency -}
   -> Proc.T s u q (
        SigA.R s v q yv
     -> SigA.R s v q yv)
meanStatic time =
   FiltR.meanStatic time

meanStaticSeparateTY :: (Additive.C yv, Field.C y, RealRing.C t,
         Module.C y yv, Dim.C u, Dim.C v) =>
      DN.T (Dim.Recip u) t   {- ^ cut-off frequency -}
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
meanStaticSeparateTY time =
   -- FiltR.meanStatic time, means that 't' = 'y'
   do f <- toFrequencyScalar time
      return $ \ x ->
         let tInt  = round ((recip f - 1)/2)
             width = tInt*2+1
         in  SigA.processBody
                ((SigA.asTypeOfAmplitude (recip (fromIntegral width)) x *> ) .
                 Delay.staticNeg tInt .
                 MA.sumsStaticInt width) x


{- | needs a better handling of boundaries, yet -}
{-# INLINE mean #-}
mean ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Storable q, Storable yv) =>
      DN.T (Dim.Recip u) q    {- ^ minimum cut-off frequency -}
   -> Proc.T s u q (
        SigA.R s (Dim.Recip u) q q
                              {- v cut-off frequencies -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
mean minFreq =
   FiltR.mean minFreq


{-# INLINE delay #-}
delay :: (Additive.C yv, Field.C y, RealRing.C t, Dim.C u, Dim.C v) =>
      DN.T u t
   -> Proc.T s u t (
        SigA.R s v y yv
     -> SigA.R s v y yv)
delay time =
   do t <- toTimeScalar time
      return $ SigA.processBody (Delay.static (round t))


{-# INLINE phaseModulation #-}
phaseModulation ::
   (Additive.C yv, RealRing.C q, Dim.C u, Dim.C v,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q
          {- ^ minDelay, minimal delay, may be negative -}
   -> DN.T u q
          {- ^ maxDelay, maximal delay, it must be @minDelay <= maxDelay@
               and the modulation must always be
               in the range [minDelay,maxDelay]. -}
   -> Proc.T s u q (
        SigA.R s u q q
          {- v delay control, positive numbers meanStatic delay,
               negative numbers meanStatic prefetch -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
phaseModulation ip minDelay maxDelay =
   FiltR.phaseModulation ip minDelay maxDelay

{-# INLINE frequencyModulation #-}
frequencyModulation ::
   (Flat.C q flat, Additive.C yv, RealRing.C q, Dim.C u, Dim.C v) =>
      Interpolation.T q yv
   -> Proc.T s u q (
        FlatSignal s flat q    {- v frequency factors -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
frequencyModulation ip =
   Proc.pure $
      \ factors ->
          SigA.processBody
             (FiltR.interpolateMultiRelativeZeroPad ip (Flat.toSamples factors))

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
   (Flat.C q flat, Additive.C yv, RealRing.C q, Dim.C u, Dim.C v) =>
      Interpolation.T q yv
   -> Proc.T s u q (
        FlatSignal s flat q    {- v frequency factors -}
     -> SigP.T u q (SigA.D v q SigS.S) yv
     -> SigA.R s v q yv)
frequencyModulationDecoupled ip =
   fmap
      (\toFreq factors y ->
         flip SigA.processBody (RP.fromSignal (SigP.signal y)) $
            FiltR.interpolateMultiRelativeZeroPad ip
               (SigA.scalarSamples toFreq
                  (SigA.fromBody (SigA.actualSampleRate y) (Flat.toSamples factors))))
      (Proc.withParam Proc.toFrequencyScalar)


{- | symmetric phaser -}
{-# INLINE phaser #-}
phaser ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q  {- ^ maxDelay, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                {- v delay control -}
     -> SigA.R s v q yv
     -> SigA.R s v q yv)
phaser = FiltR.phaser
{-
phaser ip maxDelay =
   do p <- phaserCore ip maxDelay
      return $ \ delays x ->
         FiltV.amplify 0.5 .
         uncurry MiscV.mix . p delays $ x
-}

{-# INLINE phaserStereo #-}
phaserStereo ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q   {- ^ maxDelay, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                 {- v delay control -}
     -> SigA.R s v q yv
     -> SigA.R s v q (Stereo.T yv))
phaserStereo = FiltR.phaserStereo
{-
phaserStereo ip maxDelay =
   do p <- phaserCore ip maxDelay
      return $ \ delays -> uncurry CutV.zip . p delays
-}

{-
{-# INLINE phaserCore #-}
phaserCore ::
   (Additive.C yv, RealRing.C q,
    Module.C q yv, Dim.C u, Dim.C v,
    Storable q, Storable yv) =>
      Interpolation.T q yv
   -> DN.T u q   {- ^ maxDelay, must be positive -}
   -> Proc.T s u q (
        SigA.R s u q q
                 {- v delay control -}
     -> SigA.R s v q yv
     -> (SigA.R s v q yv, SigA.R s v q yv))
phaserCore ip maxDelay =
   do let minDelay  = Additive.negate maxDelay
      pm <- phaseModulation ip minDelay maxDelay
      return $ \ delays x ->
         let negDelays = FiltV.negate delays
         in  (pm delays x,
              pm negDelays x)
-}


type FrequencyFilter s u q r ic v yv0 yv1 =
   Proc.T s u q
      (CProc.T s
          (SigA.R r (Dim.Recip u) q q)
                    {- v Control signal for the cut-off frequency. -}
          ic
          (SigA.R s v q yv0 ->
                    {- v Input signal -}
           SigA.R s v q yv1))
                    {- v Output signal -}

{-# INLINE firstOrderLowpass #-}
{-# INLINE firstOrderHighpass #-}
firstOrderLowpass, firstOrderHighpass ::
   (Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
   FrequencyFilter s u q r (Filt1.Parameter q) v yv yv
firstOrderLowpass  = firstOrderGen Filt1.lowpassModifier
firstOrderHighpass = firstOrderGen Filt1.highpassModifier

{-# INLINE firstOrderGen #-}
firstOrderGen ::
   (Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      (Modifier yv (Filt1.Parameter q) yv yv)
--      (Sig.T (Filt1.Parameter q) -> Sig.T yv -> Sig.T yv)
   -> FrequencyFilter s u q r (Filt1.Parameter q) v yv yv
firstOrderGen modif =
   frequencyControl Filt1.parameter
      (Sig.modifyModulated modif)



{-# INLINE butterworthLowpass #-}
{-# INLINE butterworthHighpass #-}
{-# INLINE chebyshevALowpass #-}
{-# INLINE chebyshevAHighpass #-}
{-# INLINE chebyshevBLowpass #-}
{-# INLINE chebyshevBHighpass #-}

butterworthLowpass, butterworthHighpass,
   chebyshevALowpass, chebyshevAHighpass,
   chebyshevBLowpass, chebyshevBHighpass ::
      (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      NonNeg.Int   {- ^ Order of the filter, must be even,
                        the higher the order, the sharper is the separation of frequencies. -}
   -> ResonantFilter s u q r flat (FiltRec.Pole q) v yv yv

butterworthLowpass  = higherOrderNoResoGen Butter.lowpassPole
butterworthHighpass = higherOrderNoResoGen Butter.highpassPole
chebyshevALowpass   = higherOrderNoResoGen Cheby.lowpassAPole
chebyshevAHighpass  = higherOrderNoResoGen Cheby.highpassAPole
chebyshevBLowpass   = higherOrderNoResoGen Cheby.lowpassBPole
chebyshevBHighpass  = higherOrderNoResoGen Cheby.highpassBPole

{- FIXME:
currently only frequencies can be interpolated not the filter parameters,
this is not very efficient
-}
{- ToDo:
initial value
-}
{-# INLINE higherOrderNoResoGen #-}
higherOrderNoResoGen ::
   (Flat.C q flat, Field.C q, Dim.C u, Dim.C v) =>
      (Int -> [q] -> [q] -> [yv] -> [yv])
   -> NonNeg.Int
   -> ResonantFilter s u q r flat (FiltRec.Pole q) v yv yv

higherOrderNoResoGen filt order =
   frequencyResonanceControl id
      (\ cs xs ->
          let csl = Sig.toList cs
          in  Sig.fromList (filt (NonNeg.toNumber order)
                 (map FiltRec.poleResonance csl)
                 (map FiltRec.poleFrequency csl)
                 (Sig.toList xs)))


type ResonantFilter s u q r flat ic v yv0 yv1 =
   Proc.T s u q
      (CProc.T s
         (FlatSignal r flat q
                   {- v signal for resonance,
                        i.e. factor of amplification at the resonance frequency
                        relatively to the transition band. -},
          SigA.R r (Dim.Recip u) q q
                   {- v signal for cut off frequency -} )
         ic
         (SigA.R s v q yv0 ->
          SigA.R s v q yv1))


{-# INLINE universal #-}
universal ::
   (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
   ResonantFilter s u q r flat (UniFilter.Parameter q) v yv (UniFilter.Result yv)
universal =
   frequencyResonanceControl
      UniFilter.parameter
      (Sig.modifyModulated UniFilter.modifier)

{-# INLINE moogLowpass #-}
moogLowpass :: (Flat.C q flat, Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      NonNeg.Int
   -> ResonantFilter s u q r flat (Moog.Parameter q) v yv yv
moogLowpass order =
   let orderInt = NonNeg.toNumber order
   in  frequencyResonanceControl
          (Moog.parameter orderInt)
          (Sig.modifyModulated (Moog.lowpassModifier orderInt))


{-# INLINE allpassCascade #-}
{- | the lowest comb frequency is used as the filter frequency -}
allpassCascade :: (Trans.C q, Module.C q yv, Dim.C u, Dim.C v) =>
      NonNeg.Int  {- ^ order, number of filters in the cascade -}
   -> q           {- ^ the phase shift to be achieved for the given frequency -}
   -> FrequencyFilter s u q r (Allpass.Parameter q) v yv yv
allpassCascade order phase =
   let orderInt = NonNeg.toNumber order
   in  frequencyControl
          (Allpass.cascadeParameter orderInt phase)
          (Sig.modifyModulated (Allpass.cascadeModifier orderInt))


{-# INLINE frequencyControl #-}
frequencyControl ::
   (Field.C q, Dim.C u, Dim.C v) =>
   (q -> ic) ->
   (Sig.T ic -> Sig.T yv0 -> Sig.T yv1) ->
   FrequencyFilter s u q r ic v yv0 yv1

frequencyControl mkParam filt =
   do toFreq <- Proc.withParam toFrequencyScalar
      return $ CProc.Cons
         (\ freqs -> Sig.map mkParam (SigA.scalarSamples toFreq freqs))
         (\ params -> SigA.processBody (filt params))


{-# INLINE frequencyResonanceControl #-}
frequencyResonanceControl ::
   (Flat.C q flat, Field.C q, Dim.C u, Dim.C v) =>
   (FiltRec.Pole q -> ic) ->
   (Sig.T ic -> Sig.T yv0 -> Sig.T yv1) ->
   ResonantFilter s u q r flat ic v yv0 yv1

frequencyResonanceControl mkParam filt =
   do toFreq <- Proc.withParam toFrequencyScalar
      return $ CProc.Cons
         (\ (resos, freqs) ->
               Sig.map mkParam $
               Sig.zipWith FiltRec.Pole
                  (Flat.toSamples resos)
                  (SigA.scalarSamples toFreq freqs))
         (\ params -> SigA.processBody (filt params))
-}


{- | Infinitely many equi-delayed exponentially decaying echos. -}
{-# INLINE comb #-}
comb :: (RealRing.C t, Module.C y yv, Dim.C u, Dim.C v, Storable yv) =>
   DN.T u t -> y -> Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
comb = FiltR.comb


{- | Infinitely many equi-delayed echos processed by an arbitrary time-preserving signal processor. -}
{-# INLINE combProc #-}
combProc ::
   (RealRing.C t, Absolute.C y, Field.C y, Module.C y yv,
    Dim.C u, Dim.C v, Storable yv) =>
   DN.T u t ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv) ->
   Proc.T s u t (SigA.R s v y yv -> SigA.R s v y yv)
combProc time proc =
   do f <- proc
      t <- fmap round $ toTimeScalar time
      let chunkSize = SigSt.chunkSize t
      return $ \x ->
         SigA.processBody
            (Sig.fromStorableSignal .
             Comb.runProc t
                (Sig.toStorableSignal chunkSize .
                 SigA.vectorSamples (SigA.toAmplitudeScalar x) .
                 f .
                 SigA.fromBody (SigA.actualAmplitude x) .
                 Sig.fromStorableSignal) .
             Sig.toStorableSignal chunkSize) x

{-
combProc time proc sr x =
   Rate.loop (\sr' y -> MiscV.mixVolume (SigA.actualAmplitude x) x (delay time sr' (proc sr' y))) sr
-}

{-
{-# INLINE integrate #-}
integrate :: (Additive.C yv, Field.C q, Dim.C u, Dim.C v) =>
      Proc.T s u q (
        SigA.R s v q yv
     -> SigA.R s (Dim.Mul u v) q yv)
integrate =
   flip fmap Proc.getSampleRate $ \rate ->
      \ x ->
         SigA.replaceAmplitude
            (DN.rewriteDimension (Dim.commute . Dim.applyRightMul Dim.invertRecip) $
             SigA.actualAmplitude x &/& rate)
            (Hom.processSamples Integrate.run x)
-}
