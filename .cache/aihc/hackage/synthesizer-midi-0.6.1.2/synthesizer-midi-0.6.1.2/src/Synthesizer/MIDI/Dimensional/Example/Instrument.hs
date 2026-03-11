{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.MIDI.Dimensional.Example.Instrument where

import qualified Synthesizer.MIDI.Dimensional as MIDI
import qualified Synthesizer.MIDI.PiecewiseConstant as PC

import qualified Synthesizer.Dimensional.Causal.Process    as Causal
import qualified Synthesizer.Dimensional.Causal.Filter     as Filt

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Rate.Cut as CutR
import qualified Synthesizer.Dimensional.Rate.Control as CtrlR
import qualified Synthesizer.Dimensional.Rate.Oscillator as OsciR
import qualified Synthesizer.Dimensional.Rate.Filter as FiltR
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Amplitude.Cut as CutA
import qualified Synthesizer.Dimensional.Amplitude.Displacement as DispA
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude.Analysis as AnaA
import qualified Synthesizer.Dimensional.Amplitude.Filter as FiltA
import qualified Synthesizer.Dimensional.RateAmplitude.Control as CtrlD
import qualified Synthesizer.Dimensional.ChunkySize.Signal as SigC
import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc

import Synthesizer.Dimensional.Causal.Process ((<<<), )
import Synthesizer.Dimensional.Wave ((&*~), )
import Synthesizer.Dimensional.Process (($:), )
import Synthesizer.Dimensional.Signal ((&*^), )
import Control.Applicative (liftA3, )

import qualified Synthesizer.Basic.Wave          as Wave
import qualified Synthesizer.Frame.Stereo        as Stereo

import qualified Synthesizer.Storable.Signal      as SigSt

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (break, )


type Real = Double


{-# INLINE ping #-}
ping :: MIDI.Instrument s Dim.Time Dim.Voltage Real (SigSt.T Real)
ping vel freq =
   fmap (flip SigC.store)
      (FiltR.envelope
         $: CtrlR.exponential2 (DN.time 0.2)
         $: OsciR.static (DN.voltage (4**vel) &*~ Wave.saw) zero freq)


{-
Generating the envelope requires great care:
 - you must avoid an append function that determines the common volume automatically,
   because the volume of the second part is only known after the first part is complete
 - you must terminate the release phase,
   otherwise you get an infinite signal for every played note
-}
{-# INLINE pingReleaseEnvelope #-}
pingReleaseEnvelope ::
   Real ->
   Proc.T s Dim.Time Real
      (MIDI.LazyTime s ->
       SigA.T (Rate.Phantom s) (Amp.Dimensional Dim.Scalar Real) (SigSt.T Real))
pingReleaseEnvelope vel =
   Proc.withParam $ \dur ->
   do decay <-
         fmap (SigC.store dur) $
         CtrlR.exponential2 (DN.time 0.4)
      end <- fmap (AnaA.endPrimitive zero) $ fmap ($ decay) SigA.embedSampleRate
      release <-
         SigA.store (DN.time 0.01) $:
         (CutR.take (DN.time 0.3) $:
          fmap Flat.canonicalize
            (DN.scalar end &*^ CtrlR.exponential2 (DN.time 0.1)))
      append <- CutR.append
      return $ DispA.inflate (DN.fromNumber $ 4**vel) (append decay release)
--      return $ DispA.inflate (DN.fromNumber $ 4**vel) decay

{-
   Proc.withParam $ \dur ->
   liftA2
      (\embed env ->
          let x = SigC.store dur env
              y = AnaA.end $ embed x
          in  )
      SigA.embedSampleRate
      (FiltR.envelope
         $: CtrlR.exponential2 (DN.time 0.2)
         $: OsciR.static (DN.voltage (4**vel) &*~ Wave.saw) zero freq)
-}

{-# INLINE pingRelease #-}
pingRelease :: MIDI.Instrument s Dim.Time Dim.Voltage Real (SigSt.T Real)
pingRelease vel freq =
   liftA3
      (\env ctrl osci dur ->
          Causal.apply
             (env <<< Causal.feedSnd osci)
             (ctrl dur))
      Filt.envelopeScalarDimension
      (pingReleaseEnvelope vel)
      (OsciR.static (DN.voltage 1 &*~ Wave.saw) zero freq)


{-# INLINE pingReleaseFM #-}
pingReleaseFM ::
   MIDI.ModulatedInstrument s Dim.Time Real
      (MIDI.Signal s Dim.Scalar Real (SigSt.T Real) ->
       MIDI.Signal s Dim.Voltage Real (SigSt.T Real))
pingReleaseFM vel freq =
   liftA3
      (\env ctrl osci dur fm ->
          Causal.apply
             (env <<<
              Causal.feedSnd (osci (FiltA.amplifyScalarDimension freq $ SigA.restore fm)))
             (ctrl dur))
      Filt.envelopeScalarDimension
      (pingReleaseEnvelope vel)
      (OsciR.freqMod (DN.voltage 1 &*~ Wave.saw) zero)


{-# INLINE pingStereoDetuneFM #-}
pingStereoDetuneFM ::
   MIDI.ModulatedInstrument s Dim.Time Real
      (MIDI.Signal s Dim.Scalar Real (PC.T Real) ->
       MIDI.Signal s Dim.Scalar Real (SigSt.T Real) ->
       MIDI.Signal s Dim.Voltage Real (SigSt.T (Stereo.T Real)))
pingStereoDetuneFM vel freq =
   liftA3
      (\env ctrl osci dur detuneSt fmSt ->
          let fm     = SigA.restore fmSt
              detune = SigA.restore detuneSt
              osciChan d =
                 osci (FiltA.amplifyScalarDimension freq
                    (FiltA.envelope (DispA.raise 1 d) fm))
          in  SigA.rewriteAmplitudeDimension Dim.identityLeft $
              Causal.apply
                 (env <<<
                  Causal.feedSnd (CutA.mergeStereo
                     (osciChan detune)
                     (osciChan $ FiltA.negate detune)))
                 (ctrl dur))
      Filt.envelopeVectorDimension
      (pingReleaseEnvelope vel)
      (OsciR.freqMod (DN.voltage 1 &*~ Wave.saw) zero)


{- INLINE stringReleaseEnvelope -}
stringReleaseEnvelope ::
   Real ->
   Proc.T s Dim.Time Real
      (MIDI.LazyTime s ->
       SigA.T (Rate.Phantom s) (Amp.Dimensional Dim.Scalar Real) (SigSt.T Real))
stringReleaseEnvelope vel =
   Proc.withParam $ \dur ->
   do let attackTime = DN.time 1
      cnst <- CtrlR.constant
      {-
      release <- take attackTime beginning
      would yield a space leak, thus we first split 'beginning'
      and then concatenate it again
      -}
      {-
      We can not easily generate attack and sustain separately,
      because we want to use the chunk structure implied by 'dur'.
      -}
      (attack, sustain) <-
         CutR.splitAt attackTime $:
         (fmap (SigC.store dur .
                flip CutA.appendPrimitive cnst .
                DispA.map sin . Flat.canonicalize)
            (CtrlD.line attackTime (0, DN.scalar (pi/2))))
      let release = CutA.reverse attack
--          infixr 5 append
      append <- CutR.append
      return $
         DispA.inflate (DN.fromNumber $ 4**vel) $
         attack `append` sustain `append` release

{- INLINE string -}
string ::
   MIDI.ModulatedInstrument s Dim.Time Real
      (MIDI.Signal s Dim.Voltage Real (SigSt.T (Stereo.T Real)))
string vel freq =
   liftA3
      (\env ctrl osci dur ->
          SigA.rewriteAmplitudeDimension Dim.identityLeft $
          Causal.apply
             (env <<< Causal.feedSnd osci)
             (ctrl dur))
      Filt.envelopeVectorDimension
      (stringReleaseEnvelope vel)
      (Proc.pure CutA.mergeStereo
         $: (Proc.pure DispA.mix
              $: OsciR.static (DN.voltage 0.5 &*~ Wave.saw) zero (DN.scale 1.005 freq)
              $: OsciR.static (DN.voltage 0.5 &*~ Wave.saw) zero (DN.scale 0.998 freq))
         $: (Proc.pure DispA.mix
              $: OsciR.static (DN.voltage 0.5 &*~ Wave.saw) zero (DN.scale 1.002 freq)
              $: OsciR.static (DN.voltage 0.5 &*~ Wave.saw) zero (DN.scale 0.995 freq)))
