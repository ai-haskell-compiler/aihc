{-# LANGUAGE RebindableSyntax #-}
module Synthesizer.ALSA.Dimensional.Server.Run where

import Synthesizer.ALSA.Dimensional.Server.Common
import qualified Synthesizer.MIDI.Dimensional.Example.Instrument as Instr

import qualified Synthesizer.MIDI.Dimensional as MIDI

import qualified Synthesizer.ALSA.Storable.Play as PlaySt

import qualified Synthesizer.Dimensional.Causal.Process    as Causal
import qualified Synthesizer.Dimensional.Causal.Oscillator as Osci
import qualified Synthesizer.Dimensional.Causal.Filter     as Filt
import qualified Synthesizer.Dimensional.Causal.FilterParameter   as FiltP
import qualified Synthesizer.Dimensional.Causal.ControlledProcess as CProc

import qualified Synthesizer.Dimensional.Rate.Oscillator as OsciR
import qualified Synthesizer.Dimensional.Amplitude.Control as CtrlA
import qualified Synthesizer.Dimensional.Amplitude.Displacement as DispA
import qualified Synthesizer.Dimensional.Amplitude.Filter as FiltA
import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Wave as WaveD

import Synthesizer.Dimensional.Causal.Process ((<<<), )
import Synthesizer.Dimensional.Wave ((&*~), )
import Synthesizer.Dimensional.Process (($:), (.:), )
import Control.Applicative (liftA2, liftA3, (<$>), )

import qualified Synthesizer.Basic.Wave          as Wave

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Number.DimensionTerm  as DN

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (break, )



channelVolume :: VoiceMsg.Controller
channelVolume = VoiceMsg.modulation


volume :: IO ()
volume =
   putStrLn "run 'aconnect' to connect to the MIDI controller" >>
   (withMIDIEvents play $
   \evs ->
      liftA3
          (\env osci vol ->
              Causal.apply
                 (Causal.applySnd env osci) $
              MIDI.piecewiseConstant $ vol)
          Filt.envelopeScalarDimension
          (OsciR.static (DN.voltage 1 &*~ Wave.sine) zero (DN.frequency (880::Real)))
          (MIDI.runFilter evs (MIDI.controllerLinear channel channelVolume
              (DN.scalar 0, DN.scalar 1) (DN.scalar (1::Real)))))

pitchBend :: IO ()
pitchBend =
   withMIDIEvents play $
   \evs ->
      liftA2 Causal.apply
          (Osci.freqMod (DN.voltage (1::Real) &*~ Wave.sine) zero)
          (MIDI.piecewiseConstant <$>
           MIDI.runFilter evs
              (MIDI.pitchBend channel 2 (DN.frequency (880::Real))))

-- preserve chunk structure of channel volume
volumePitchBend0 :: IO ()
volumePitchBend0 =
   putStrLn "run 'aconnect' to connect to the MIDI controller" >>
   (withMIDIEvents play $
   \evs ->
      liftA3
          (\osci env (freq,vol) ->
              Causal.apply
                 (Causal.applySnd env (osci $ SigA.restore freq)) $
                 MIDI.piecewiseConstant vol)
          (OsciR.freqMod (DN.voltage 1 &*~ Wave.sine) zero)
          Filt.envelopeScalarDimension
          (MIDI.runFilter evs $ liftA2 (,)
             (MIDI.pitchBend channel 2 (DN.frequency (880::Real)))
             (MIDI.controllerLinear channel channelVolume
                (DN.scalar 0, DN.scalar 1) (DN.scalar (1::Real)))))

-- preserve chunk structure of pitch bender
volumePitchBend1 :: IO ()
volumePitchBend1 =
   putStrLn "run 'aconnect' to connect to the MIDI controller" >>
   (withMIDIEvents play $
   \evs ->
      liftA3
          (\osci env (freq,vol) ->
              Causal.apply
                 (Causal.applyFst env (SigA.restore vol) <<< osci) $
                 MIDI.piecewiseConstant freq)
          (Osci.freqMod (DN.voltage 1 &*~ Wave.sine) zero)
          Filt.envelopeScalarDimension
          (MIDI.runFilter evs $ liftA2 (,)
             (MIDI.pitchBend channel 2 (DN.frequency (880::Real)))
             (MIDI.controllerLinear channel channelVolume
                (DN.scalar 0, DN.scalar 1) (DN.scalar (1::Real)))))


keyboard :: IO ()
keyboard =
   withMIDIEvents play $
   \evs ->
      MIDI.runFilter evs
         (MIDI.sequence PlaySt.defaultChunkSize (DN.voltage 1) channel Instr.ping)


keyboardMulti :: IO ()
keyboardMulti =
   withMIDIEvents play $
   \evs ->
      MIDI.runFilter evs
         (MIDI.sequenceMultiProgram PlaySt.defaultChunkSize (DN.voltage 1) channel
             (VoiceMsg.toProgram 0)
             [Instr.ping, Instr.pingRelease])
--             [Instr.string])


keyboardFM :: IO ()
keyboardFM =
   withMIDIEvents play $
   \evs ->
      FiltA.amplify 0.3 <$>
         (MIDI.runFilter evs
            (MIDI.sequenceModulated PlaySt.defaultChunkSize (DN.voltage 1) channel Instr.pingReleaseFM $:
             MIDI.bendWheelPressure channel 2 (DN.frequency 10) 0.04 0.03))
--             MIDI.pitchBend channel (2 ** recip 12) (DN.scalar one)))


extraController :: VoiceMsg.Controller
extraController =
   VoiceMsg.vectorX
--   VoiceMsg.toController 21

extraController1 :: VoiceMsg.Controller
extraController1 =
   VoiceMsg.modulation
--   VoiceMsg.vectorY
--   VoiceMsg.toController 22


keyboardDetuneFM :: IO ()
keyboardDetuneFM =
   withMIDIEvents play $
   \evs ->
      FiltA.amplify 0.3 <$>
         (MIDI.runFilter evs
            (MIDI.sequenceMultiModulated PlaySt.defaultChunkSize (DN.voltage 1) channel Instr.pingStereoDetuneFM
              ((MIDI.applyModulation <$>
                  MIDI.bendWheelPressure channel 2 (DN.frequency 10) 0.04 0.03)
               .:
               (MIDI.applyModulation <$>
                  MIDI.controllerLinear channel extraController (0, 0.005) 0))
               ))


keyboardFilter :: IO ()
keyboardFilter =
   withMIDIEvents play $
   \evs ->
        liftA3
           (\osci filt (music,speed,depth) ->
              (FiltP.lowpassFromUniversal <<<
               filt (CtrlA.constant 10)
                 (DispA.mapExponential 4 (DN.frequency 1000) $
                  FiltA.envelope (SigA.restore depth) $
                  osci (SigA.restore speed)))
              `Causal.apply`
              FiltA.amplify 0.2 music)
           (OsciR.freqMod (WaveD.flat Wave.sine) zero)
           (CProc.runSynchronous2 FiltP.universal)
--           FiltR.universal
           (MIDI.runFilter evs
              (liftA3 (,,)
                 (MIDI.sequence PlaySt.defaultChunkSize (DN.voltage 1) channel Instr.string)
                 (MIDI.controllerExponential channel extraController
                     (DN.frequency 0.1, DN.frequency 5) (DN.frequency 0.2))
                 (MIDI.controllerLinear channel extraController1
                     (0, 1 :: DN.Scalar Real) 0.5)
            ))
