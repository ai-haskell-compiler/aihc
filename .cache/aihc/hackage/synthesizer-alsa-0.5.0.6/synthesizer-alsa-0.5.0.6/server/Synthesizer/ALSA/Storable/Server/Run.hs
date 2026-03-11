module Synthesizer.ALSA.Storable.Server.Run where

import qualified Synthesizer.MIDI.Example.Instrument as Instr
import Synthesizer.ALSA.Storable.Server.Common
          (Real, withMIDIEvents, play, device, clientName,
           sampleRate, lazySize, chunkSize, periodTime, channel, )

import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.MIDI.Storable as MidiSt
import Synthesizer.MIDI.Storable (applyModulation, )

import qualified Synthesizer.ALSA.CausalIO.Process as PAlsa
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.Causal.Oscillator as OsciC
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltNRC

import qualified Synthesizer.Basic.Wave          as Wave

import qualified Synthesizer.Interpolation.Module as Ip

import qualified Synthesizer.Storable.Oscillator  as OsciSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL
import qualified Data.StorableVector              as SV
import Foreign.Storable (Storable, )

import qualified Synthesizer.Generic.Loop      as LoopG
import qualified Synthesizer.Generic.Signal    as SigG
import qualified Synthesizer.State.Signal      as SigS
import qualified Synthesizer.Plain.Filter.Recursive    as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Control.Monad.Trans.State (evalState, )
import Control.Category ((.), )
import Control.Arrow (arr, second, (&&&), )

import Data.Tuple.HT (mapSnd, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.), )
import Prelude ()



volume :: IO ()
volume =
   putStrLn "run 'aconnect' to connect to the MIDI controller" >>
   (withMIDIEvents play $
      SigSt.zipWith (*)
         (OsciSt.static chunkSize Wave.sine zero (800/sampleRate)) .
      evalState (MidiSt.controllerLinear channel VoiceMsg.mainVolume (0,1) (0.2::Real)))

frequency :: IO ()
frequency =
   withMIDIEvents play $
      OsciSt.freqMod chunkSize Wave.sine zero .
      evalState (MidiSt.controllerExponential channel VoiceMsg.modulation
         (400/sampleRate, 1600/sampleRate) (800/sampleRate::Real))


{-# INLINE storableChunk #-}
storableChunk ::
   (SigG.Read sig a, Storable a) =>
   sig a -> SV.Vector a
storableChunk chunk =
   SigS.toStrictStorableSignal (SigG.length chunk) $
   SigG.toState chunk

frequencyCausal :: IO ()
frequencyCausal =
   PAlsa.playFromEvents device clientName 0.01 (periodTime::Double) sampleRate
      ((PIO.fromCausal $
        Causal.applyStorableChunk $
        OsciC.freqMod (fmap (0.99*) Wave.sine) zero)
       .
       arr storableChunk
       .
       (MIO.controllerExponential channel VoiceMsg.modulation
          (400/sampleRate, 1600/sampleRate) (800/sampleRate::Real)))


pitchBend :: IO ()
pitchBend =
   withMIDIEvents play $
      OsciSt.freqMod chunkSize Wave.sine zero .
      evalState (MidiSt.pitchBend channel 2 (880/sampleRate::Real))

volumeFrequency :: IO ()
volumeFrequency =
   putStrLn "run 'aconnect' to connect to the MIDI controller" >>
   (withMIDIEvents play $
      evalState (do
         vol  <- MidiSt.controllerLinear channel VoiceMsg.mainVolume (0,1) 0.5
         freq <- MidiSt.pitchBend channel 2 (880/sampleRate::Real)
         return $
            SigSt.zipWith (*) vol
               (OsciSt.freqMod chunkSize Wave.sine zero freq)))

volumeFrequencyCausal :: IO ()
volumeFrequencyCausal =
   PAlsa.playFromEvents device clientName 0.01 (periodTime::Double) sampleRate
      ((PIO.fromCausal $
        Causal.applyStorableChunk $
        FiltNRC.envelope
        .
        second (OsciC.freqMod Wave.sine zero))
       .
       arr (uncurry (SV.zipWith (,)))
       .
       (arr storableChunk
        .
        MIO.controllerLinear channel VoiceMsg.mainVolume (0,0.99) (0.5::Real)
        &&&
        arr storableChunk
        .
        MIO.pitchBend channel 2 (880/sampleRate::Real)))


keyboard :: IO ()
keyboard =
   withMIDIEvents play $
--      playALSA (Bld.put :: Int16 -> Bld.Builder Int16) (sampleRate::Real) .
      SigSt.map (0.2*) .
      evalState (MidiSt.sequence chunkSize channel Instr.tine)

keyboardMulti :: IO ()
keyboardMulti =
   withMIDIEvents play $
--      playALSA (Bld.put :: Int16 -> Bld.Builder Int16) (sampleRate::Real) .
      SigSt.map (0.2*) .
      evalState (MidiSt.sequenceMultiProgram chunkSize channel
         (VoiceMsg.toProgram 2)
         [Instr.pingDur, Instr.pingRelease, Instr.tine])

keyboardStereo :: IO ()
keyboardStereo =
   withMIDIEvents play $
--      playALSA (Bld.put :: Int16 -> Bld.Builder Int16) (sampleRate::Real) .
      SigSt.map ((0.2::Real)*>) .
      evalState (MidiSt.sequenceMultiProgram chunkSize channel
         (VoiceMsg.toProgram 1)
         [Instr.pingStereoRelease, Instr.tineStereo,
          Instr.softString, Instr.softStringCausal])

keyboardPitchbend :: IO ()
keyboardPitchbend =
   withMIDIEvents play $
      SigSt.map ((0.2::Real)*>) .
      evalState
         (do bend <- MidiSt.pitchBend channel (2^?(2/12)) 1
             MidiSt.sequenceModulated chunkSize bend channel Instr.stringStereoFM)

keyboardFM :: IO ()
keyboardFM =
   withMIDIEvents play $
      SigSt.map ((0.2::Real)*>) .
      evalState
         (do fm <- MidiSt.bendWheelPressure channel
                      2 (10/sampleRate) 0.04 0.03
             MidiSt.sequenceModulated chunkSize fm channel Instr.stringStereoFM)

keyboardDetuneFM :: IO ()
keyboardDetuneFM =
   withMIDIEvents play $
      SigSt.map ((0.2::Real)*>) .
      evalState
         (do fm <- MidiSt.bendWheelPressure channel
                      2 (10/sampleRate) 0.04 0.03
             detune <- MidiSt.controllerLinear channel
                          VoiceMsg.vectorX (0,0.005) 0
             MidiSt.sequenceMultiModulated
                chunkSize channel Instr.stringStereoDetuneFM
                (applyModulation fm .
                 applyModulation detune))


keyboardFilter :: IO ()
keyboardFilter =
   withMIDIEvents play $
      SigSt.map (0.2*) .
      evalState
         (do music <- MidiSt.sequence chunkSize channel Instr.pingRelease
             freq  <- MidiSt.controllerLinear channel
                         -- VoiceMsg.vectorY
                         (VoiceMsg.toController 21)
                         (100/sampleRate, 5000/sampleRate)
                         (700/sampleRate)
             return $
                SigS.toStorableSignal chunkSize $
                SigS.map UniFilter.lowpass $
                SigS.modifyModulated
                   UniFilter.modifier
                   (SigS.map UniFilter.parameter $
                    SigS.zipWith FiltR.Pole
                       (SigS.repeat (5 :: Real))
                       (SigS.fromStorableSignal freq)) $
                SigS.fromStorableSignal music)


makeLoop ::
   LoopG.TimeControl Real -> Real -> Real ->
   (Real, SigSt.T Real) -> (Real, SigSt.T Real)
makeLoop = LoopG.timeReverse lazySize Ip.linear Ip.linear

keyboardSample :: IO ()
keyboardSample =
   do piano <- Instr.readPianoSample
      string <- Instr.readStringSample
      let loopedString     = mapSnd (LoopG.simple 8750 500) string
          fadedString      = mapSnd (LoopG.fade (undefined::Real) 8750 500) string
          timeSineString   = makeLoop LoopG.timeControlSine 8750 500 string
          timeZigZagString = makeLoop LoopG.timeControlZigZag 8750 500 string
      withMIDIEvents play $
         SigSt.map (0.2*) .
         evalState (MidiSt.sequenceMultiProgram chunkSize channel
               (VoiceMsg.toProgram 5) $
            Instr.sampledSound piano :
            Instr.sampledSound string :
            Instr.sampledSound loopedString :
            Instr.sampledSound fadedString :
            Instr.sampledSound timeSineString :
            Instr.sampledSound timeZigZagString :
            Instr.sampledSoundTimeLoop Instr.loopTimeModSine string 8750 500 :
            Instr.sampledSoundTimeLoop Instr.loopTimeModZigZag string 8750 500 :
            [])


keyboardVariousStereo :: IO ()
keyboardVariousStereo =
   do piano <- Instr.readPianoSample
      string <- Instr.readStringSample
      let loopedString = makeLoop LoopG.timeControlZigZag 8750 500 string
      withMIDIEvents play $
         SigSt.map ((0.2::Real)*>) .
         evalState (MidiSt.sequenceMultiProgram chunkSize channel
               (VoiceMsg.toProgram 0) $
            Instr.pingStereoRelease :
            Instr.tineStereo :
            Instr.softString :
            Instr.sampledSoundDetuneStereo 0.001 piano :
            Instr.sampledSoundDetuneStereo 0.002 loopedString :
            Instr.sampledSoundDetuneStereoRelease 0.1 0.001 piano :
            Instr.sampledSoundDetuneStereoRelease 0.3 0.002 loopedString :
            [])


keyboardSampleTFM :: IO ()
keyboardSampleTFM =
   do instr <- Instr.readPianoSample
      withMIDIEvents play $
         evalState
            (do fm <- MidiSt.bendWheelPressure channel
                         2 (10/sampleRate) 0.04 0.03
                speed <- MidiSt.controllerLinear channel
                             (VoiceMsg.toController 22)
                             (0,2) 1
                offset <- MidiSt.controllerLinear channel
                             (VoiceMsg.toController 21)
                             (0, fromIntegral (SVL.length (snd instr))) 0
                MidiSt.sequenceMultiModulated
                   chunkSize channel (Instr.timeModulatedSample instr)
                   (applyModulation fm .
                    applyModulation speed .
                    applyModulation offset))


keyboardNoisePipe :: IO ()
keyboardNoisePipe =
   withMIDIEvents play $
      evalState
         (do fm <- MidiSt.bendWheelPressure channel
                      2 (10/sampleRate) 0.04 0.03
             resonance <-
                   MidiSt.controllerExponential channel
                      (VoiceMsg.toController 23)
                      (1, 100) 10
             MidiSt.sequenceMultiModulated
                chunkSize channel Instr.colourNoise
                (applyModulation fm .
                 applyModulation resonance))


keyboardNoisyTone :: IO ()
keyboardNoisyTone =
   withMIDIEvents play $
      evalState
         (do fm <- MidiSt.bendWheelPressure channel
                      2 (10/sampleRate) 0.04 0.03
             speed <- MidiSt.controllerLinear channel
                          (VoiceMsg.toController 21)
                          (0,0.5) 0.1
             MidiSt.sequenceMultiModulated
                chunkSize channel Instr.toneFromNoise
                (applyModulation fm .
                 applyModulation speed))


keyboardFilteredNoisyTone :: IO ()
keyboardFilteredNoisyTone =
   withMIDIEvents play $
      evalState
         (do fm <- MidiSt.bendWheelPressure channel
                      2 (10/sampleRate) 0.04 0.03
             {-
             speed must never be zero,
             since this requires to fetch unlimited data from future.
             -}
             speed <- MidiSt.controllerLinear channel
                          (VoiceMsg.toController 21)
                          (0.05,0.5) 0.1
             cutoff <- MidiSt.controllerExponential channel
                          (VoiceMsg.toController 22)
                          (1, 30) 10
             resonance <- MidiSt.controllerExponential channel
                          (VoiceMsg.toController 23)
                          (1, 20) 5
             MidiSt.sequenceMultiModulated
                chunkSize channel Instr.toneFromFilteredNoise
                (applyModulation fm .
                 applyModulation speed .
                 applyModulation cutoff .
                 applyModulation resonance))



keyboardCausal :: IO ()
keyboardCausal =
   PAlsa.playFromEvents device clientName 0.01 (periodTime::Double) sampleRate $
      arr (SV.map (0.2*))
      .
      MIO.sequenceStorable channel (\ _pgm -> Instr.pingReleaseCausal)
