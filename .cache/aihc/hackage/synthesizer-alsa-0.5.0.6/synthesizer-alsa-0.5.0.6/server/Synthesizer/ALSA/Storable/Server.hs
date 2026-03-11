module Main where

import qualified Synthesizer.ALSA.Storable.Server.Run as Run
import qualified Synthesizer.ALSA.Storable.Server.Test as Test
import Synthesizer.ALSA.Storable.Server.Common
          (Real, play, sampleRate, chunkSize, periodTime, )

import qualified Synthesizer.Basic.Wave          as Wave

import qualified Synthesizer.ALSA.Storable.Play as Play
import qualified Synthesizer.Storable.Oscillator  as OsciSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy as SVL

import NumericPrelude.Numeric (zero, )
import Prelude hiding (Real, break, id, (.), )


{-
This program has still a very slowly growing memory leak.
-}
main :: IO ()
main =
   case fromInteger 300 :: Int of
      001 -> print Test.keyboard3
      002 -> play (periodTime::Real) sampleRate Test.keyboard3
      003 -> Test.speed
      004 -> Test.frequency1
      005 -> Test.frequency2
      006 -> Test.frequency3
{-
      007 -> Test.frequency4
-}
      008 -> Test.keyboard1
      009 -> SigSt.writeFile "test.f32" Test.keyboard2
      010 -> SigSt.writeFile "test.f32" Test.keyboard3
      011 -> SigSt.writeFile "test.f32" Test.keyboard4
      012 -> SigSt.writeFile "test.f32" Test.keyboard5
{-
      013 -> Test.keyboard6
      014 -> Test.keyboard7
-}
      015 -> Test.arrangeSpaceLeak0
      016 -> Test.arrangeSpaceLeak1
      018 -> Test.arrangeSpaceLeak3
      019 -> Test.arrangeSpaceLeak4
      020 -> Test.chordSpaceLeak1
--      021 -> Test.chordSpaceLeak2
--      022 -> Test.chordSpaceLeak3
--      023 -> Test.chordSpaceLeak4
      023 -> Test.sequencePitchBend
      024 -> Test.sequencePitchBend1
      025 -> Test.sequencePitchBend2
      026 -> Test.sequencePitchBend3
      027 -> Test.sequencePitchBend4
      028 -> Test.sequencePitchBend4a
      029 -> Test.sequencePitchBend4b
      030 -> Test.sequencePitchBend4c
      031 -> Test.sequencePitchBend4d
      032 -> Test.sequencePitchBend4e
      033 -> Test.sequencePitchBend5
      040 -> Test.sequenceStaccato
      050 -> Test.speed
      051 -> Test.speedChunky
      052 -> Test.speedArrange
      053 -> Play.auto
{-
                (ALSA.alsaSoundSinkTime Play.defaultDevice
                    (ALSA.SoundFmt {
                       ALSA.sampleFreq = sampleRate
                    }) $
                 ALSA.SoundBufferTime
                    (round (5000000*periodTime::Real))
                    (round (1000000*periodTime::Real))
                ) $
-}
{-
                (ALSA.fileSoundSink "test.f32"
                   (ALSA.SoundFmt {
                      ALSA.sampleFreq = sampleRate
                   })) $
-}
                (Play.makeSink Play.defaultDevice (periodTime::Real) sampleRate) $
             SVL.cycle $
             SigSt.take 100000 $
                OsciSt.static chunkSize (fmap (0.9*) Wave.sine) zero (1/100::Real)
      054 -> Play.auto
                (Play.makeSink Play.defaultDevice (periodTime::Real) sampleRate) $
                OsciSt.static chunkSize (fmap (0.9*) Wave.sine)
                   zero (600/sampleRate::Real)
      055 -> play (periodTime::Real) sampleRate $
                OsciSt.static chunkSize (fmap (0.9*) Wave.sine)
                   zero (600/sampleRate::Real)
      100 -> Run.volume
      101 -> Run.frequency
      102 -> Run.frequencyCausal
      103 -> Run.pitchBend
      104 -> Run.volumeFrequency
      105 -> Run.volumeFrequencyCausal
      200 -> Run.keyboard
      201 -> Run.keyboardMulti
      202 -> Run.keyboardStereo
      203 -> Run.keyboardPitchbend
      204 -> Run.keyboardFM
      205 -> Run.keyboardDetuneFM
      206 -> Run.keyboardFilter
      207 -> Run.keyboardSample
      208 -> Run.keyboardVariousStereo
      209 -> Run.keyboardSampleTFM
      210 -> Run.keyboardNoisyTone
      211 -> Run.keyboardFilteredNoisyTone
      300 -> Run.keyboardCausal
      _   -> error "not implemented server part"
