module Synthesizer.Storable.Generate where

import qualified Synthesizer.Storable.Oscillator as Osci
import qualified Synthesizer.Storable.Signal as SigSt

import qualified Algebra.Additive as Additive
import qualified Algebra.RealRing as RealRing
import qualified Algebra.Transcendental as Trans

import Foreign.Storable (Storable, )


{- |
@clickTrack silenceChunkSize barBeepFreq beatBeepFreq beepDur beatsPerBar beatPeriod@
generates click track for one bar.
You may cycle it infinitely or replicate it as often as you want.
-}
clickTrack ::
   (RealRing.C a, Trans.C a, Storable a) =>
   SigSt.ChunkSize ->
   a -> a -> Int -> Int -> Int -> SigSt.T a
clickTrack chunkSize
      barBeepFreq beatBeepFreq beepDur beatsPerBar beatPeriod =
   let beep freq =
          SigSt.take beepDur
             (Osci.staticSine (SigSt.chunkSize beepDur) Additive.zero freq)
          `SigSt.append`
          SigSt.replicate chunkSize (beatPeriod-beepDur) Additive.zero
   in  SigSt.concat $
          beep barBeepFreq :
          replicate (beatsPerBar-1) (beep beatBeepFreq)

clickTrackExample :: SigSt.T Float
clickTrackExample =
   SigSt.concat $ replicate (3*8) $
   clickTrack SigSt.defaultChunkSize 0.04 0.02 500 4 (84*441)
