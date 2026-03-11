module Synthesizer.ALSA.Storable.Server.Common where

import qualified Sound.ALSA.PCM as ALSA
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Synthesizer.ALSA.Storable.Play as Play
import qualified Synthesizer.ALSA.EventList as MIDIEv
import Synthesizer.ALSA.EventList (StrictTime, )

import qualified Synthesizer.Generic.Signal       as SigG
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import Sound.MIDI.Message.Channel (Channel, )

import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Sound.Sox.Frame         as SoxFrame
import qualified System.Exit as Exit

import Control.Category ((.), )

import qualified Algebra.RealField as RealField
import qualified Algebra.Field     as Field
import qualified Algebra.Ring      as Ring
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric (zero, round, )
import Prelude hiding (Real, round, break, id, (.), )


channel :: Channel
channel = ChannelMsg.toChannel 0

sampleRate :: Num a => a
-- sampleRate = 24000
-- sampleRate = 48000
sampleRate = 44100

latency :: Int
latency = 0
-- latency = 256
-- latency = 1000

chunkSize :: SVL.ChunkSize
chunkSize = Play.defaultChunkSize

lazySize :: SigG.LazySize
lazySize =
   let (SVL.ChunkSize size) = chunkSize
   in  SigG.LazySize size

periodTime :: Field.C t => t
periodTime =
   let (SVL.ChunkSize size) = chunkSize
   in  ToInteger.fromIntegral size Field./ Ring.fromInteger sampleRate

device :: Play.Device
device = Play.defaultDevice

clientName :: MIDIEv.ClientName
clientName = MIDIEv.ClientName "Haskell-Synthesizer"


type Real = Float


{-# INLINE withMIDIEvents #-}
withMIDIEvents ::
   (Double -> Double -> a -> IO b) ->
   (EventList.T StrictTime [Event.T] -> a) -> IO b
withMIDIEvents action proc =
   let rate = sampleRate
       per  = periodTime
   in  MIDIEv.withMIDIEvents clientName per rate $
       action per rate . proc


{-# INLINE play #-}
play ::
   (RealField.C t, Additive.C y, ALSA.SampleFmt y) =>
   t -> t -> SigSt.T y -> IO ()
play period rate =
   Play.auto (Play.makeSink device period (round rate)) .
   SigSt.append (SigSt.replicate chunkSize latency zero)
--   FiltG.delayPosLazySize chunkSize latency
--   FiltG.delayPos latency

-- ToDo: do not record the empty chunk that is inserted for latency
{-# INLINE playAndRecord #-}
playAndRecord ::
   (RealField.C t, Additive.C y, ALSA.SampleFmt y, SoxFrame.C y) =>
   FilePath -> t -> t -> SigSt.T y -> IO Exit.ExitCode
playAndRecord fileName period rate =
   let intRate = round rate
   in  Play.autoAndRecord fileName
          (ALSA.SoundFmt {ALSA.sampleFreq = intRate})
          (Play.makeSink device period intRate) .
       SigSt.append (SigSt.replicate chunkSize latency zero)
