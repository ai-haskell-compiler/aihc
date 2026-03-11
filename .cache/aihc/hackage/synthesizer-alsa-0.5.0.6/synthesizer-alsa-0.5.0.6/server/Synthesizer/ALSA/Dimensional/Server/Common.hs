{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Synthesizer.ALSA.Dimensional.Server.Common where

import qualified Synthesizer.ALSA.Dimensional.Play as Play

import qualified Sound.ALSA.PCM as ALSA
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Synthesizer.ALSA.EventList as MIDIEv

import qualified Synthesizer.ALSA.Storable.Play as PlaySt

import qualified Synthesizer.Dimensional.Rate.Filter as FiltR
import qualified Synthesizer.Dimensional.Process as Proc

import Synthesizer.Dimensional.Process (($:), )

import qualified Data.StorableVector.Lazy         as SVL

import qualified Sound.MIDI.Message.Channel       as ChannelMsg

import qualified Data.EventList.Relative.TimeBody  as EventList

-- import qualified Numeric.NonNegative.Class   as NonNeg
-- import qualified Numeric.NonNegative.Wrapper as NonNegW
-- import qualified Numeric.NonNegative.ChunkyPrivate as NonNegChunky

import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring

import qualified Algebra.DimensionTerm as Dim
import qualified Number.DimensionTerm  as DN

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (break, )



channel :: ChannelMsg.Channel
channel = ChannelMsg.toChannel 0


sampleRate :: Ring.C a => DN.Frequency a
-- sampleRate = DN.frequency 48000
sampleRate = DN.frequency 44100

latency :: Field.C a => DN.Time a
latency = DN.time 0
-- latency = DN.time 0.01

{-
chunkSize :: SVL.ChunkSize
chunkSize = Play.defaultChunkSize
-}

periodTime :: Field.C a => DN.Time a
periodTime =
   let (SVL.ChunkSize size) = PlaySt.defaultChunkSize
   in  DN.scale (fromIntegral size) $ DN.unrecip sampleRate

device :: Play.Device
device = PlaySt.defaultDevice

clientName :: MIDIEv.ClientName
clientName = MIDIEv.ClientName "Haskell-Synthesizer"


type Real = Double


{-# INLINE withMIDIEvents #-}
withMIDIEvents ::
   Field.C t =>
   (DN.Time t -> DN.Frequency t ->
    (forall s. Proc.T s Dim.Time t
        (Play.StorableSignal s Dim.Voltage y yv)) ->
    IO b) ->
   (EventList.T MIDIEv.StrictTime [Event.T] ->
    forall s. Proc.T s Dim.Time t
       (Play.StorableSignal s Dim.Voltage y yv)) ->
   IO b
withMIDIEvents action proc =
   MIDIEv.withMIDIEvents clientName
      (DN.toNumberWithDimension Dim.time periodTime :: Double)
      (DN.toNumberWithDimension Dim.frequency sampleRate :: Double) $
   \ sig -> action periodTime sampleRate (proc sig)

{-# INLINE play #-}
play ::
   (Module.C y yv, ALSA.SampleFmt yv, RealField.C t) =>
   DN.Time t ->
   DN.Frequency t ->
   (forall s. Proc.T s Dim.Time t
      (Play.StorableSignal s Dim.Voltage y yv)) ->
   IO ()
play period rate sig =
   Play.renderTimeVoltageStorable device period rate
   (FiltR.delay latency $: sig)
