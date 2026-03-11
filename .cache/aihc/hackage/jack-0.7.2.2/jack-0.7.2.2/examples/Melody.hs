module Main where

import Common (mainWait)

import qualified Sound.JACK.MIDI as JackMidi
import qualified Sound.JACK as Jack
import qualified Sound.MIDI.Message as MIDI
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.Class.Construct as MidiCons

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Numeric.NonNegative.Wrapper as NonNegW
import Data.IORef (IORef, newIORef, readIORef, writeIORef, )

import qualified Foreign.C.Error as E

import System.Environment (getProgName)


scale :: [Channel.Pitch]
scale = map Channel.toPitch [60, 62, 64, 65, 67, 69, 71, 72]

eventLoop :: EventList.T NonNegW.Double MIDI.T
eventLoop =
    EventList.fromPairList $
    concatMap
        (\p ->
            let note on =
                    MidiCons.note (Channel.toChannel 0)
                        (Voice.normalVelocity, p, on)
            in  [(0, note True), (0.1, note False)])
        scale

main :: IO ()
main = do
    name <- getProgName
    Jack.handleExceptions $
        Jack.withClientDefault name $ \client ->
        Jack.withPort client "output" $ \output -> do
            rate <- fmap fromIntegral $ Trans.lift $ Jack.getSampleRate client
            stateRef <-
                Trans.lift $
                newIORef (EventList.resample rate $ EventList.cycle eventLoop)
            Jack.withProcess client (process stateRef output) $
                mainWait client name

process ::
    IORef (EventList.T NonNegW.Int MIDI.T) ->
    JackMidi.Port Jack.Output ->
    Jack.NFrames ->
    Sync.ExceptionalT E.Errno IO ()
process stateRef output nframes@(Jack.NFrames nframesInt) = do
    events <- Trans.lift $ readIORef stateRef
    let (currentEvents, futureEvents) =
            EventListTM.splitAtTime (fromIntegral nframesInt) events
    Trans.lift $ writeIORef stateRef futureEvents
    JackMidi.writeEventsToPort output nframes $
        AbsEventList.mapTime (Jack.NFrames . NonNegW.toNumber . fromIntegral) $
        EventList.toAbsoluteEventList 0 $
        fst $ EventListTM.viewTimeR currentEvents
