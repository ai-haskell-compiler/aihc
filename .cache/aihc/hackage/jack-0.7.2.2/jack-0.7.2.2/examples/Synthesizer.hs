module Main where

import Common (mainWait)

import qualified Synthesizer.Render as Render

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Sound.JACK.MIDI as MIDI
import qualified Sound.JACK.Audio as Audio
import qualified Sound.JACK as JACK

import qualified Data.EventList.Absolute.TimeBody as EventList

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.State.Strict as MS
import qualified Control.Monad.Trans.Class as Trans
import qualified Foreign.C.Error as E
import Foreign.Marshal.Array (copyArray, )

import System.Environment (getProgName, )

import qualified Data.StorableVector.Base as SVB

import Data.IORef (IORef, newIORef, readIORef, writeIORef, )


main :: IO ()
main = do
    name <- getProgName
    stateRef <- newIORef Render.initialState
    JACK.handleExceptions $
        JACK.withClientDefault name $ \client ->
        JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
        JACK.withProcess client (process client stateRef input output) $
            mainWait client name

checkVoiceMsg :: Msg.T -> Maybe VoiceMsg.T
checkVoiceMsg ev =
    case ev of
       Msg.Channel (ChannelMsg.Cons _chan (ChannelMsg.Voice dat)) ->
          Just dat
       _ -> Nothing

intFromNFrames :: Integral i => JACK.NFrames -> i
intFromNFrames (JACK.NFrames n) = fromIntegral n

runStateOnIORef :: IORef s -> MS.State s a -> IO a
runStateOnIORef ref m = do
    (a, state) <- fmap (MS.runState m) $ readIORef ref
    writeIORef ref state
    return a

process ::
    JACK.Client ->
    IORef (Render.State Audio.Sample) ->
    MIDI.Port JACK.Input ->
    Audio.Port JACK.Output ->
    JACK.NFrames ->
    Sync.ExceptionalT E.Errno IO ()
process client stateRef input output nframes = do
    evs <- MIDI.readEventsFromPort input nframes
    Trans.lift $ do
        rate <- JACK.getSampleRate client
        outArr <- Audio.getBufferPtr output nframes
        block <-
            runStateOnIORef stateRef $
            Render.run (intFromNFrames nframes) rate $
            EventList.toPairList $
            EventList.mapMaybe checkVoiceMsg $
            EventList.mapTime intFromNFrames evs
        SVB.withStartPtr block $ \src len -> copyArray outArr src len
