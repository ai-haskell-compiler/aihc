--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Event
-- Copyright : (c) Henning Thielemann, 2011
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- This module contains functions for working with events.
-- Reference:
-- <http://www.alsa-project.org/alsa-doc/alsa-lib/group___seq_event.html>
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Event (
  -- * Output

  -- $trouble

  output,
  outputBuffer,
  outputDirect,
  outputPending,
  extractOutput,
  removeOutput,
  drainOutput,
  dropOutput,
  dropOutputBuffer,
  syncOutputQueue,

  -- * Input

  input,
  inputPending,
  dropInput,
  dropInputBuffer,

  -- * Data types

  Event.T(..), simple, forSourcePort, forConnection,
  Event.Data(..),
  Event.Type,
  NoteEv(..), Note(..), simpleNote,
  CtrlEv(..), Ctrl(..),
  CustomEv(..), Custom(..), customZero,
  ExtEv(..),
  QueueEv(..),
  AddrEv(..),
  ConnEv(..),
  EmptyEv(..),

  Tag(..),
  Tempo(..),

  Parameter(..),
  Value(..),
  Channel(..),
  Pitch(..),
  Velocity(..), normalVelocity, offVelocity,
  Duration(..),

{- available in alsa-1.0.14, but gone in 1.0.22
  Instr(..),
  Sample(..),
  Cluster(..),
  VolumeControl(..),

  InstrCluster(..),
  StandardId(..),
  Bank(..),
  Program(..),
  Volume(..), volumeSame,
  Balance(..),
-}
  ) where

import Sound.ALSA.Sequencer.Marshal.Event as Event

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, nullPtr, )
import Foreign.Marshal.Alloc (alloca, )
import Foreign.Storable (peek, )

import Data.Word (Word, )


{- $trouble
If you send an event but you do not hear something,
then check the following conditions:

* Check whether your event was actually delivered.
  E.g. use the event monitor @aseqdump@ in order to see sent messages.

* If the message was not delivered,
  maybe you quit the program too early
  and thus destroyed pending messages.
  Use 'syncOutputQueue' at the end of the program
  in order to wait for all messages to be sent.

* Make sure the sequencer supports output
  and the target port supports input.

* Make sure to setup a connection to the receiver.

* Make sure to have called 'drainOutput' after 'output'.

* If you use a timestamp in an event,
  make sure you also declare a queue (and create one before).

* Make sure you started the queue you used for sending.

* Be aware of that 'Event.QueueStart' clears the queue before running the queue.
  That is, events sent before 'Event.QueueStart' are deleted.
  If you want to keep these events, then use 'Event.QueueContinue' instead.
-}


-- | Wait until all events of the client are processed.
syncOutputQueue :: Seq.T mode -> IO ()
syncOutputQueue (Seq.Cons h) =
  Exc.checkResult_ "syncOutputQueue" =<< snd_seq_sync_output_queue h

foreign import ccall safe "alsa/asoundlib.h snd_seq_sync_output_queue"
  snd_seq_sync_output_queue :: Ptr Seq.Core -> IO C.CInt


-- | Get an event from the input buffer.
-- If the input buffer is empty, then it is filled with data from the
-- sequencer queue.  If there is no data in the sequencer queue,
-- then the process is either put to sleep (if the sequencer is operating
-- in blocking mode), or we throw @EAGAIN@ (if the sequence is operating
-- in non-blocking mode).
--
-- We may also throw @ENOSPC@, which means that the sequencer queue
-- over-run and some events were lost (this clears the input buffer).
--
input :: Seq.AllowInput mode => Seq.T mode -> IO Event.T
input (Seq.Cons h) = alloca $ \p ->
  do Exc.checkResult_ "input" =<< snd_seq_event_input h p
     peek =<< peek p

foreign import ccall safe "alsa/asoundlib.h snd_seq_event_input"
  snd_seq_event_input :: Ptr Seq.Core -> Ptr (Ptr Event.T) -> IO C.CInt


checkResult :: String -> C.CInt -> IO Word
checkResult loc n =
   fmap fromIntegral $ Exc.checkResult loc n


-- | Returns the number of events in the input buffer.
-- If the input buffer is empty and the boolean argument is true,
-- then try to fill the input buffer with data from the sequencer queue.
-- See also: 'input'.

inputPending
  :: Seq.AllowInput mode
  => Seq.T mode
  -> Bool     -- ^ refill if empty?
  -> IO Word  -- ^ number of events in buffer
inputPending (Seq.Cons h) fill =
  checkResult "inputPending" =<< snd_seq_event_input_pending h (if fill then 1 else 0)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_event_input_pending"
  snd_seq_event_input_pending :: Ptr Seq.Core -> C.CInt -> IO C.CInt



-- | Output an event and drain the buffer, if it became full.
-- Throws exceptions.
-- See also: 'outputDirect', 'outputBuffer',
--           'outputPending', 'drainOutput', 'dropOutput',
--           'extractOutput', 'removeOutput'

output :: Seq.AllowOutput mode
             => Seq.T mode
             -> Event.T
             -> IO Word   -- ^ the number of remaining events (or bytes?)
output (Seq.Cons h) e =
  Event.with e $ \p -> checkResult "output" =<< snd_seq_event_output h p

foreign import ccall safe "alsa/asoundlib.h snd_seq_event_output"
  snd_seq_event_output :: Ptr Seq.Core -> Ptr Event.T -> IO C.CInt



-- | Output an event without draining the buffer.
-- Throws @-EAGAIN@ if the buffer becomes full.
-- See also 'output'.

outputBuffer :: Seq.AllowOutput mode
                    => Seq.T mode
                    -> Event.T
                    -> IO Word  -- ^ the byte size of remaining events

outputBuffer (Seq.Cons h) e =
  Event.with e $ \p -> checkResult "outputBuffer" =<< snd_seq_event_output_buffer h p

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_event_output_buffer"
  snd_seq_event_output_buffer :: Ptr Seq.Core -> Ptr Event.T -> IO C.CInt


-- | Output an event directly to the sequencer, NOT through the output buffer.
-- If an error occurs, then we throw an exception.
-- See also 'output'.

outputDirect
  :: Seq.AllowOutput mode
  => Seq.T mode
  -> Event.T
  -> IO Word  -- ^ number of bytes sent to the sequencer

outputDirect (Seq.Cons h) e =
  Event.with e $ \p -> checkResult "outputDirect" =<< snd_seq_event_output_direct h p

foreign import ccall safe "alsa/asoundlib.h snd_seq_event_output_direct"
  snd_seq_event_output_direct :: Ptr Seq.Core -> Ptr Event.T -> IO C.CInt


-- | Return the size (in bytes) of pending events on output buffer.
-- See also 'output'.
outputPending
  :: Seq.AllowOutput mode
  => Seq.T mode
  -> IO Word  -- ^ size of pending events (in bytes)
outputPending (Seq.Cons h) =
  fromIntegral `fmap` snd_seq_event_output_pending h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_event_output_pending"
  snd_seq_event_output_pending :: Ptr Seq.Core -> IO C.CInt


-- | Extract the first event in output buffer.
-- Throws @(Errno 2)@ exception if output buffer is empty.
-- See also 'output'.
extractOutput
  :: Seq.AllowOutput mode
  => Seq.T mode
  -> IO Event.T   -- ^ the first event in the buffer (if one was present)
extractOutput (Seq.Cons h) =
  alloca $ \p -> do Exc.checkResult_ "extractOutput" =<< snd_seq_extract_output h p
                    peek =<< peek p

-- | Remove the first event in output buffer.
-- Throws an exception on error.
-- See also 'output'.
removeOutput :: Seq.AllowOutput mode
  => Seq.T mode -> IO ()
removeOutput (Seq.Cons h) = Exc.checkResult_ "removeOutput" =<< snd_seq_extract_output h nullPtr

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_extract_output"
  snd_seq_extract_output :: Ptr Seq.Core -> Ptr (Ptr Event.T) -> IO C.CInt


-- | Drain output buffer to sequencer.
-- This function drains all pending events on the output buffer.
-- The function returns immediately after the events are sent to the queues
-- regardless whether the events are processed or not.
-- To get synchronization with the all event processes,
-- use 'syncOutputQueue' after calling this function.
-- Throws an exception on error.
-- See also: 'output', 'syncOutputQueue'.

drainOutput
  :: Seq.AllowOutput mode
  => Seq.T mode
  -> IO Word -- ^ byte size of events remaining in the buffer.

drainOutput (Seq.Cons h) = checkResult "drainOutput" =<< snd_seq_drain_output h

foreign import ccall safe "alsa/asoundlib.h snd_seq_drain_output"
  snd_seq_drain_output :: Ptr Seq.Core -> IO C.CInt


-- | Remove events from both the user-space output buffer,
-- and the kernel-space sequencer queue.
-- See also: 'drainOutput', 'dropOutputBuffer', 'removeOutput'.
dropOutput
  :: Seq.AllowOutput mode
  => Seq.T mode -> IO ()
dropOutput (Seq.Cons h) = Exc.checkResult_ "dropOutput" =<< snd_seq_drop_output h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_drop_output"
  snd_seq_drop_output :: Ptr Seq.Core -> IO C.CInt


-- | Remove events from the user-space output buffer.
-- See also: 'dropOutput'.
dropOutputBuffer
  :: Seq.AllowOutput mode
  => Seq.T mode -> IO ()
dropOutputBuffer (Seq.Cons h) = Exc.checkResult_ "dropOutputBuffer" =<< snd_seq_drop_output_buffer h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_drop_output_buffer"
  snd_seq_drop_output_buffer :: Ptr Seq.Core -> IO C.CInt


-- | Remove events from both the user-space input buffer,
-- and the kernel-space sequencer queue.
-- See also: 'dropInputBuffer', 'removeOutput'.
dropInput
  :: Seq.AllowInput mode
  => Seq.T mode -> IO ()
dropInput (Seq.Cons h) = Exc.checkResult_ "dropInput" =<< snd_seq_drop_input h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_drop_input"
  snd_seq_drop_input :: Ptr Seq.Core -> IO C.CInt


-- | Remove events from the user-space input buffer.
-- See also: 'dropInput'.
dropInputBuffer
  :: Seq.AllowInput mode
  => Seq.T mode -> IO ()
dropInputBuffer (Seq.Cons h) = Exc.checkResult_ "dropInputBuffer" =<< snd_seq_drop_input_buffer h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_drop_input_buffer"
  snd_seq_drop_input_buffer :: Ptr Seq.Core -> IO C.CInt



-- | Make a note whose unspecified fields contain 0.
simpleNote :: Channel -> Pitch -> Velocity -> Event.Note
simpleNote c n v =
   Event.Note {
      Event.noteChannel = c,
      Event.noteNote = n,
      Event.noteVelocity = v,
      Event.noteOffVelocity = offVelocity,
      Event.noteDuration = Duration 0
   }

normalVelocity, offVelocity :: Velocity
normalVelocity = Velocity 64
offVelocity = Velocity 0

{- |
Construct an ALSA sequencer event from very few information.
Most fields are initialized with sensible defaults.
You may use this as a start and alter its fields for your special needs.

> (Event.simple myAddr (Event.simpleNote (Event.Channel 0) (Event.Pitch 60) Event.normalVelocity)) {Event.dest = destAddr}
-}
simple :: Addr.T -> Event.Data -> Event.T
simple src bdy = Cons
  { Event.highPriority = False
  , Event.tag = Tag 0
  , Event.queue = Queue.direct
  , Event.time = Time.consAbs $ Time.Tick 0
  , Event.source = src
  , Event.dest = Addr.subscribers
  , Event.body = bdy
  }

forSourcePort :: Port.T -> Event.Data -> Event.T
forSourcePort port =
   simple (Addr.Cons Client.unknown port)

forConnection :: Connect.T -> Event.Data -> Event.T
forConnection (Connect.Cons src dst) bdy =
   (simple src bdy) { Event.dest = dst }


customZero :: Event.Custom
customZero = Event.Custom 0 0 0


{- available in alsa-1.0.14, but gone in 1.0.22
-- | Used for volume control: means do not change the volume.
volumeSame :: Volume
volumeSame = Volume (-1)
-}
