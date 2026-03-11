{-# LANGUAGE ForeignFunctionInterface #-}

#include "jack/midiport.h"
#include <Sound/JACK/Common.h>

module Sound.JACK.FFI.MIDI
  (
    EventBuffer(EventBuffer),
    Buffer(Buffer),

    RawEvent(RawEvent),
    time,
    buffer,

    toRawEventFunction,

    get_event_count,
    event_get,
    clear_buffer,
    event_reserve,
    event_write,

    -- internal
    withByteStringPtr,
  )

where

import Sound.JACK.FFI (NFrames(NFrames), Input, Output, )

import Foreign.Marshal.Array (copyArray, advancePtr, )
import Foreign.ForeignPtr (withForeignPtr, )
import Foreign.Ptr (Ptr, castPtr, )
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf, alignment, peek, poke)
import qualified Foreign.C.Error as E
import qualified Foreign.C.Types as C
import Data.Word (Word8, )

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Parser.Report as Report

import System.IO (hPutStrLn, stderr, )


-- could also be an empty data declaration
data EventBuffer = EventBuffer

newtype Buffer dir = Buffer (Ptr EventBuffer)

_dummy :: Buffer dir
_dummy = Buffer undefined


-- | Represents a raw JACK MIDI event
data RawEvent = RawEvent {
      time   :: NFrames      -- ^ Sample index at which event is valid (relative to cycle start)
    , buffer :: B.ByteString -- ^ Raw MIDI data
} deriving (Eq, Ord)

-- | Converts high level MIDI Event transformation functions into raw MIDI Event transformation functions
toRawEventFunction ::
   (NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T))
        -- ^ transforms Sound.MIDI.File.Event
   ->
   (NFrames -> RawEvent -> IO RawEvent)
        -- ^ transforms Sound.JACK.MIDI.RawEvent
toRawEventFunction fun cycleStart rawEvent =
   case Msg.maybeFromByteString $ BL.fromChunks [buffer rawEvent] of
     Report.Cons warnings result -> do
        mapM_ (hPutStrLn stderr) warnings
        case result of
           Left _ -> do
              putStrLn $ "Warning: Did not understand Event: " ++ show rawEvent
              return rawEvent
           Right e -> do
              (time_, event_) <- fun cycleStart (time rawEvent, e)
              return $ RawEvent time_ $ B.concat $
                 BL.toChunks $ Msg.toByteString event_

instance Storable RawEvent where
    sizeOf    _ = #{size jack_midi_event_t}

    alignment _ = #{alignment jack_midi_event_t}

    peek pointer = do
        time_ <- #{peek jack_midi_event_t, time} pointer
        size_ <- #{peek jack_midi_event_t, size} pointer

        bufferPtr <- #{peek jack_midi_event_t, buffer} pointer
        let sizeInt = fromIntegral (size_ :: C.CSize)
        buffer_ <-
            BI.create sizeInt $ \dest ->
                copyArray dest (castPtr bufferPtr) sizeInt

        return $ RawEvent (NFrames time_) buffer_

    {- |
    This implementation expects that port buffer pointer is already initialized.
    This is dangerous, but currently we do not know, how to do it better.
    -}
    poke pointer (RawEvent (NFrames time_) buffer_) = do
        #{poke jack_midi_event_t, time} pointer time_
        #{poke jack_midi_event_t, size} pointer
           (fromIntegral $ B.length buffer_ :: C.CSize)

        bufferPtr <- #{peek jack_midi_event_t, buffer} pointer
        withByteStringPtr buffer_ $ \ptr len ->
            copyArray (castPtr bufferPtr) ptr len

        #{poke jack_midi_event_t, buffer} pointer bufferPtr

withByteStringPtr :: B.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteStringPtr bstr act =
    case BI.toForeignPtr bstr of
        (fptr, start, len) ->
            withForeignPtr fptr $ \ptr ->
                act (advancePtr ptr start) len


instance Show RawEvent where
    show rawEvent =
       "MIDIEvent @ " ++ show (time rawEvent) ++
       "\t: " ++ showEvent (buffer rawEvent)

showEvent :: B.ByteString -> String
showEvent buffer_ =
   case Msg.maybeFromByteString $ BL.fromChunks [buffer_] of
      Report.Cons warnings result ->
         unlines warnings ++
         case result of
            Left  errMsg -> "Warning: " ++ errMsg
            Right e ->
               case e of
                  Msg.Channel b -> "MidiMsg.Channel " ++ show b
                  Msg.System  _ -> "MidiMsg.System ..."



foreign import ccall "static jack/midiport.h jack_midi_get_event_count"
  get_event_count :: Buffer Input -> IO NFrames

foreign import ccall "static jack/midiport.h jack_midi_event_get"
  event_get :: Ptr RawEvent -> Buffer Input -> NFrames -> IO E.Errno

foreign import ccall "static jack/midiport.h jack_midi_clear_buffer"
  clear_buffer :: Buffer Output -> IO ()

-- nullPtr may be mapped to eNOBUFS exception as in event_write
foreign import ccall "static jack/midiport.h jack_midi_event_reserve"
  event_reserve :: Buffer Output -> NFrames -> C.CSize -> IO (Ptr Word8)

foreign import ccall "static jack/midiport.h jack_midi_event_write"
  event_write :: Buffer Output -> NFrames -> Ptr Word8 -> C.CULong -> IO E.Errno
