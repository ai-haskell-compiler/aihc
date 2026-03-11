{-| PRIVATE MODULE
Reference:
<http://www.alsa-project.org/alsa-doc/alsa-lib/group___sequencer.html>
-}

{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Sequencer where

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.C.Types (CSize, )
import Foreign.C.String (CString, withCAString, peekCString, )
import Foreign.Ptr (Ptr, )
import Foreign.Marshal.Alloc (alloca, )
import Foreign.Storable (peek, )

import Data.Word (Word, )

import Control.Exception (bracket, )


-- | Creates a new handle and opens a connection to the kernel sequencer
-- interface. After a client is created successfully,
-- a 'ClientStart' event is broadcast to the announce port.
-- May throw an exception.
-- See also: 'open_lconf', 'close', 'get_seq_type',
--   'get_seq_name', 'set_blocking', 'get_client_id'.

open
  :: Seq.OpenMode mode
        -- Read\/Write permissions
  => String
        {- ^
        The sequencer's \"name\".
        This is not a name that you make up for your own purposes;
        it has special significance to the ALSA library.
        Usually you need to pass 'defaultName' here
        or simply use 'openDefault'.
        -}
  -> Seq.BlockMode    -- ^ Blocking behavior
  -> IO (Seq.T mode)  -- ^ Handle to the sequencer.

open t bm = withOpenMode $ \om -> alloca $ \p -> withCAString t $ \s ->
  do Exc.checkResult_ "open" =<< snd_seq_open p s (Seq.expOpenMode om) (Seq.expBlockMode bm)
     fmap Seq.Cons $ peek p

openDefault
  :: Seq.OpenMode mode
        -- Read\/Write permissions
  => Seq.BlockMode    -- ^ Blocking behavior
  -> IO (Seq.T mode)  -- ^ Handle to the sequencer.
openDefault = open defaultName


foreign import ccall unsafe "alsa/asoundlib.h snd_seq_open"
  snd_seq_open :: Ptr (Ptr Seq.Core) -> CString -> C.CInt -> C.CInt -> IO C.CInt

withOpenMode :: (mode -> IO (Seq.T mode)) -> IO (Seq.T mode)
withOpenMode f = f undefined


-- | Close the sequencer. Closes the sequencer client and releases its
-- resources. After a client is closed, an event with 'ClientExit' is
-- broadcast to announce port. The connection between other clients are
-- disconnected. Call this just before exiting your program.
-- NOTE: we could put this in a finalizer for the handle?

close
  :: Seq.T mode   -- ^ handle to the sequencer
  -> IO ()
close (Seq.Cons h) = Exc.checkResult_ "close" =<< snd_seq_close h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_close"
  snd_seq_close :: Ptr Seq.Core -> IO C.CInt


with
  :: Seq.OpenMode mode
        -- Read\/Write permissions
  => String
        {- ^
        The sequencer's \"name\".
        This is not a name that you make up for your own purposes;
        it has special significance to the ALSA library.
        Usually you need to pass 'defaultName' here
        or simply use 'withDefault'.
        -}
  -> Seq.BlockMode
        -- ^ Blocking behavior
  -> (Seq.T mode -> IO a)
        -- ^ Action on the sequencer, the result must be computed strictly.
  -> IO a
with t bm =
   bracket (open t bm) close

withDefault
  :: Seq.OpenMode mode
  => Seq.BlockMode
  -> (Seq.T mode -> IO a)
  -> IO a
withDefault = with defaultName


-- | This is the name that should be passed to 'open' in most cases.
defaultName :: String
defaultName = "default"


-- | Get identifier of a sequencer handle.
-- It is the same identifier specified in the call to 'open'.
getName
  :: Seq.T mode  -- ^ sequencer handle
  -> IO String   -- ^ ALSA identifier for the handle
getName (Seq.Cons h) = peekCString =<< snd_seq_name h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_name"
  snd_seq_name :: Ptr Seq.Core -> IO CString


-- | Change the blocking mode of the given client.
-- In block mode, the client falls into sleep when it fills the output
-- pool with events, or when it demands events from an empty input pool.
-- memory pool with full events. Clients that are sleeping due to
-- loack of space in the output pool are woken when a certain
-- amount of free space becomes available (see 'set_output_room').
setBlocking
  :: Seq.T mode     -- ^ sequencer handle
  -> Seq.BlockMode  -- ^ blocking mode
  -> IO ()
setBlocking (Seq.Cons h) m = Exc.checkResult_ "set_blocking" =<< snd_seq_nonblock h(Seq.expBlockMode m)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_nonblock"
  snd_seq_nonblock :: Ptr Seq.Core -> C.CInt -> IO C.CInt



-- Buffers ---------------------------------------------------------------------

-- | Return the byte size of the output buffer.
getOutputBufferSize
  :: Seq.T mode   -- ^ Sequencer handle.
  -> IO Word  -- ^ Size of output buffer in bytes.

getOutputBufferSize (Seq.Cons h) =
  fromIntegral `fmap` snd_seq_get_output_buffer_size h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_get_output_buffer_size"
  snd_seq_get_output_buffer_size :: Ptr Seq.Core -> IO CSize


-- | Resize of the output buffer.
-- This function clears all output events (see 'drop_output').
setOutputBufferSize
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Word     -- ^ New buffer size in bytes.
  -> IO ()

setOutputBufferSize (Seq.Cons h) x =
  Exc.checkResult_ "set_output_buffer_size" =<< snd_seq_set_output_buffer_size h (fromIntegral x)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_output_buffer_size"
  snd_seq_set_output_buffer_size :: Ptr Seq.Core -> CSize -> IO C.CInt


-- | Return the byte size of input buffer.
getInputBufferSize
  :: Seq.T mode   -- ^ Sequencer handle.
  -> IO Word  -- ^ Size of input buffer in bytes.

getInputBufferSize (Seq.Cons h) =
  fromIntegral `fmap` snd_seq_get_input_buffer_size h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_get_input_buffer_size"
   snd_seq_get_input_buffer_size :: Ptr Seq.Core -> IO CSize


-- | Resize the input buffer.
-- This function clears all input events (see 'drop_input').
setInputBufferSize
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Word     -- ^ New byffer size in bytes.
  -> IO ()

setInputBufferSize (Seq.Cons h) x =
  Exc.checkResult_ "set_input_buffer_size" =<< snd_seq_set_input_buffer_size h (fromIntegral x)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_input_buffer_size"
  snd_seq_set_input_buffer_size :: Ptr Seq.Core -> CSize -> IO C.CInt



-- Pool management -------------------------------------------------------------

-- | Resize the output memory pool.
setPoolOutput
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Word     -- ^ New size in bytes.
  -> IO ()

setPoolOutput (Seq.Cons h) x =
  Exc.checkResult_ "set_pool_output" =<< snd_seq_set_client_pool_output h (fromIntegral x)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_client_pool_output"
  snd_seq_set_client_pool_output :: Ptr Seq.Core -> CSize -> IO C.CInt


-- | Specify how much space should become free before waking clients
-- that are blocked due to a lack of space in the output pool.
setPoolOutputRoom
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Word     -- ^ Number of bytes need to wake up.
  -> IO ()

setPoolOutputRoom (Seq.Cons h) x =
  Exc.checkResult_ "set_pool_output_room" =<< snd_seq_set_client_pool_output_room h (fromIntegral x)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_client_pool_output_room"
  snd_seq_set_client_pool_output_room :: Ptr Seq.Core -> CSize -> IO C.CInt


-- | Reset the output pool.
resetPoolOutput
  :: Seq.T mode   -- ^ Sequencer handle.
  -> IO ()

resetPoolOutput (Seq.Cons h) =
  Exc.checkResult_ "reset_pool_output" =<< snd_seq_reset_pool_output h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_reset_pool_output"
  snd_seq_reset_pool_output :: Ptr Seq.Core -> IO C.CInt



-- | Resize the input memory pool.
setPoolInput
  :: Seq.T mode   -- ^ Sequencer handle.
  -> Word     -- ^ New size in bytes.
  -> IO ()

setPoolInput (Seq.Cons h) x =
  Exc.checkResult_ "set_pool_input" =<< snd_seq_set_client_pool_input h (fromIntegral x)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_set_client_pool_input"
  snd_seq_set_client_pool_input :: Ptr Seq.Core -> CSize -> IO C.CInt


-- | Reset the input pool.
resetPoolInput
  :: Seq.T mode   -- ^ Sequencer handle.
  -> IO ()

resetPoolInput (Seq.Cons h) =
  Exc.checkResult_ "reset_pool_input" =<< snd_seq_reset_pool_input h

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_reset_pool_input"
  snd_seq_reset_pool_input :: Ptr Seq.Core -> IO C.CInt
