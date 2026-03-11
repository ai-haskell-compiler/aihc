{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
'T.Text' variant of the tracing functions in "Debug.Trace".
-}
module Debug.Trace.Text
  ( -- * Eventlog tracing
  -- $eventlog_tracing
    traceEvent
  , traceEventIO

  -- * Execution phase markers
  -- $markers
  , traceMarker
  , traceMarkerIO
  ) where
import Control.Monad (when)
import Foreign.C.String (CString)
import GHC.Exts (Ptr(..), traceEvent#, traceMarker#)
import GHC.IO (IO(..))
import qualified System.IO.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Debug.Trace.Flags (userTracingEnabled)

-- $eventlog_tracing
--
-- Eventlog tracing is a performance profiling system. These functions emit
-- extra events into the eventlog. In combination with eventlog profiling
-- tools these functions can be used for monitoring execution and
-- investigating performance problems.

-- | 'T.Text' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEvent :: T.Text -> a -> a
traceEvent message a
  | userTracingEnabled = traceEvent' message a
  | otherwise = a

traceEvent' :: T.Text -> a -> a
traceEvent' message a = Unsafe.unsafeDupablePerformIO $ do
  traceEventIO' message
  return a
{-# NOINLINE traceEvent' #-}

-- | 'T.Text' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEventIO :: T.Text -> IO ()
traceEventIO message = when userTracingEnabled $ traceEventIO' message

traceEventIO' :: T.Text -> IO ()
traceEventIO' message = withCString message $ \(Ptr p) -> IO $ \s ->
  case traceEvent# p s of
    s' -> (# s', () #)

-- $markers
--
-- When looking at a profile for the execution of a program we often want to
-- be able to mark certain points or phases in the execution and see that
-- visually in the profile.
--
-- For example, a program might have several distinct phases with different
-- performance or resource behaviour in each phase. To properly interpret the
-- profile graph we really want to see when each phase starts and ends.
--
-- Markers let us do this: we can annotate the program to emit a marker at
-- an appropriate point during execution and then see that in a profile.

-- | 'T.Text' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarker :: T.Text -> a -> a
traceMarker message a
  | userTracingEnabled = traceMarker' message a
  | otherwise = a

traceMarker' :: T.Text -> a -> a
traceMarker' message a = Unsafe.unsafeDupablePerformIO $ do
  traceMarkerIO' message
  return a
{-# NOINLINE traceMarker' #-}

-- | 'T.Text' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarkerIO :: T.Text -> IO ()
traceMarkerIO message = when userTracingEnabled $ traceMarkerIO' message

traceMarkerIO' :: T.Text -> IO ()
traceMarkerIO' message = withCString message $ \(Ptr p) -> IO $ \s ->
  case traceMarker# p s of
    s' -> (# s', () #)

withCString :: T.Text -> (CString -> IO a) -> IO a
withCString text = B.useAsCString (TE.encodeUtf8 text)
