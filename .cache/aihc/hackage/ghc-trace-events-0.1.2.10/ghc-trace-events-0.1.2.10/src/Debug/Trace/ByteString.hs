{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
'B.ByteString' variant of the tracing functions in "Debug.Trace".
-}
module Debug.Trace.ByteString
  ( -- * Eventlog tracing
    -- $eventlog_tracing
    traceEvent
  , traceEventIO

  , unsafeTraceEvent
  , unsafeTraceEventIO

  -- * Execution phase markers
  -- $markers
  , traceMarker
  , traceMarkerIO

  , unsafeTraceMarker
  , unsafeTraceMarkerIO
  ) where
import Control.Monad (when)
import GHC.Exts (Ptr(..), traceEvent#, traceMarker#)
import GHC.IO (IO(..))
import qualified System.IO.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace.Flags (userTracingEnabled)

-- $eventlog_tracing
--
-- Eventlog tracing is a performance profiling system. These functions emit
-- extra events into the eventlog. In combination with eventlog profiling
-- tools these functions can be used for monitoring execution and
-- investigating performance problems.

-- | 'B.ByteString' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEvent :: B.ByteString -> a -> a
traceEvent message a
  | userTracingEnabled = traceEvent' message a
  | otherwise = a

traceEvent' :: B.ByteString -> a -> a
traceEvent' message a = Unsafe.unsafeDupablePerformIO $ do
  traceEventIO' message
  return a
{-# NOINLINE traceEvent' #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEventIO :: B.ByteString -> IO ()
traceEventIO message = when userTracingEnabled $ traceEventIO' message

traceEventIO' :: B.ByteString -> IO ()
traceEventIO' message = B.useAsCString message $ \(Ptr p) -> IO $ \s ->
  case traceEvent# p s of
    s' -> (# s', () #)

-- | 'B.ByteString' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
unsafeTraceEvent :: B.ByteString -> a -> a
unsafeTraceEvent message a
  | userTracingEnabled = unsafeTraceEvent' message a
  | otherwise = a

unsafeTraceEvent' :: B.ByteString -> a -> a
unsafeTraceEvent' message a = Unsafe.unsafeDupablePerformIO $ do
  unsafeTraceEventIO' message
  return a
{-# NOINLINE unsafeTraceEvent' #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
unsafeTraceEventIO :: B.ByteString -> IO ()
unsafeTraceEventIO message =
  when userTracingEnabled $ unsafeTraceEventIO' message

unsafeTraceEventIO' :: B.ByteString -> IO ()
unsafeTraceEventIO' message =
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
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

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarker :: B.ByteString -> a -> a
traceMarker message a
  | userTracingEnabled = traceMarker' message a
  | otherwise = a

traceMarker' :: B.ByteString -> a -> a
traceMarker' message a = Unsafe.unsafeDupablePerformIO $ do
  traceMarkerIO' message
  return a
{-# NOINLINE traceMarker' #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarkerIO :: B.ByteString -> IO ()
traceMarkerIO message = when userTracingEnabled $ traceMarkerIO' message

traceMarkerIO' :: B.ByteString -> IO ()
traceMarkerIO' message = B.useAsCString message $ \(Ptr p) -> IO $ \s ->
  case traceMarker# p s of
    s' -> (# s', () #)

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
unsafeTraceMarker :: B.ByteString -> a -> a
unsafeTraceMarker message a
  | userTracingEnabled = unsafeTraceMarker' message a
  | otherwise = a

unsafeTraceMarker' :: B.ByteString -> a -> a
unsafeTraceMarker' message a = Unsafe.unsafeDupablePerformIO $ do
  unsafeTraceMarkerIO' message
  return a
{-# NOINLINE unsafeTraceMarker' #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
unsafeTraceMarkerIO :: B.ByteString -> IO ()
unsafeTraceMarkerIO message =
  when userTracingEnabled $ unsafeTraceMarkerIO' message

unsafeTraceMarkerIO' :: B.ByteString -> IO ()
unsafeTraceMarkerIO' message =
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)
