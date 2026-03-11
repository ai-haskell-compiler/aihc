{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
Arbitary binary object logging available for GHC 8.8 or later. Unlike the other
tracing functions 'traceBinaryEvent' takes an arbitrary 'B.ByteString' object as
opposed to a UTF-8 encoded string.
-}
module Debug.Trace.Binary
  ( -- * Binary eventlog tracing
     traceBinaryEvent
  , traceBinaryEventIO
  ) where
import Control.Monad (when)
import GHC.Exts (Ptr(..), Int(..), traceBinaryEvent#)
import GHC.IO (IO(..))
import qualified System.IO.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace.Flags (userTracingEnabled)

-- | The 'traceBinaryEvent' function behaves like
-- 'Debug.Trace.ByteString.traceEvent' but with the difference that the message
-- is a binary object rather than a UTF-8 encoded string.
--
-- It is suitable for use in pure code. In an IO context use
-- 'traceBinaryEventIO' instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceBinaryEvent'.
--
-- Also note that this function doesn't evaluate the 'B.ByteString' if user
-- tracing in evnetlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceBinaryEvent :: B.ByteString -> a -> a
traceBinaryEvent bytes a
  | userTracingEnabled = traceBinaryEvent' bytes a
  | otherwise = a

traceBinaryEvent' :: B.ByteString -> a -> a
traceBinaryEvent' bytes a = Unsafe.unsafeDupablePerformIO $ do
  traceBinaryEventIO' bytes
  return a
{-# NOINLINE traceBinaryEvent' #-}

-- | The 'traceBinaryEventIO' function emits a binary message to the eventlog,
-- if eventlog profiling is available and enabled at runtime.
--
-- Compared to 'traceBinaryEvent', 'traceBinaryEventIO' sequences the event with
-- respect to other IO actions.
--
-- Also note that this function doesn't evaluate the 'B.ByteString' if user
-- tracing in evnetlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceBinaryEventIO :: B.ByteString -> IO ()
traceBinaryEventIO bytes = when userTracingEnabled $ traceBinaryEventIO' bytes

traceBinaryEventIO' :: B.ByteString -> IO ()
traceBinaryEventIO' bytes =
  BU.unsafeUseAsCStringLen bytes $ \(Ptr p, I# n) -> IO $ \s ->
    case traceBinaryEvent# p n s of
      s' -> (# s', () #)
