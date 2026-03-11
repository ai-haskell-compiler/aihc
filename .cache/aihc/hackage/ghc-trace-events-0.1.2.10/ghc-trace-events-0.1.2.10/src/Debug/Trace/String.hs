{-|
Drop-in replacement for the event tracing functions in "Debug.Trace" but are
faster when user tracing is disabled.
-}
module Debug.Trace.String
  ( -- * Eventlog tracing
    -- $eventlog_tracing
    traceEvent
  , traceEventIO

  -- * Execution phase markers
  -- $markers
  , traceMarker
  , traceMarkerIO
  ) where
import Control.Monad
import qualified Debug.Trace as Base

import Debug.Trace.Flags (userTracingEnabled)

-- $eventlog_tracing
--
-- Eventlog tracing is a performance profiling system. These functions emit
-- extra events into the eventlog. In combination with eventlog profiling
-- tools these functions can be used for monitoring execution and
-- investigating performance problems.

-- | Drop-in replacement for 'Debug.Trace.traceEvent' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEvent :: String -> a -> a
traceEvent message a
  | userTracingEnabled = Base.traceEvent message a
  | otherwise = a

-- | Drop-in replacement for 'Debug.Trace.traceEventIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEventIO :: String -> IO ()
traceEventIO message = when userTracingEnabled $ Base.traceEventIO message

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

-- | Drop-in replacement for 'Debug.Trace.traceMarker' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarker :: String -> a -> a
traceMarker message a
  | userTracingEnabled = Base.traceMarker message a
  | otherwise = a

-- | Drop-in replacement for 'Debug.Trace.traceMarkerIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarkerIO :: String -> IO ()
traceMarkerIO message = when userTracingEnabled $ Base.traceMarkerIO message
