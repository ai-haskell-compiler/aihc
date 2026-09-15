-- | Functions for tracing and monitoring execution.
module Debug.Trace
  ( trace,
    traceId,
    traceShow,
    traceShowId,
    traceWith,
    traceShowWith,
    traceIO,
    putTraceMsg,
    traceM,
    traceShowM,
  )
where

import GHC.Base (Applicative (..), String, seq, (.))
import GHC.IO (IO)
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.StdHandles (stderr)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Show (Show (..))

-- | Write the message to the standard error stream.
traceIO :: String -> IO ()
traceIO = hPutStrLn stderr

-- | A deprecated spelling of 'traceIO'.
putTraceMsg :: String -> IO ()
putTraceMsg = traceIO

-- | Write the message to the standard error stream, then return the second
-- argument. The message is emitted when the result is forced.
trace :: String -> a -> a
trace message result = unsafePerformIO (traceIO message) `seq` result

-- | Like 'trace', but returns the message itself.
traceId :: String -> String
traceId message = trace message message

-- | Like 'trace', but takes a showable value instead of a message.
traceShow :: (Show a) => a -> b -> b
traceShow = trace . show

-- | Like 'traceShow', but returns the shown value itself.
traceShowId :: (Show a) => a -> a
traceShowId value = trace (show value) value

-- | Like 'trace', but the message is derived from the value that is traced.
traceWith :: (a -> String) -> a -> a
traceWith describe value = trace (describe value) value

-- | Like 'traceWith', but the description is shown rather than a 'String'.
traceShowWith :: (Show b) => (a -> b) -> a -> a
traceShowWith describe = traceWith (show . describe)

-- | Like 'trace', but in an arbitrary applicative; the message is emitted when
-- the action is sequenced.
traceM :: (Applicative f) => String -> f ()
traceM message = trace message (pure ())

-- | Like 'traceM', but takes a showable value instead of a message.
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show
