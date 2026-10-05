module Control.Concurrent
  ( module Control.Concurrent.MVar,
    ThreadId,
    forkFinally,
    forkIO,
    forkOS,
    getNumCapabilities,
    killThread,
    myThreadId,
    rtsSupportsBoundThreads,
    threadDelay,
    threadWaitRead,
    threadWaitReadSTM,
    threadWaitWrite,
    threadWaitWriteSTM,
    throwTo,
    yield,
  )
where

import Control.Concurrent.MVar
import Control.Exception.Base (SomeException, mask, try)
import GHC.Conc.IO (threadDelay, threadWaitRead, threadWaitReadSTM, threadWaitWrite, threadWaitWriteSTM)
import GHC.Conc.Sync (ThreadId, forkIO, getNumCapabilities, killThread, myThreadId, throwTo, yield)
import GHC.Exception (ErrorCall (..))
import GHC.IO (throwIO)
import Prelude (Bool (..), Either, IO, ($), (>>=))

-- | Run the action in a child thread. Give its result to the callback.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action callback =
  mask $ \restore -> forkIO (try (restore action) >>= callback)

-- | Bound threads are not supported. The stub does not run the action.
forkOS :: IO () -> IO ThreadId
forkOS _ = throwIO (ErrorCallWithLocation "forkOS: bound threads are not supported" "")

-- | Whether the runtime supports bound threads.
--
-- The aihc runtime runs all Haskell threads on one OS thread, so the answer
-- is 'False'. 'forkOS' fails for the same reason.
rtsSupportsBoundThreads :: Bool
rtsSupportsBoundThreads = False
