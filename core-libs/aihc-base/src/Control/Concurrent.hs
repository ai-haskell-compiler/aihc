module Control.Concurrent
  ( MVar,
    ThreadId,
    forkFinally,
    forkIO,
    forkOS,
    killThread,
    myThreadId,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
    throwTo,
    yield,
  )
where

import Control.Exception.Base (SomeException, mask, try)
import GHC.Conc.IO (threadDelay)
import GHC.Conc.Sync (ThreadId, forkIO, killThread, myThreadId, throwTo, yield)
import GHC.Exception (ErrorCall (..))
import GHC.IO (throwIO)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Prelude (Either, IO, ($), (>>=))

-- | Run the action in a child thread. Give its result to the callback.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action callback =
  mask $ \restore -> forkIO (try (restore action) >>= callback)

-- | Bound threads are not supported. The stub does not run the action.
forkOS :: IO () -> IO ThreadId
forkOS _ = throwIO (ErrorCallWithLocation "forkOS: bound threads are not supported" "")
