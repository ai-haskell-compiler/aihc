module Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    forkOS,
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

import GHC.Conc.IO (threadDelay)
import GHC.Conc.Sync (ThreadId, forkIO, myThreadId, throwTo, yield)
import GHC.Exception (ErrorCall (..))
import GHC.IO (throwIO)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Prelude (IO)

-- | Bound threads are not supported. The stub does not run the action.
forkOS :: IO () -> IO ThreadId
forkOS _ = throwIO (ErrorCallWithLocation "forkOS: bound threads are not supported" "")
