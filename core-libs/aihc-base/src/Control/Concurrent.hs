module Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    myThreadId,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
    yield,
  )
where

import GHC.Conc.IO (threadDelay)
import GHC.Conc.Sync (ThreadId, forkIO, myThreadId, yield)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
