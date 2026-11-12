module Main where

import Control.Concurrent (ThreadId, forkIO, myThreadId, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import GHC.Conc (BlockReason (..), ThreadStatus (..), atomically, newTVarIO, readTVar, retry, threadStatus, writeTVar)

-- | Read the status until it agrees with the predicate. The bound stops the
-- loop, and the last status then shows the defect.
awaitStatus :: (ThreadStatus -> Bool) -> ThreadId -> IO ThreadStatus
awaitStatus wanted thread = go (100000 :: Int)
  where
    go remaining = do
      status <- threadStatus thread
      if wanted status || remaining == 0
        then pure status
        else yield >> go (remaining - 1)

main :: IO ()
main = do
  self <- myThreadId
  threadStatus self >>= print
  gate <- newEmptyMVar
  done <- newEmptyMVar
  waiter <- forkIO (takeMVar gate >> putMVar done ())
  awaitStatus (/= ThreadRunning) waiter >>= print
  putMVar gate ()
  takeMVar done
  awaitStatus (== ThreadFinished) waiter >>= print
  flag <- newTVarIO False
  transaction <- forkIO (atomically (readTVar flag >>= \value -> if value then pure () else retry))
  awaitStatus (/= ThreadRunning) transaction >>= print
  atomically (writeTVar flag True)
  awaitStatus (== ThreadFinished) transaction >>= print
