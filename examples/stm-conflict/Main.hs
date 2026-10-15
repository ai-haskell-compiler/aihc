module Main (main) where

import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_)
import GHC.Conc (atomically, newTVarIO, orElse, readTVar, readTVarIO, retry, unsafeIOToSTM, writeTVar)

-- Two threads add to one counter. Each transaction reads the counter, lets
-- the other thread run, and then writes. A transaction whose read no longer
-- holds at its commit runs again, so no addition is lost. See "STM" in
-- docs/multi-threaded-rts-design.md.

main :: IO ()
main = do
  counter <- newTVarIO (0 :: Int)
  done <- newEmptyMVar
  let add = atomically $ do
        value <- readTVar counter
        unsafeIOToSTM yield
        writeTVar counter (value + 1)
  forM_ [1 .. 2 :: Int] $ \_ -> forkIO (replicateM_ 100 add >> putMVar done ())
  replicateM_ 2 (takeMVar done)
  readTVarIO counter >>= print
  -- A transaction reads its own writes, and orElse drops the writes of a
  -- branch that retries.
  result <- atomically $ do
    writeTVar counter 1
    (writeTVar counter 2 >> retry) `orElse` return ()
    readTVar counter
  print result
