module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (BlockedIndefinitelyOnSTM (..), try)
import Control.Monad (forM, forM_, unless)
import GHC.Conc (atomically, newTVarIO, readTVar, retry, writeTVar)

-- A transaction that retries waits until another transaction commits. The
-- arguments give the machine four capabilities, so the waiting threads and
-- the writer can run at the same time. See "STM" in
-- docs/multi-threaded-rts-design.md.

main :: IO ()
main = do
  counter <- newTVarIO (0 :: Int)
  dones <- forM [1 .. 8 :: Int] $ \goal -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      atomically $ do
        value <- readTVar counter
        unless (value >= goal) retry
      putMVar done goal
    return done
  -- The waiting threads usually retry before the first write.
  threadDelay 50000
  forM_ [1 .. 8 :: Int] $ \_ ->
    atomically (readTVar counter >>= writeTVar counter . (+ 1))
  mapM takeMVar dones >>= print
  -- No other thread can write this variable, so the wait never ends.
  never <- newTVarIO False
  outcome <- try (atomically (readTVar never >>= \ready -> unless ready retry))
  case outcome of
    Left BlockedIndefinitelyOnSTM -> putStrLn "blocked indefinitely"
    Right () -> putStrLn "the variable changed"
