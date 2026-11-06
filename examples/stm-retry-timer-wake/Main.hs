module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM)
import GHC.Conc (atomically, newTVarIO, readTVar, retry, writeTVar)

-- A transaction that retries waits until another transaction commits. The
-- program has one capability, and one thread sleeps for a long time, so an
-- STM timer is pending during each wait. Each hand-off forks a thread that
-- writes a variable, and the main thread retries until it sees the value.
-- The commit of the forked thread must end the wait of the main thread.
-- The wait must not continue until the timer of the sleeping thread.

handOff :: Int -> IO Int
handOff value = do
  box <- newTVarIO Nothing
  _ <- forkIO (atomically (writeTVar box (Just value)))
  atomically (readTVar box >>= maybe retry pure)

main :: IO ()
main = do
  _ <- forkIO (threadDelay 60000000)
  values <- forM [1 .. 100] handOff
  print (sum values)
