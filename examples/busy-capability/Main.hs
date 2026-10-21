{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef, writeIORef)

-- The arguments give the machine two capabilities. The main thread wakes
-- the child on its own capability and then computes. It does not schedule
-- until the child sets the flag. Thus the child must go to the idle
-- capability while the main thread runs. See "Thread movement" in
-- docs/multi-threaded-rts-design.md.

-- Allocate until the flag is set. Each round keeps a new list, so the loop
-- fails its heap check from time to time.
spin :: IORef Bool -> IORef [Int] -> Int -> IO ()
spin flag sink = go
  where
    go !count = do
      done <- readIORef flag
      if done
        then return ()
        else do
          writeIORef sink (replicate 64 count)
          go (count + 1)

main :: IO ()
main = do
  flag <- newIORef False
  sink <- newIORef []
  ready <- newEmptyMVar
  gate <- newEmptyMVar
  _ <- forkIO $ do
    putMVar ready ()
    takeMVar gate
    atomicWriteIORef flag True
  -- The child runs on this capability and waits for the gate.
  takeMVar ready
  putMVar gate ()
  spin flag sink 0
  putStrLn "the child ran"
