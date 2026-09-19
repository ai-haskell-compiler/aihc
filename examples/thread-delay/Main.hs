module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)

-- | A delayed thread suspends only itself: the runtime keeps running every
-- other green thread while it waits. Forking only enqueues the worker, so it
-- has not run when main reaches the delay, and it has run once the delay is
-- over. The worker performs no IO of its own, because the WASI host carries
-- one operation at a time and would order it against the timer.
main :: IO ()
main = do
  ran <- newIORef False
  _ <- forkIO (writeIORef ran True)
  putStrLn "main: delaying"
  threadDelay 100000
  finished <- readIORef ran
  putStrLn ("main: the worker ran while main waited: " ++ show finished)
