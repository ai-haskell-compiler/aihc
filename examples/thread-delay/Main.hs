module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)

-- | A delayed thread suspends only itself: the runtime keeps every other
-- green thread running while it waits. The delays are far enough apart that
-- the five lines come out in one order on any scheduler.
main :: IO ()
main = do
  finished <- newEmptyMVar
  _ <- forkIO $ do
    threadDelay 50000
    putStrLn "worker: first"
    threadDelay 200000
    putStrLn "worker: second"
    putMVar finished ()
  putStrLn "main: forked"
  threadDelay 150000
  putStrLn "main: between"
  takeMVar finished
  putStrLn "main: joined"
