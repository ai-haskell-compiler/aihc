module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, when)

-- The arguments give the machine eight capabilities. Threads on several
-- capabilities fork at the same time, and each child must run on the
-- stack that its fork made. See "Workers" in
-- docs/multi-threaded-rts-design.md.

workers :: Int
workers = 8

children :: Int
children = 4000

-- A worker forks its children and waits until each child has run.
worker :: IO Int
worker = do
  count <- newMVar (0 :: Int)
  finished <- newEmptyMVar
  forM_ [1 .. children] $ \_ ->
    forkIO $ do
      seen <- modifyMVar count (\value -> return (value + 1, value + 1))
      when (seen == children) (putMVar finished ())
  takeMVar finished
  return children

main :: IO ()
main = do
  dones <- forM [1 .. workers] $ \_ -> do
    done <- newEmptyMVar
    _ <- forkIO (worker >>= putMVar done)
    return done
  totals <- mapM takeMVar dones
  print (sum totals)
