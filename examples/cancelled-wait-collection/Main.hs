-- | A wait that an asynchronous exception cancels leaves its record in the
-- queue of the MVar. The record names a frame of the thread. After the
-- thread ends, a different thread takes the stack chunk, and the address is
-- then in the middle of a frame. A collection must not scan that address.
module Main (main) where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad (forM)

-- | Wait on the MVar below @depth@ frames.
{-# NOINLINE waitDeep #-}
waitDeep :: MVar Int -> Int -> IO Int
waitDeep var 0 = takeMVar var
waitDeep var depth = do
  result <- waitDeep var (depth - 1)
  return $! result + 1

-- | One round. The second thread waits first, so its first stack chunk is
-- not the chunk of the first thread.
attempt :: Int -> IO Bool
attempt depth = do
  var <- newEmptyMVar
  start <- newEmptyMVar
  gate <- newEmptyMVar
  done <- newEmptyMVar
  _ <- forkIO $ do
    frames <- takeMVar start
    result <- waitDeep gate frames
    putMVar done result
  first <- forkIO (waitDeep var depth >> return ())
  yield
  -- The first thread ends, and its stack chunk is free. Its record stays in
  -- the queue of the MVar.
  killThread first
  yield
  -- The second thread grows into the free chunk and waits there.
  putMVar start 400
  yield
  -- Collect while the second thread waits.
  size <- evaluate (length (show [depth .. depth + 150000]))
  putMVar gate size
  result <- takeMVar done
  empty <- isEmptyMVar var
  return (empty && result == size + 400)

main :: IO ()
main = do
  results <- forM [1 .. 12] attempt
  print (and results)
