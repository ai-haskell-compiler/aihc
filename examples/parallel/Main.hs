{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar (MVar, isEmptyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM_)
import Data.List (foldl')
import GHC.Conc (atomically, newTVarIO, numCapabilities, readTVar, readTVarIO, writeTVar)

-- The arguments give the machine four capabilities, and Haskell threads run
-- on them at the same time. See "Workers" in
-- docs/multi-threaded-rts-design.md.

-- Wait until a variable is full, at most a number of times. The wait never
-- schedules, so only a thread on another capability can fill the variable
-- while it runs.
waitFull :: MVar () -> Int -> IO Bool
waitFull signal = go
  where
    go 0 = return False
    go !count = do
      empty <- isEmptyMVar signal
      if empty then go (count - 1) else return True

-- A top-level value that several threads force at the same time.
primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : rest) = p : sieve [n | n <- rest, n `mod` p /= 0]
    sieve [] = []

main :: IO ()
main = do
  capabilities <- getNumCapabilities
  print (capabilities, numCapabilities)
  signal <- newEmptyMVar
  answer <- newEmptyMVar
  _ <- forkIO (waitFull signal 20000000 >>= putMVar answer)
  _ <- forkIO (putMVar signal ())
  parallel <- takeMVar answer
  putStrLn ("threads run at the same time: " ++ show parallel)
  -- Eight threads force one lazy list, add to one MVar, and add to one
  -- TVar. No addition is lost.
  counter <- newMVar (0 :: Int)
  total <- newTVarIO (0 :: Int)
  dones <- forM [1 .. 8 :: Int] $ \index -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      let mine = foldl' (+) 0 (take (100 + index) primes)
      replicateM_ 1000 (modifyMVar_ counter (return . (+ 1)))
      replicateM_ 1000 (atomically (readTVar total >>= writeTVar total . (+ 1)))
      putMVar done mine
    return done
  sums <- mapM takeMVar dones
  print sums
  readMVar counter >>= print
  readTVarIO total >>= print
  -- A token goes around a ring of threads.
  let size = 16
  links <- forM [1 .. size] (const newEmptyMVar)
  finished <- newEmptyMVar
  forM_ (zip [0 :: Int ..] links) $ \(index, link) ->
    forkIO $ do
      let next = links !! ((index + 1) `mod` size)
      let pass 0 = return ()
          pass rounds = do
            token <- takeMVar link
            if index == size - 1 && token == size * 500 - 1
              then putMVar finished token
              else putMVar next (token + 1)
            pass (rounds - 1 :: Int)
      pass 500
  putMVar (head links) 0
  takeMVar finished >>= print
