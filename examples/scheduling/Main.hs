{-# LANGUAGE MagicHash #-}

-- Green threads, a delayed thread, and STM timers.
module Main where

import Control.Concurrent (forkIO, myThreadId, threadDelay, yield)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Conc (STM, TVar, atomically, newTVarIO, orElse, readTVar, readTVarIO, registerDelay, retry, writeTVar)
import GHC.Conc.Sync (fromThreadId)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

main :: IO ()
main = do
  greenThreads
  delayedThread
  stmTimers

writeLine :: Addr# -> Int -> IO ()
writeLine message length = hPutBuf stdout (Ptr message :: Ptr ()) length

-- This runs first: the numbering of threads below assumes that no thread
-- has been forked before.
greenThreads :: IO ()
greenThreads = do
  writeLine "Hello world main green thread\n"# 30
  child <- forkIO (writeLine "Hello from forked thread\n"# 25)
  writeLine "Still in main\n"# 14
  yield
  writeLine "Back in main\n"# 13

  -- The main thread has the number one, and it keeps that number. Each new
  -- thread has a larger number.
  mainThread <- myThreadId
  mainThreadAgain <- myThreadId
  if mainThread == mainThreadAgain && fromThreadId mainThread == 1
    then writeLine "main thread keeps one number\n"# 29
    else writeLine "unexpected main thread number\n"# 30
  if child > mainThread && fromThreadId child == 2
    then writeLine "new thread has a larger number\n"# 31
    else writeLine "unexpected new thread number\n"# 29

-- | A delayed thread suspends only itself: the runtime keeps running every
-- other green thread while it waits. Forking only enqueues the worker, so it
-- has not run when main reaches the delay, and it has run once the delay is
-- over. The worker performs no IO of its own, because the WASI host carries
-- one operation at a time and would order it against the timer.
delayedThread :: IO ()
delayedThread = do
  ran <- newIORef False
  _ <- forkIO (writeIORef ran True)
  putStrLn "main: delaying"
  threadDelay 100000
  finished <- readIORef ran
  putStrLn ("main: the worker ran while main waited: " ++ show finished)

wait :: TVar Bool -> STM ()
wait timer = do
  ready <- readTVar timer
  if ready then pure () else retry

stmTimers :: IO ()
stmTimers = do
  immediate <- registerDelay 0
  negative <- registerDelay (-1)
  a <- readTVarIO immediate
  b <- readTVarIO negative
  value <- newTVarIO (7 :: Int)
  first <- registerDelay 1000
  second <- registerDelay 20000
  atomically $
    (writeTVar value 99 >> retry)
      `orElse` (wait first >> wait second)
  c <- readTVarIO value
  d <- readTVarIO first
  e <- readTVarIO second
  third <- registerDelay 1000
  atomically (wait third)
  putStrLn (if a && b && c == 7 && d && e then "STM timers: ok" else "STM timers: failed")
