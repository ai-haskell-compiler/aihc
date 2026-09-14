{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO, myThreadId, yield)
import GHC.Conc.Sync (fromThreadId)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

writeLine :: Addr# -> Int -> IO ()
writeLine message length = hPutBuf stdout (Ptr message :: Ptr ()) length

main :: IO ()
main = do
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
