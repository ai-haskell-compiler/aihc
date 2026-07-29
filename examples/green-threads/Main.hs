{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

writeLine :: Addr# -> Int -> IO ()
writeLine message length = hPutBuf stdout (Ptr message :: Ptr ()) length

main :: IO ()
main = do
  writeLine "Hello world main green thread\n"# 30
  startForkedThread <- newEmptyMVar
  forkedThreadDone <- newEmptyMVar
  forkIO
    ( do
        takeMVar startForkedThread
        writeLine "Hello from forked thread\n"# 25
        putMVar forkedThreadDone ()
    )
  writeLine "Still in main\n"# 14
  putMVar startForkedThread ()
  takeMVar forkedThreadDone
  writeLine "Back in main\n"# 13
