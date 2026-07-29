{-# LANGUAGE MagicHash #-}

module Main where

import Control.Concurrent (forkIO, yield)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

writeLine :: Addr# -> Int -> IO ()
writeLine message length = hPutBuf stdout (Ptr message :: Ptr ()) length

main :: IO ()
main = do
  writeLine "Hello world main green thread\n"# 30
  forkIO (writeLine "Hello from forked thread\n"# 25)
  writeLine "Still in main\n"# 14
  yield
  writeLine "Back in main\n"# 13
