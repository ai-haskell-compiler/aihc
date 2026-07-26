{-# LANGUAGE MagicHash #-}

module Main where

import GHC.IO.StdHandles
  ( copyAddrToByteArray,
    stdoutHandle,
    withPinnedByteArray,
    writeFromBuffer,
  )

main :: IO Int
main =
  withPinnedByteArray 13# (\buffer -> do
    copyAddrToByteArray "Hello world!\n"# buffer 0# 13#
    output <- stdoutHandle
    writeFromBuffer output buffer 0 13)
