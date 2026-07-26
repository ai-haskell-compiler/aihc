{-# LANGUAGE MagicHash #-}

module Main where

import GHC.IO.StdHandles
  ( readIntoBuffer,
    stdinHandle,
    stdoutHandle,
    withPinnedByteArray,
    writeFromBuffer,
  )

-- This example uses one stable buffer below the future Handle layer. It echoes
-- one input block while the green-thread scheduler can run during each IO
-- request.
main :: IO Int
main =
  withPinnedByteArray 64# (\buffer -> do
    input <- stdinHandle
    count <- readIntoBuffer input buffer 0 64
    output <- stdoutHandle
    writeFromBuffer output buffer 0 count)
