{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CInt (..))
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
main :: IO CInt
main =
  withPinnedByteArray 64# (\buffer -> do
    input <- stdinHandle
    count <- readIntoBuffer input buffer (CInt (I32# 0#Int32)) (CInt (I32# 64#Int32))
    output <- stdoutHandle
    writeFromBuffer output buffer (CInt (I32# 0#Int32)) count)
