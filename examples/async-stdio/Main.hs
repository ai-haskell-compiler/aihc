{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CInt (..))
import GHC.IO.StdHandles
  ( getIOBufferByte,
    newIOBuffer,
    readIntoBuffer,
    setIOBufferByte,
    stdinHandle,
    stdoutHandle,
    writeFromBuffer,
  )

-- This example uses one stable buffer below the future Handle layer. It echoes
-- one input block while the green-thread scheduler can run during each IO
-- request. The get/set pair also demonstrates direct buffer access.
main :: IO CInt
main = do
  buffer <- newIOBuffer (CInt (I32# 64#Int32))
  input <- stdinHandle
  count <- readIntoBuffer input buffer (CInt (I32# 0#Int32)) (CInt (I32# 64#Int32))
  firstByte <- getIOBufferByte buffer (CInt (I32# 0#Int32))
  setIOBufferByte buffer (CInt (I32# 0#Int32)) firstByte
  output <- stdoutHandle
  writeFromBuffer output buffer (CInt (I32# 0#Int32)) count
