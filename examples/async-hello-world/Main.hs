{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CInt (..))
import GHC.IO.StdHandles
  ( copyAddrToByteArray,
    stdoutHandle,
    withPinnedByteArray,
    writeFromBuffer,
  )

main :: IO CInt
main =
  withPinnedByteArray 13# (\buffer -> do
    copyAddrToByteArray "Hello world!\n"# buffer 0# 13#
    output <- stdoutHandle
    writeFromBuffer output buffer zero length)
  where
    zero = CInt (I32# 0#Int32)
    length = CInt (I32# 13#Int32)
