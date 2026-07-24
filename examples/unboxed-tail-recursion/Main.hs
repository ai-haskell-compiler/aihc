{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CInt (..))
import GHC.IO.StdHandles
  ( copyAddrToByteArray,
    stdoutHandle,
    withPinnedByteArray,
    writeFromBuffer,
  )

foreign import prim (+#) :: Int# -> Int# -> Int#

countToTenMillion :: Int# -> Int#
countToTenMillion current =
  case current of
    10_000_000# -> current
    _ -> countToTenMillion ((+#) current 1#)

main :: IO CInt
main =
  case countToTenMillion 0# of
    10_000_000# -> writeOk
    _ -> writeFail

writeOk :: IO CInt
writeOk =
  withPinnedByteArray 3# (\buffer -> do
    copyAddrToByteArray "ok\n"# buffer 0# 3#
    output <- stdoutHandle
    writeFromBuffer output buffer zero length)
  where
    zero = CInt (I32# 0#Int32)
    length = CInt (I32# 3#Int32)

writeFail :: IO CInt
writeFail =
  withPinnedByteArray 5# (\buffer -> do
    copyAddrToByteArray "fail\n"# buffer 0# 5#
    output <- stdoutHandle
    writeFromBuffer output buffer zero length)
  where
    zero = CInt (I32# 0#Int32)
    length = CInt (I32# 5#Int32)
