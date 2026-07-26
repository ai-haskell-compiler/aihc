{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import GHC.IO.StdHandles
  ( copyAddrToByteArray,
    stdoutHandle,
    withPinnedByteArray,
    writeFromBuffer,
  )
import GHC.Int (Int (..))

foreign import prim (+#) :: Int# -> Int# -> Int#

countToTenMillion :: Int# -> Int#
countToTenMillion current =
  case current of
    10_000_000# -> current
    _ -> countToTenMillion ((+#) current 1#)

main :: IO Int
main =
  case countToTenMillion 0# of
    10_000_000# -> writeOk
    _ -> writeFail

writeOk :: IO Int
writeOk =
  withPinnedByteArray 3# (\buffer -> do
    copyAddrToByteArray "ok\n"# buffer 0# 3#
    output <- stdoutHandle
    writeFromBuffer output buffer zero length)
  where
    zero = I# 0#
    length = I# 3#

writeFail :: IO Int
writeFail =
  withPinnedByteArray 5# (\buffer -> do
    copyAddrToByteArray "fail\n"# buffer 0# 5#
    output <- stdoutHandle
    writeFromBuffer output buffer zero length)
  where
    zero = I# 0#
    length = I# 5#
