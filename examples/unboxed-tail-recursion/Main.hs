{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import GHC.Int (Int (..))
import GHC.Prim ((+#))
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

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
writeOk = do
  hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) length
  return length
  where
    length = I# 3#

writeFail :: IO Int
writeFail = do
  hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) length
  return length
  where
    length = I# 5#
