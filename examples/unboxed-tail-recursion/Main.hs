{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Int (Int32 (..))

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import ccall unsafe putchar :: CInt -> IO CInt

countToTenMillion :: Int# -> Int#
countToTenMillion current =
  case current of
    10_000_000# -> current
    _ -> countToTenMillion ((+#) current 1#)

char :: Int32# -> CInt
char value = CInt (I32# value)

main :: IO CInt
main =
  case countToTenMillion 0# of
    10_000_000# -> putchar (char 10#Int32)
    _ -> putchar (char 70#Int32)
