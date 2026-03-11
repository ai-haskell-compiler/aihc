{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts (Int#, Word#)
import GHC.Int.Compat
import GHC.Word.Compat

_i8 :: Int8 -> Int#
_i8 (I8# x) = x

_i16 :: Int16 -> Int#
_i16 (I16# x) = x

_i32 :: Int32 -> Int#
_i32 (I32# x) = x

_w8 :: Word8 -> Word#
_w8 (W8# x) = x

_w16 :: Word16 -> Word#
_w16 (W16# x) = x

_w32 :: Word32 -> Word#
_w32 (W32# x) = x

_w64 :: Word64 -> Word#
_w64 (W64# x) = x

main :: IO ()
main = pure ()