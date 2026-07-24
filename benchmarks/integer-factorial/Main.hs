{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Exts (Addr#)

foreign import ccall unsafe puts :: Addr# -> IO CInt

factorial :: Int -> Integer -> Integer -> Integer
factorial count factor accumulator =
  case count of
    0 -> accumulator
    _ -> factorial (count - 1) (factor + 1) (accumulator * factor)

main :: IO CInt
main =
  if factorial 1500 1 1 > 0
    then puts "ok"#
    else puts "fail"#
