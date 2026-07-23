{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Int (Int32 (..))

foreign import ccall unsafe putchar :: CInt -> IO CInt

factorial :: Int -> Integer -> Integer -> Integer
factorial count factor accumulator =
  case count of
    0 -> accumulator
    _ -> factorial (count - 1) (factor + 1) (accumulator * factor)

char value = CInt (I32# value)

main :: IO CInt
main =
  if factorial 1500 1 1 > 0
    then putchar (char 10#Int32)
    else putchar (char 70#Int32)
