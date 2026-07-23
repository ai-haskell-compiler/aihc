{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Int (Int32 (..))

foreign import ccall unsafe putchar :: CInt -> IO CInt

fibonacci :: Int -> Integer -> Integer -> Integer
fibonacci count older newer =
  case count of
    0 -> older
    _ -> fibonacci (count - 1) newer (older + newer)

char value = CInt (I32# value)

main :: IO CInt
main =
  if fibonacci 20000 0 1 > 0
    then putchar (char 10#Int32)
    else putchar (char 70#Int32)
