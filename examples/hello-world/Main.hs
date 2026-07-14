{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Foreign.C.Types (CInt (..))
import GHC.Int (Int32 (..))

foreign import ccall unsafe putchar :: CInt -> IO CInt

char :: Int32# -> CInt
char value = CInt (I32# value)

main :: IO CInt
main =
  putchar (char 72#Int32)
    >> putchar (char 101#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 111#Int32)
    >> putchar (char 44#Int32)
    >> putchar (char 32#Int32)
    >> putchar (char 119#Int32)
    >> putchar (char 111#Int32)
    >> putchar (char 114#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 100#Int32)
    >> putchar (char 33#Int32)
    >> putchar (char 10#Int32)
