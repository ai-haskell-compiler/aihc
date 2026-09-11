{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts (Char (..), Char#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

-- A code point crosses the C boundary as the 32-bit HsChar, while GRIN holds
-- it in a word slot, so both directions convert. The boxed import unwraps the
-- Char constructor first; the unboxed one passes the Char# itself.
foreign import ccall unsafe "toupper" c_toupper :: Char -> Char

foreign import ccall unsafe "toupper" c_toupperChar# :: Char# -> Char#

upper :: Char -> Char
upper (C# code) = C# (c_toupperChar# code)

main :: IO ()
main =
  if c_toupper 'a' == 'A' && upper 'z' == 'Z' && c_toupper '1' == '1'
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
