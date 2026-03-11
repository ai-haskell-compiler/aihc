{-# LANGUAGE TemplateHaskell #-}
module Text.Unidecode where

import           Data.Char        (ord)
import           Foreign.C
import           System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "findString" cFindString :: CUInt -> IO  CString

-- | A transliterator of last resort
unidecode :: Char -> String
unidecode x = unsafePerformIO $ cFindString (fromIntegral $ ord x) >>= peekCString
