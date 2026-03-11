{-# LANGUAGE ForeignFunctionInterface #-}
module Text.Discount.Internal where

import Data.Bits
import Foreign.C
import Foreign.Ptr


-- |An option flag to be passed to the discount parser.
newtype DiscountOption = DiscountOption { unDiscountOption :: CInt } deriving (Eq, Show)

data MMIOT = MMIOT
type MMIOPtr = Ptr MMIOT

foreign import ccall unsafe "mkdio.h mkd_string" mkd_string :: CString -> CInt -> CInt -> IO MMIOPtr
foreign import ccall unsafe "mkdio.h mkd_compile" mkd_compile :: MMIOPtr -> CInt -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_document" mkd_document :: MMIOPtr -> Ptr CString -> IO CInt
foreign import ccall unsafe "mkdio.h mkd_cleanup" mkd_cleanup :: MMIOPtr -> IO ()


-- |INTERNAL USE ONLY. Combine a list of 'DiscountOption' values into
-- a single flag.
combineOptions :: [DiscountOption] -> DiscountOption
combineOptions = DiscountOption . foldr ((.|.) . unDiscountOption) 0