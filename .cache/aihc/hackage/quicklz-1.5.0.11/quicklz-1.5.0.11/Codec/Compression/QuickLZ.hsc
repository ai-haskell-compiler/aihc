{-# LANGUAGE CPP #-}
-- |
-- Module      : Codec.Compression.QuickLZ
-- Copyright   : (c) Austin Seipp 2011-2012
-- License     : GPLv2
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a high level 'ByteString' interface to the
-- QuickLZ library. More information about quicklz can be found here:
-- <http://quicklz.com>
-- 
-- QuickLZ is fast and compresses well.  The library that is bundled
-- with this version is QuickLZ v1.5.0, with the compression level set
-- to 1.
module Codec.Compression.QuickLZ
       ( -- * Compressing and decompressing strict 'ByteString's
         compress      -- :: S.ByteString -> S.ByteString
       , decompress    -- :: S.ByteString -> S.ByteString
       , decompress'   -- :: S.ByteString -> S.ByteString
       ) where
import Data.Bits
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

#include <quicklz.h>

-- 
-- A word of notice: QuickLZ, for inputs of less than 5 bytes in
-- length, will generate different compression results for the same
-- input on identical runs.
--
-- Because this behavior is not referentially transparent, we use a
-- hack: merely short-circuit on an empty input, and always pad the
-- input with 4 extra null bytes otherwise. This yields identical
-- compression results for every input, giving compress the above nice
-- law.  On decompression, after yielding the resulting string from
-- decompression, we cut the characters off.
--
-- Arguably this is an abomination; nonetheless, this pure interface
-- is a nice abstraction.
--


-- 
-- High-level interface
-- 

-- | Compresses the input 'ByteString'.
compress :: S.ByteString -> S.ByteString
compress xs'
  | S.null xs'    = S.empty
  | otherwise = 
      -- hack: always append 4 extra bytes to ensure input len >= 5
      let xs = xs' `S.append` (S.pack [0,0,0,0]) 
      in unsafePerformIO . allocaBytes qlz_state_compress_sz $ \compress_state ->
        SI.createAndTrim (S.length xs + 400) $ \output -> do
          U.unsafeUseAsCStringLen xs $ \(cstr,len) -> 
            c_qlz_compress cstr output len compress_state
{-# INLINEABLE compress #-}

-- | Decompress the input 'ByteString'.
decompress :: S.ByteString -> S.ByteString
decompress xs
  | S.null xs = S.empty
  | otherwise = 
      unsafePerformIO . allocaBytes qlz_state_decompress_sz $ \decompress_state -> do
        sz <- U.unsafeUseAsCString xs c_qlz_size_decompressed
        SI.createAndTrim sz $ \output -> do
          U.unsafeUseAsCString xs $ \cstr -> do 
            c_ <- c_qlz_decompress cstr output decompress_state
            return $ c_ - 4 -- hack: remove 4 ending bytes off of output string
{-# INLINEABLE decompress #-}

-- | Decompress the input 'ByteString' and save memory via overlapping decompression.
decompress' :: S.ByteString -> S.ByteString
decompress' xs
  | S.null xs = S.empty
  | otherwise = 
      unsafePerformIO . allocaBytes qlz_state_decompress_sz $ \decompress_state -> do
        d <- U.unsafeUseAsCString xs c_qlz_size_decompressed
        let sz = (d + (d `shiftR` 3) + 400)
        SI.createAndTrim sz $ \output -> do
          U.unsafeUseAsCStringLen xs $ \(cstr,clen) -> do
            let dest = output `plusPtr` (sz - clen)
            SI.memcpy dest (castPtr cstr) (fromIntegral clen)
            c_ <- c_qlz_decompress dest output decompress_state
            return $ c_ - 4 -- hack: remove 4 ending bytes off of output string
{-# INLINEABLE decompress' #-}

-- 
-- Simple bindings to some constants
-- 

qlz_state_compress_sz :: Int
qlz_state_compress_sz = #{size qlz_state_compress}
qlz_state_decompress_sz :: Int
qlz_state_decompress_sz = #{size qlz_state_decompress}

--
-- FFI Bindings
-- 

foreign import ccall unsafe "quicklz.h qlz_compress"
  c_qlz_compress :: Ptr a -> Ptr b -> Int -> Ptr c -> IO Int

foreign import ccall unsafe "quicklz.h qlz_decompress"
  c_qlz_decompress :: Ptr a -> Ptr b -> Ptr c -> IO Int

foreign import ccall unsafe "quicklz.h qlz_size_decompressed"
  c_qlz_size_decompressed :: CString -> IO Int
