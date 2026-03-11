{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.Hash.Types
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of internal utility functions and exports of common imports
--
-----------------------------------------------------------------------------

module Nettle.Utils
	( Ptr
	, plusPtr
	, FunPtr
	, Word8
	, Word
	, forM_
	, unsafeDupablePerformIO
	, withByteStringPtr
	, netEncode
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Foreign.Ptr (Ptr, plusPtr, FunPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Word (Word8, Word)
import Control.Monad (forM_)

import System.IO.Unsafe (unsafeDupablePerformIO)

{-|
Run action in IO monad with length and pointer to first byte of a 'B.ByteString'
-}
withByteStringPtr :: B.ByteString -> (Word -> Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f = withForeignPtr fptr $ \ptr -> f (fromIntegral len) (ptr `plusPtr` off)
	where (fptr, off, len) = B.toForeignPtr b

{-|
Encode any 'Integral' @value@ in @bytes@ 'Word8' as big endian value.
-}
netEncode :: (Integral n) => Int {- ^ @bytes@ argument -} -> n {- ^ @value@ argument -} -> [Word8]
netEncode bytes = _work bytes [] where
	_work 0 r _ = r
	_work n r v = let (d, m) = divMod v 256 in _work (n-1) (fromIntegral m:r) d
