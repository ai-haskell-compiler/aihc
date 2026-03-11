-- | Missing @byteSwap@ functions for unsigned integers.
--
-- Don't know why this one is missing.

module Raehik.Compat.Data.Word.ByteSwap where

import GHC.Exts ( byteSwap# )
import GHC.Word ( Word(W#) )

byteSwap :: Word -> Word
byteSwap (W# i#) = W# (byteSwap# i#)
{-# INLINE byteSwap #-}
