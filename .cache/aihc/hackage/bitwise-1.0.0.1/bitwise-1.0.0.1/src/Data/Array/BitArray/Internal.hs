{-# OPTIONS_HADDOCK hide #-}
{-|

Module      :  Data.Array.BitArray.Internal
Copyright   :  (c) Claude Heiland-Allen 2012
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Bit arrays internals.  Not exposed.

-}
module Data.Array.BitArray.Internal
  ( BitArray(..)
  , IOBitArray(..)
  , getBounds
  , newArray_
  , freeze
  , thaw
  , copy
  , unsafeFreeze
  , unsafeThaw
  ) where

import Data.Bits (shiftL, shiftR)
import Data.Ix (Ix, rangeSize)
import Data.Word (Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)

-- | The type of immutable bit arrays.
newtype BitArray i = B (IOBitArray i)

-- | The type of mutable bit arrays in the 'IO' monad.
data IOBitArray i = IOB{ iobBoundLo :: !i, iobBoundHi :: !i, iobBytes :: {-# UNPACK #-} !Int, iobData :: {-# UNPACK #-} !(ForeignPtr Word64) }

-- | Create a new array filled with unspecified initial values.
{-# INLINE newArray_ #-}
newArray_ :: Ix i => (i, i) {- ^ bounds -} -> IO (IOBitArray i)
newArray_ bs@(bl, bh) = do
  let bits = rangeSize bs
      nwords = (bits + 63) `shiftR` 6
      bytes = nwords `shiftL` 3
  p <- mallocForeignPtrBytes bytes
  return IOB{ iobBoundLo = bl, iobBoundHi = bh, iobBytes = bytes, iobData = p }

-- | Get the bounds of a bit array.
{-# INLINE getBounds #-}
getBounds :: Ix i => IOBitArray i -> IO (i, i)
getBounds a = return (iobBoundLo a, iobBoundHi a)

-- | Snapshot the array into an immutable form.
{-# INLINE freeze #-}
freeze :: Ix i => IOBitArray i -> IO (BitArray i)
freeze a = B `fmap` copy a

-- | Snapshot the array into an immutable form.  Unsafe when the source
--   array can be modified later.
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: Ix i => IOBitArray i -> IO (BitArray i)
unsafeFreeze a = B `fmap` return a

-- | Convert an array from immutable form.
{-# INLINE thaw #-}
thaw :: Ix i => BitArray i -> IO (IOBitArray i)
thaw (B a) = copy a

-- | Convert an array from immutable form.  Unsafe to modify the result
--   unless the source array is never used later.
{-# INLINE unsafeThaw #-}
unsafeThaw :: Ix i => BitArray i -> IO (IOBitArray i)
unsafeThaw (B a) = return a

-- | Copy an array.
{-# INLINE copy #-}
copy :: Ix i => IOBitArray i -> IO (IOBitArray i)
copy a = do
  b <- newArray_ =<< getBounds a
  withForeignPtr (iobData a) $ \ap ->
    withForeignPtr (iobData b) $ \bp ->
      copyBytes bp ap (iobBytes b)
  return b
