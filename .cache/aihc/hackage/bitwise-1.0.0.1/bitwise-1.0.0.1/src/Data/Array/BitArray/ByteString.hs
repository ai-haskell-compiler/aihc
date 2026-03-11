{-|

Module      :  Data.Array.BitArray.ByteString
Copyright   :  (c) Claude Heiland-Allen 2012,2018
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Copy bit array data to and from ByteStrings.

-}
module Data.Array.BitArray.ByteString
  (
  -- * Immutable copying.
    toByteString
  , fromByteString
  -- * Mutable copying.
  , toByteStringIO
  , fromByteStringIO
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Ix (Ix, rangeSize)
import Data.Word (Word8)
import Control.Monad (when)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)

import Data.Bits.Bitwise (mask)
import Data.Array.BitArray (BitArray)
import Data.Array.BitArray.IO (IOBitArray)
import qualified Data.Array.BitArray.IO as IO
import Data.Array.BitArray.Internal (iobData)

-- | Copy to a ByteString.  The most significant bits of the last byte
--   are padded with 0 unless the array was a multiple of 8 bits in size.
toByteString :: Ix i => BitArray i -> ByteString
toByteString a = unsafePerformIO $ toByteStringIO =<< IO.unsafeThaw a

-- | Copy from a ByteString.  Much like 'listArray' but with packed bits.
fromByteString :: Ix i => (i, i) {- ^ bounds -} -> ByteString {- ^ packed elems -} -> BitArray i
fromByteString bs s = unsafePerformIO $ IO.unsafeFreeze =<< fromByteStringIO bs s

-- | Copy to a ByteString.  The most significant bits of the last byte
--   are padded with 0 unless the array was a multiple of 8 bits in size.
toByteStringIO :: Ix i => IOBitArray i -> IO ByteString
toByteStringIO a = do
  bs <- IO.getBounds a
  let rs = rangeSize bs
      bytes = (rs + 7) `shiftR` 3
      bits = rs .&. 7
      lastByte = bytes - 1
  withForeignPtr (iobData a) $ \p -> do
    when (bits /= 0) $ do
      b <- peekByteOff p lastByte
      pokeByteOff p lastByte (b .&. mask bits :: Word8)
    packCStringLen (castPtr p, bytes)

-- | Copy from a ByteString.  Much like 'newListArray' but with packed bits.
fromByteStringIO :: Ix i => (i, i) {- ^ bounds -} -> ByteString {- ^ packed elems -} -> IO (IOBitArray i)
fromByteStringIO bs s = do
  a <- IO.newArray bs False
  let rs = rangeSize bs
      bytes = (rs + 7) `shiftR` 3
  unsafeUseAsCStringLen s $ \(src, len) ->
    withForeignPtr (iobData a) $ \dst ->
      copyBytes dst (castPtr src) (bytes `min` len)
  return a
