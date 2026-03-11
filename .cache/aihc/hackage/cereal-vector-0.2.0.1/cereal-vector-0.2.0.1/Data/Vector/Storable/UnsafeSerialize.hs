{-# LANGUAGE ScopedTypeVariables, Unsafe #-}
{-# OPTIONS_GHC -Wall #-}

-- | Efficient, but unsafe 'Get' and 'Putter' for
-- "Data.Vector.Storable" vectors. The serialized format is an 'Int64'
-- representing the length of the 'Vector', followed by the raw
-- bytes. Therefore behavior may be unpredictable if serialized data
-- is transferred between machines with different word size or
-- endianness.
module Data.Vector.Storable.UnsafeSerialize (
    unsafeGetVector
  , unsafePutVector
) where

import Control.Monad (when)

import qualified Data.ByteString.Internal as BS
import Data.Int (Int64)
import Data.Serialize (Get, getBytes, putByteString, Putter, Serialize(..))
import Data.Vector.Storable ( unsafeFromForeignPtr0
                            , unsafeToForeignPtr0
                            , Vector)
import Data.Vector.Storable.Internal (updPtr)

import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable, sizeOf)

-- | Get a 'Vector' in host order, endian form, and word width.
unsafeGetVector :: forall a. Storable a => Get (Vector a)
{-# INLINE unsafeGetVector #-}
unsafeGetVector = do 
  len64 <- get :: Get Int64
  when (len64 > fromIntegral (maxBound :: Int)) $
    fail "Host can't deserialize a Vector longer than (maxBound :: Int)"
  let len    = fromIntegral len64
      nbytes = len * sizeOf (undefined :: a)
  bs <- getBytes nbytes
  let (fp, off, _)    = BS.toForeignPtr bs
      fp' | off /= 0  = updPtr (`advancePtr` off) fp
          | otherwise = fp
  return $ unsafeFromForeignPtr0 (castForeignPtr fp') len

-- | Put a 'Vector' in host order, endian form, and word width.
unsafePutVector :: forall a. Storable a => Putter (Vector a)
{-# INLINE unsafePutVector #-}
unsafePutVector v = do
  let (fp, len) = unsafeToForeignPtr0 v
      nbytes    = len * sizeOf (undefined :: a)
      bs        = BS.fromForeignPtr (castForeignPtr fp) 0 nbytes
  put (fromIntegral len :: Int64)
  putByteString bs
