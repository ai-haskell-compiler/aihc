{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnliftedFFITypes  #-}

module Data.Digest.CRC32C
  ( CRC32C
  , crc32c
  , crc32cUpdate
  ) where

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Short          as BSS
import           Data.ByteString.Unsafe         (unsafeUseAsCStringLen)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.Exts                       (ByteArray#)
import           System.IO.Unsafe               (unsafeDupablePerformIO)

#if !MIN_VERSION_bytestring(0, 11, 1)
import qualified Data.ByteString.Short.Internal as BSS
#endif

class CRC32C a where
  -- | Compute CRC32C checksum
  crc32c :: a -> Word32
  crc32c = crc32cUpdate 0

  -- | Given the CRC32C checksum of a string, compute CRC32C of its
  -- concatenation with another string (t.i., incrementally update
  -- the CRC32C hash value)
  crc32cUpdate :: Word32 -> a -> Word32

instance CRC32C BS.ByteString where
  crc32c bs = unsafeDupablePerformIO $
    unsafeUseAsCStringLen bs $ \(ptr, len) ->
      crc32c_value (castPtr ptr) (fromIntegral len)

  crc32cUpdate cks bs = unsafeDupablePerformIO $
    unsafeUseAsCStringLen bs $ \(ptr, len) ->
      crc32c_extend cks (castPtr ptr) (fromIntegral len)

instance CRC32C BL.ByteString where
  crc32cUpdate = BL.foldlChunks crc32cUpdate

instance CRC32C [Word8] where
  crc32cUpdate n = (crc32cUpdate n) . BL.pack

instance CRC32C BSS.ShortByteString where
  crc32c sbs@(BSS.SBS ba#) = unsafeDupablePerformIO $
    -- Must be unsafe ffi
    crc32c_value' ba# (fromIntegral $ BSS.length sbs)

  crc32cUpdate cks sbs@(BSS.SBS ba#) = unsafeDupablePerformIO $
    -- Must be unsafe ffi
    crc32c_extend' cks ba# (fromIntegral $ BSS.length sbs)

-------------------------------------------------------------------------------

foreign import ccall unsafe "crc32c/crc32c.h crc32c_value"
  crc32c_value :: Ptr Word8 -> CSize -> IO Word32

foreign import ccall unsafe "crc32c/crc32c.h crc32c_extend"
  crc32c_extend :: Word32 -> Ptr Word8 -> CSize -> IO Word32

foreign import ccall unsafe "crc32c/crc32c.h crc32c_value"
  crc32c_value' :: ByteArray# -> CSize -> IO Word32

foreign import ccall unsafe "crc32c/crc32c.h crc32c_extend"
  crc32c_extend' :: Word32 -> ByteArray# -> CSize -> IO Word32
