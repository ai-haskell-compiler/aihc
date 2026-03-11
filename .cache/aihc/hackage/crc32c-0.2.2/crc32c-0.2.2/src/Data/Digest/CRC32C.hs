module Data.Digest.CRC32C(
    crc32c
  , crc32c_update
  ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Word

import           Foreign
import           Foreign.C
import           Foreign.Marshal.Unsafe

crc32c :: ByteString -> Word32
crc32c bs =
  fromIntegral $
    unsafeLocalState $
      unsafeUseAsCStringLen bs $ \(p, l) ->
        lib_crc32c_value (castPtr p) (fromIntegral l)

crc32c_update :: Word32 -> ByteString -> Word32
crc32c_update hash bs =
  fromIntegral $
    unsafeLocalState $
      unsafeUseAsCStringLen bs $ \(p, l) ->
        lib_crc32c_extend (fromIntegral hash) (castPtr p) (fromIntegral l)

-- FFI: safe or unsafe?
-- All of the FFI here is not-reentrant so we don't have to use safe. However
-- for very large inputs, the calls will take a while and during that time
-- they will block GC and other Haskell threads on the same capability. So
-- for very large inputs we would prefer to use a safe FFI call. But for small
-- inputs, the overhead of a safe FFI call is quite substantial, e.g
-- ~5x for 256 bytes, dropping to only ~10% for 16kb.
--
-- The solution we use here is to use unsafe FFI calls for smaller buffers and
-- safe FFI calls for larger buffers. This bounds the time that these calls
-- can block other threads or GC.

lib_crc32c_extend :: CUInt -> Ptr CUChar -> CSize -> IO CUInt
lib_crc32c_extend hash p l | l > 0x10000 = lib_crc32c_extend_safe hash p l
                           | otherwise   = lib_crc32c_extend_unsafe hash p l

lib_crc32c_value :: Ptr CUChar -> CSize -> IO CUInt
lib_crc32c_value p l | l > 0x10000 = lib_crc32c_value_safe p l
                     | otherwise   = lib_crc32c_value_unsafe p l

foreign import ccall unsafe "crc32c/crc32c.h crc32c_extend"
  lib_crc32c_extend_unsafe :: CUInt -> Ptr CUChar -> CSize -> IO CUInt

foreign import ccall safe "crc32c/crc32c.h crc32c_extend"
  lib_crc32c_extend_safe :: CUInt -> Ptr CUChar -> CSize -> IO CUInt

foreign import ccall unsafe "crc32c/crc32c.h crc32c_value"
  lib_crc32c_value_unsafe :: Ptr CUChar -> CSize -> IO CUInt

foreign import ccall safe "crc32c/crc32c.h crc32c_value"
  lib_crc32c_value_safe :: Ptr CUChar -> CSize -> IO CUInt
