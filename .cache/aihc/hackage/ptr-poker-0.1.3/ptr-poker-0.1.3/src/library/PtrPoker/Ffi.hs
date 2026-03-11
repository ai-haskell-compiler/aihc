{-# LANGUAGE UnliftedFFITypes #-}

module PtrPoker.Ffi where

import Foreign.C
import PtrPoker.Prelude

foreign import ccall unsafe "static int_dec"
  pokeIntInDec :: CInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_int_dec"
  pokeLongLongIntInDec :: CLLong -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static uint_dec"
  pokeUIntInDec :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_uint_dec"
  pokeLongLongUIntInDec :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static uint_hex"
  pokeUIntInHex :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static long_long_uint_hex"
  pokeLongLongUIntInHex :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static rev_poke_int64"
  revPokeInt64 :: CLLong -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static rev_poke_uint64"
  revPokeUInt64 :: CULLong -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static dtoa_grisu3"
  pokeDouble :: Double -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "static count_text_allocation_size"
  countTextAllocationSize :: ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "static encode_text"
  encodeText :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)
