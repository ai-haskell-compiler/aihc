{-# CFILES
      c_impl/reference/skein.c
      c_impl/reference/skein_block.c #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Skein.Internal
-- Copyright   :  (c) 2011 Felipe A. Lessa
-- License     :  BSD3 (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- Basic wrappers around the C library.  You shouldn't need to
-- use these functions.  Instead, use the high-level
-- "Crypto.Skein" module.
--
-----------------------------------------------------------------------------

module Crypto.Skein.Internal
    ( -- * Return codes
      sKEIN_SUCCESS
    , sKEIN_FAIL
    , sKEIN_BAD_HASHLEN
    , check

      -- * Skein-256
    , Skein256Ctx(..)
    , skein256Init
    , skein256Update
    , skein256Final
    , skein256InitExt
    , skein256FinalPad
    , skein256Output

      -- * Skein-512
    , Skein512Ctx(..)
    , skein512Init
    , skein512Update
    , skein512Final
    , skein512InitExt
    , skein512FinalPad
    , skein512Output

      -- * Skein-1024
    , Skein1024Ctx(..)
    , skein1024Init
    , skein1024Update
    , skein1024Final
    , skein1024InitExt
    , skein1024FinalPad
    , skein1024Output

      -- * Misc constants
    , sKEIN_SEQUENTIAL
    ) where

-- from base
import Foreign
import Foreign.C

-- from bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU

-- from this package
#include "skein.h"

sKEIN_SUCCESS, sKEIN_FAIL, sKEIN_BAD_HASHLEN :: CInt
sKEIN_SUCCESS     = #{const SKEIN_SUCCESS}
sKEIN_FAIL        = #{const SKEIN_FAIL}
sKEIN_BAD_HASHLEN = #{const SKEIN_BAD_HASHLEN}

-- | Throws exception if the function does not return
-- successfully.
check :: IO CInt -> IO ()
check act = do
  x <- act
  case () of
    () | x == sKEIN_SUCCESS     -> return ()
       | x == sKEIN_FAIL        -> fail "Skein returned FAIL."
       | x == sKEIN_BAD_HASHLEN -> fail "Skein returned BAD_HASHLEN."
       | otherwise              -> fail "Skein returned unknown code."

newtype Skein256Ctx  = S256Ctx  {unS256Ctx  :: B.ByteString}
newtype Skein512Ctx  = S512Ctx  {unS512Ctx  :: B.ByteString}
newtype Skein1024Ctx = S1024Ctx {unS1024Ctx :: B.ByteString}

size256Ctx, size512Ctx, size1024Ctx :: Int
size256Ctx  = #{size Skein_256_Ctxt_t}
size512Ctx  = #{size Skein_512_Ctxt_t}
size1024Ctx = #{size Skein1024_Ctxt_t}

instance Storable Skein256Ctx where
    sizeOf    _ = size256Ctx
    alignment _ = 16
    peek ptr =
        fmap S256Ctx $ BI.create size256Ctx $ \bptr ->
            BI.memcpy bptr (castPtr ptr) (fromIntegral size256Ctx)
    poke ptr (S256Ctx bs) =
        BU.unsafeUseAsCString bs $ \bs_ptr ->
            BI.memcpy (castPtr ptr) (castPtr bs_ptr) (fromIntegral size256Ctx)

instance Storable Skein512Ctx where
    sizeOf    _ = size512Ctx
    alignment _ = 16
    peek ptr =
        fmap S512Ctx $ BI.create size512Ctx $ \bptr ->
            BI.memcpy bptr (castPtr ptr) (fromIntegral size512Ctx)
    poke ptr (S512Ctx bs) =
        BU.unsafeUseAsCString bs $ \bs_ptr ->
            BI.memcpy (castPtr ptr) (castPtr bs_ptr) (fromIntegral size512Ctx)

instance Storable Skein1024Ctx where
    sizeOf    _ = size1024Ctx
    alignment _ = 16
    peek ptr =
        fmap S1024Ctx $ BI.create size1024Ctx $ \bptr ->
            BI.memcpy bptr (castPtr ptr) (fromIntegral size1024Ctx)
    poke ptr (S1024Ctx bs) =
        BU.unsafeUseAsCString bs $ \bs_ptr ->
            BI.memcpy (castPtr ptr) (castPtr bs_ptr) (fromIntegral size1024Ctx)

foreign import ccall unsafe "skein.h Skein_256_Init" skein256Init  :: Ptr Skein256Ctx  -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_Init" skein512Init  :: Ptr Skein512Ctx  -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_Init" skein1024Init :: Ptr Skein1024Ctx -> CSize -> IO CInt

foreign import ccall unsafe "skein.h Skein_256_Update" skein256Update  :: Ptr Skein256Ctx  -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_Update" skein512Update  :: Ptr Skein512Ctx  -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_Update" skein1024Update :: Ptr Skein1024Ctx -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "skein.h Skein_256_Final" skein256Final  :: Ptr Skein256Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_Final" skein512Final  :: Ptr Skein512Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_Final" skein1024Final :: Ptr Skein1024Ctx -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "skein.h Skein_256_InitExt" skein256InitExt   :: Ptr Skein256Ctx  -> CSize -> Word64 -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_InitExt" skein512InitExt   :: Ptr Skein512Ctx  -> CSize -> Word64 -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_InitExt" skein1024InitExt  :: Ptr Skein1024Ctx -> CSize -> Word64 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "skein.h Skein_256_Final_Pad" skein256FinalPad  :: Ptr Skein256Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_Final_Pad" skein512FinalPad  :: Ptr Skein512Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_Final_Pad" skein1024FinalPad :: Ptr Skein1024Ctx -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "skein.h Skein_256_Output" skein256Output  :: Ptr Skein256Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein_512_Output" skein512Output  :: Ptr Skein512Ctx  -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "skein.h Skein1024_Output" skein1024Output :: Ptr Skein1024Ctx -> Ptr Word8 -> IO CInt


-- | Pass to @InitExt@ to get sequential processing.
sKEIN_SEQUENTIAL :: Word64
sKEIN_SEQUENTIAL = 0
