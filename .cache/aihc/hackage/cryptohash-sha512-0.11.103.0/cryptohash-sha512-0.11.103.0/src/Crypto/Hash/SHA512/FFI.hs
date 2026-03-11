{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Unsafe  #-}

-- Ugly hack to workaround https://ghc.haskell.org/trac/ghc/ticket/14452
{-# OPTIONS_GHC -O0
                -fdo-lambda-eta-expansion
                -fcase-merge
                -fstrictness
                -fno-omit-interface-pragmas
                -fno-ignore-interface-pragmas #-}

{-# OPTIONS_GHC -optc-Wall -optc-O3 #-}

-- |
-- Module      : Crypto.Hash.SHA522.FFI
-- License     : BSD-3
--
module Crypto.Hash.SHA512.FFI where

import           Data.ByteString (ByteString)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

-- | SHA-512 Context
--
-- The context data is exactly 208 bytes long, however
-- the data in the context is stored in host-endianness.
--
-- The context data is made up of
--
--  * Two 'Word64's representing the number of bytes already feed to hash algorithm so far (lower word first),
--
--  * a 128-element 'Word8' buffer holding partial input-chunks, and finally
--
--  * a 8-element 'Word64' array holding the current work-in-progress digest-value.
--
newtype Ctx = Ctx ByteString
  deriving (Eq)

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha384_init"
    c_sha384_init :: Ptr Ctx -> IO ()

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512_init"
    c_sha512_init :: Ptr Ctx -> IO ()

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512t_init"
    c_sha512t_init :: Ptr Ctx -> Word16 -> IO ()


foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512_update"
    c_sha512_update_unsafe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()

foreign import capi safe "hs_sha512.h hs_cryptohash_sha512_update"
    c_sha512_update_safe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()


foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512t_finalize"
    c_sha512t_finalize :: Ptr Ctx -> Word16 -> Ptr Word8 -> IO ()

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512t_finalize"
    c_sha512t_finalize_len :: Ptr Ctx -> Word16 -> Ptr Word8 -> IO Word64


foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha384_hash"
    c_sha384_hash_unsafe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi safe "hs_sha512.h hs_cryptohash_sha384_hash"
    c_sha384_hash_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512_hash"
    c_sha512_hash_unsafe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi safe "hs_sha512.h hs_cryptohash_sha512_hash"
    c_sha512_hash_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi unsafe "hs_sha512.h hs_cryptohash_sha512t_hash"
    c_sha512t_hash_unsafe :: Ptr Word8 -> CSize -> Ptr Word8 -> Word16 -> IO ()

foreign import capi safe "hs_sha512.h hs_cryptohash_sha512t_hash"
    c_sha512t_hash_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> Word16 -> IO ()
