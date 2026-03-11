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
-- Module      : Crypto.Hash.SHA1.FFI
-- License     : BSD-3
--
module Crypto.Hash.SHA1.FFI where

import           Data.ByteString (ByteString)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

-- | SHA-1 Context
--
-- The context data is exactly 92 bytes long, however
-- the data in the context is stored in host-endianness.
--
-- The context data is made up of
--
--  * a 'Word64' representing the number of bytes already feed to hash algorithm so far,
--
--  * a 64-element 'Word8' buffer holding partial input-chunks, and finally
--
--  * a 5-element 'Word32' array holding the current work-in-progress digest-value.
--
-- Consequently, a SHA-1 digest as produced by 'hash', 'hashlazy', or 'finalize' is 20 bytes long.
newtype Ctx = Ctx ByteString
  deriving (Eq)

foreign import capi unsafe "sha1.h hs_cryptohash_sha1_init"
    c_sha1_init :: Ptr Ctx -> IO ()

foreign import capi unsafe "sha1.h hs_cryptohash_sha1_update"
    c_sha1_update_unsafe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()

foreign import capi safe "sha1.h hs_cryptohash_sha1_update"
    c_sha1_update_safe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()

foreign import capi unsafe "sha1.h hs_cryptohash_sha1_finalize"
    c_sha1_finalize :: Ptr Ctx -> Ptr Word8 -> IO ()

foreign import capi unsafe "sha1.h hs_cryptohash_sha1_finalize"
    c_sha1_finalize_len :: Ptr Ctx -> Ptr Word8 -> IO Word64

foreign import capi unsafe "sha1.h hs_cryptohash_sha1_hash"
    c_sha1_hash_unsafe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi safe "sha1.h hs_cryptohash_sha1_hash"
    c_sha1_hash_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()
