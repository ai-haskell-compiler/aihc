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
-- Module      : Crypto.Hash.MD5.FFI
-- License     : BSD-3
--
module Crypto.Hash.MD5.FFI where

import           Data.ByteString (ByteString)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

-- | MD5 Context
--
-- The context data is exactly 88 bytes long, however
-- the data in the context is stored in host-endianness.
--
-- The context data is made up of
--
--  * a 'Word64' representing the number of bytes already feed to hash algorithm so far,
--
--  * a 64-element 'Word8' buffer holding partial input-chunks, and finally
--
--  * a 4-element 'Word32' array holding the current work-in-progress digest-value.
--
-- Consequently, a MD5 digest as produced by 'hash', 'hashlazy', or 'finalize' is 16 bytes long.
newtype Ctx = Ctx ByteString
  deriving (Eq)

foreign import capi unsafe "md5.h hs_cryptohash_md5_init"
    c_md5_init :: Ptr Ctx -> IO ()

foreign import capi unsafe "md5.h hs_cryptohash_md5_update"
    c_md5_update_unsafe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()

foreign import capi safe "md5.h hs_cryptohash_md5_update"
    c_md5_update_safe :: Ptr Ctx -> Ptr Word8 -> CSize -> IO ()

foreign import capi unsafe "md5.h hs_cryptohash_md5_finalize"
    c_md5_finalize :: Ptr Ctx -> Ptr Word8 -> IO ()

foreign import capi unsafe "md5.h hs_cryptohash_md5_finalize"
    c_md5_finalize_len :: Ptr Ctx -> Ptr Word8 -> IO Word64

foreign import capi unsafe "md5.h hs_cryptohash_md5_hash"
    c_md5_hash_unsafe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()

foreign import capi safe "md5.h hs_cryptohash_md5_hash"
    c_md5_hash_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()
