{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest.Digest where

import OpenSSL.EVP.Digest.Algorithm
import OpenSSL.EVP.Digest.Context
import OpenSSL.EVP.Digest.Error ( throwIfZero )

import Foreign
import Foreign.C

-- | Configure the given digest context to use the given message digest
-- algorithm. Throws an exception to signal failure, i.e. because the system is
-- out of memory.

initDigest :: Algorithm -> Context -> IO ()
initDigest algo ctx =
  throwIfZero "OpenSSL.EVP.Digest.initDigest" (_initDigest ctx algo nullPtr)

-- | Hash the given block of memory and update the digest state accordingly.
-- This function can be called many times. Then use 'finalizeDigest' to
-- retrieve the actual hash value.

updateDigest :: Context -> Ptr a -> CSize -> IO ()
updateDigest ctx ptr len =
  throwIfZero "OpenSSL.EVP.Digest.updateDigest" (_updateDigest ctx ptr len)

-- | Finalize the digest calculation and return the result in the 'Word8' array
-- passed as an argument. Naturally, that array is expected to be large enough
-- to contain the digest. 'digestSize' or 'maxDigestSize' are your friends.
-- This function does /not/ clean up the digest context; this has to be done
-- with an explicit call to 'freeContext' (or 'resetContext', if you want to
-- re-use it). However, it does invalidate the digest state so that no further
-- calls of 'digestUpdate' can be made without re-initializing the context
-- first.

finalizeDigest :: Context -> Ptr Word8 -> IO ()
finalizeDigest ctx ptr =
  throwIfZero "OpenSSL.EVP.Digest.finalizeDigest" (_finalizeDigest ctx ptr nullPtr)

-------------------------------------------------------------------------------

-- | We don't support choosing a custom engine to implement the given
-- algorithm. This type is just a place holder, and we always pass 'nullPtr'
-- wherever it is required to let OpenSSL choose whatever engine it thinks is
-- best.

data OpaqueEngine

foreign import ccall unsafe "openssl/evp.h EVP_DigestInit_ex"
  _initDigest :: Context -> Algorithm -> Ptr OpaqueEngine -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_DigestUpdate"
  _updateDigest :: Context -> Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_DigestFinal_ex"
  _finalizeDigest :: Context -> Ptr Word8 -> Ptr CUInt -> IO CInt
