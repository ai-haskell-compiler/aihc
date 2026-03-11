{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest.Context where

import OpenSSL.EVP.Digest.Error ( throwIfZero )

import Control.Monad
import Foreign
import Foreign.C

#include "openssl/opensslv.h"

-- | A context for digest computations. Use 'newContext' and 'freeContext' to
-- allocate/deallocate this type.

newtype Context = Context (Ptr ())
  deriving (Show, Eq)

-- | Allocate and initialize an 'Context' for use in a digest computation
-- on the heap. Release its underlying memory after use with 'freeContext'.

newContext :: IO Context
newContext = do ctx@(Context p) <- _newContext
                when (p == nullPtr) (fail "OpenSSL.EVP.Digest.Context.newContext failed")
                return ctx

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x1010000f
  "openssl/evp.h EVP_MD_CTX_create"
#else
  "openssl/evp.h EVP_MD_CTX_new"
#endif
  _newContext :: IO Context

-- | Release all resources associated with a digest computation.

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x1010000f
  "openssl/evp.h EVP_MD_CTX_destroy"
#else
  "openssl/evp.h EVP_MD_CTX_free"
#endif
  freeContext :: Context -> IO ()

-- | Free all resources associated with this 'Context', but don't destroy the
-- context itself so that it can be re-used for a new digest computation.

resetDigest :: Context -> IO ()
resetDigest ctx =
  throwIfZero "OpenSSL.EVP.Digest.resetDigest" (_resetContext ctx)

foreign import ccall unsafe
#if OPENSSL_VERSION_NUMBER < 0x1010000f
  "openssl/evp.h EVP_MD_CTX_cleanup"
#else
  "openssl/evp.h EVP_MD_CTX_reset"
#endif
  _resetContext :: Context -> IO CInt
