{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest
 (
   -- * Digest Algorithms
   Algorithm
 , digestByName, digestByName', digestSize, maxDigestSize, digestBlockSize
 , UnknownAlgorithm
 , -- * Digest Contexts
   Context, newContext, freeContext, resetDigest
 , -- * Digest Computations
   initDigest, updateDigest, finalizeDigest
 )
 where

import OpenSSL.EVP.Digest.Algorithm
import OpenSSL.EVP.Digest.Context
import OpenSSL.EVP.Digest.Digest
import OpenSSL.EVP.Digest.Error
