{-# LANGUAGE ForeignFunctionInterface #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest.Algorithm where

import OpenSSL.EVP.Digest.Initialization
import OpenSSL.EVP.Digest.Error ( UnknownAlgorithm(..) )

import Control.Exception
import Data.Maybe
import Data.String ( IsString(..) )
import Foreign
import Foreign.C
import System.IO.Unsafe as IO

#include "openssl/opensslv.h"
#include "openssl/evp.h"

-- | An opaque handle into OpenSSL's collection of message digest algorithms.
-- Use 'digestByName' to look up any of the available algorithms by name. For
-- the sake of convenience, 'Algorithm' is an instance of 'IsString' so
-- that the compiler can transparently map 'String' literals to algorithms via
-- 'fromString' if the @XOverloadedStrings@ extension is enabled.
--
-- >>> fromString "sha256" == digestByName "sha256"
-- True

newtype Algorithm = Algorithm (Ptr ())
  deriving (Show, Eq)

instance IsString Algorithm where
  fromString = digestByName

-- | Look up a digest algorithm engine by name. Algorithms usually offered by
-- OpenSSL are "md2", "md5", "sha1", "mdc2", "ripemd160", "blake2b512",
-- "blake2s256", "sha224", "sha256", "sha384", and "sha512", but the exact set
-- may vary between platforms. Throws 'UnknownAlgorithm' if the requested
-- algorithm is not known.

digestByName :: String -> Algorithm
digestByName algo =
  fromMaybe (throw (UnknownAlgorithm algo)) (digestByName' algo)

-- | Variant of 'digestByName' that signals failure by evaluating to 'Nothing'
-- rather than failing.
--
-- >>> digestByName' "sha256" == Just (digestByName "sha256")
-- True
-- >>> digestByName' "Guess what?" :: Maybe Algorithm
-- Nothing

digestByName' :: String -> Maybe Algorithm
digestByName' algo = do
  let Algorithm p = IO.unsafePerformIO $ do
                      initializeEVPDigests
                      withCString algo (return . _digestByName)
  if p == nullPtr then Nothing else Just (Algorithm p)

-- | Return the size of the digest in bytes that the given algorithm will produce.
--
-- >>> digestSize (digestByName "sha256")
-- 32

digestSize :: Algorithm -> Int
digestSize = fromIntegral . _digestSize

-- | The largest possible digest size of any of the algorithms supported by
-- this library will generate. So if you want to store a digest without
-- bothering to retrieve the appropriate size with 'digestSize' first, allocate
-- a buffer of that size.

maxDigestSize :: Int
maxDigestSize = #{const EVP_MAX_MD_SIZE}

-- | Return the block size the the given algorithm operates with.
--
-- >>> digestBlockSize (digestByName "sha256")
-- 64

digestBlockSize :: Algorithm -> Int
digestBlockSize = fromIntegral . _digestBlockSize

-------------------------------------------------------------------------------

foreign import ccall unsafe "openssl/evp.h EVP_get_digestbyname"
  _digestByName :: CString -> Algorithm

foreign import ccall unsafe
#if OPENSSL_VERSION_MAJOR < 3
  "openssl/evp.h EVP_MD_size"
#else
  "openssl/evp.h EVP_MD_get_size"
#endif
  _digestSize :: Algorithm -> CInt

foreign import ccall unsafe
#if OPENSSL_VERSION_MAJOR < 3
  "openssl/evp.h EVP_MD_block_size"
#else
  "openssl/evp.h EVP_MD_get_block_size"
#endif
  _digestBlockSize :: Algorithm -> CInt
