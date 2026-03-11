{-# LANGUAGE DeriveDataTypeable #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest.Error where

import Control.Exception
import Data.Typeable ( Typeable )
import Foreign
import Foreign.C

-- | Most OpenSSL functions return an approximation of @Bool@ to signify
-- failure. This wrapper makes it easier to move the error handling to the
-- exception layer where appropriate.

throwIfZero :: String -> IO CInt -> IO ()
throwIfZero fname =
  throwIf_ (==0) (const (showString fname " failed with error code 0"))

-- | A custom exception type which is thrown by 'digestByName' in case the
-- requested digest algorithm is not available in the OpenSSL system library.

newtype UnknownAlgorithm = UnknownAlgorithm String
  deriving (Show, Typeable)

instance Exception UnknownAlgorithm
