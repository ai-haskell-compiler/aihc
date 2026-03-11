{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Email.Check where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Foreign.C
import Foreign.C.Types
import Text.Email.Validate (validate,domainPart,EmailAddress)

#include "check-mx.h"

foreign import ccall "check-mx.h check_mx" checkMx
    :: CString -> IO CInt

-- | Check to see whether an email is (1) RFC-valid and (2) has an
-- existant MX record.
check :: ByteString -> IO (Either String EmailAddress)
check email = do
  case validate email of
    Left e -> return $ Left $ show e
    Right e -> do exists <- S.useAsCString (domainPart e) checkMx
                  if exists /= 0
                     then return $ Right e
                     else return $ Left $ "no MX record exists for domain "
                                          ++ S8.unpack (domainPart e)
