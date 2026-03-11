{-# LANGUAGE ForeignFunctionInterface #-}
module Crypto.OpenSSL.ASN1
    ( Nid(..)
    , asn1Description
    ) where

import          Foreign.Ptr 
import          Foreign.C.Types
import          Foreign.C.String
import          Crypto.OpenSSL.Misc
import          Control.Applicative

-- | openssl ASN1 unique identifier
newtype Nid = Nid Int

foreign import ccall unsafe "OBJ_txt2nid"
    _obj_txt2nid :: Ptr CChar -> IO CInt

asn1Description :: String -> Maybe Nid
asn1Description s = doIO $
    (mnid <$> withCString s (_obj_txt2nid))
  where mnid 0 = Nothing
        mnid i = Just $ Nid (fromIntegral i)

