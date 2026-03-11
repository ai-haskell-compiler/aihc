module OpenSSL.X509.SystemStore.Win32
    ( contextLoadSystemCerts
    ) where

import Control.Exception (bracket)
import Control.Monad (when, (>=>))
import OpenSSL.X509 (X509)
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.X509.Store as SSL
import qualified OpenSSL.EVP.Base64 as SSL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Foreign (Ptr, nullPtr, peekByteOff)
import System.Win32.Types (DWORD, BOOL, LPCTSTR, withTString)

contextLoadSystemCerts :: SSL.SSLContext -> IO () 
contextLoadSystemCerts ctx = do
    st <- SSL.contextGetCAStore ctx
    iterCertStoreX509 "ROOT" (SSL.addCertToStore st)

iterCertStoreX509 :: String -> (X509 -> IO ()) -> IO ()
iterCertStoreX509 subsystemProtocol action =
    iterCertStorePEM subsystemProtocol (SSL.readX509 >=> action)

iterCertStorePEM :: String -> (String -> IO ()) -> IO ()
iterCertStorePEM subsystemProtocol action =
    iterCertStoreDER subsystemProtocol (action . derToPem)

iterCertStoreDER :: String -> (B.ByteString -> IO ()) -> IO ()
iterCertStoreDER subsystemProtocol action =
    withTString subsystemProtocol $ \ssProtPtr ->
        bracket
            (certOpenSystemStore nullPtr ssProtPtr)
            (flip certCloseStore 0)
            (loop nullPtr)
  where
    loop prevCertCtx certStore = do
        certCtx <- certEnumCertificatesInStore certStore prevCertCtx
        when (certCtx /= nullPtr) $ do
            certEncType <- (#peek struct _CERT_CONTEXT, dwCertEncodingType) certCtx
            when (certEncType == x509EncType) $ do
                len <- (#peek struct _CERT_CONTEXT, cbCertEncoded) certCtx :: IO DWORD
                certBuf <- (#peek struct _CERT_CONTEXT, pbCertEncoded) certCtx
                cert <- B.packCStringLen (certBuf, fromIntegral len)
                action cert
                loop certCtx certStore

derToPem :: B.ByteString -> String
derToPem der = unlines ([beginCert] ++ ls ++ [endCert])
  where
    ls = map C8.unpack $ splitChunks $ SSL.encodeBase64BS der
    splitChunks s
        | B.null s = []
        | otherwise = chunk : splitChunks rest
          where
            (chunk, rest) = B.splitAt 64 s
    beginCert = "-----BEGIN CERTIFICATE-----"
    endCert = "-----END CERTIFICATE-----"

--------------------------------------------------------------------------------

#include <windows.h>
#include <Wincrypt.h>

data HCERTSTORE

data PCCERT_CONTEXT

data HCRYPTPROV_LEGACY

foreign import stdcall unsafe "CertOpenSystemStoreW"
    certOpenSystemStore
        :: Ptr HCRYPTPROV_LEGACY
        -> LPCTSTR
        -> IO (Ptr HCERTSTORE)

foreign import stdcall unsafe "CertCloseStore"
    certCloseStore :: Ptr HCERTSTORE -> DWORD -> IO BOOL

foreign import stdcall unsafe "CertEnumCertificatesInStore"
    certEnumCertificatesInStore
        :: Ptr HCERTSTORE
        -> Ptr PCCERT_CONTEXT
        -> IO (Ptr PCCERT_CONTEXT)

x509EncType :: DWORD
x509EncType = (#const X509_ASN_ENCODING)
