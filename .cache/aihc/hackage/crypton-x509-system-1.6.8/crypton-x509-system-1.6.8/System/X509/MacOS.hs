module System.X509.MacOS (
    getSystemCertificateStore,
) where

import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.PEM (PEM (..), pemParseLBS)
import System.Process

import Data.X509
import Data.X509.CertificateStore
import System.X509.Common (maybeSSLCertEnvOr)

rootCAKeyChain :: FilePath
rootCAKeyChain = "/System/Library/Keychains/SystemRootCertificates.keychain"

systemKeyChain :: FilePath
systemKeyChain = "/Library/Keychains/System.keychain"

listInKeyChains :: [FilePath] -> IO [SignedCertificate]
listInKeyChains keyChains = do
    (_, Just hout, _, ph) <-
        createProcess
            (proc "security" ("find-certificate" : "-pa" : keyChains))
                { std_out = CreatePipe
                }
    pems <- either error id . pemParseLBS <$> LBS.hGetContents hout
    let targets =
            rights $
                map (decodeSignedCertificate . pemContent) $
                    filter ((== "CERTIFICATE") . pemName) pems
    _ <- targets `seq` waitForProcess ph
    return targets

getSystemCertificateStore :: IO CertificateStore
getSystemCertificateStore =
    maybeSSLCertEnvOr
        (makeCertificateStore <$> listInKeyChains [rootCAKeyChain, systemKeyChain])
