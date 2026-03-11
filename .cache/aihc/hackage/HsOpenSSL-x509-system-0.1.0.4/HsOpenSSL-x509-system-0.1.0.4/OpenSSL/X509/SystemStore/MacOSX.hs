module OpenSSL.X509.SystemStore.MacOSX
    ( contextLoadSystemCerts
    ) where

import System.Process (createProcess, waitForProcess, proc,
                       CreateProcess(std_out), StdStream(CreatePipe))
import System.IO (hGetLine, hIsEOF)
import Control.Monad ((>=>))
import Control.Exception (throwIO, ErrorCall(ErrorCall))
import OpenSSL.Session (SSLContext, contextGetCAStore)
import OpenSSL.X509 (X509)
import OpenSSL.X509.Store (addCertToStore)
import OpenSSL.PEM (readX509)

contextLoadSystemCerts :: SSLContext -> IO () 
contextLoadSystemCerts ctx = do
    st <- contextGetCAStore ctx
    iterSystemCertsX509 (addCertToStore st)

iterSystemCertsX509 :: (X509 -> IO ()) -> IO ()
iterSystemCertsX509 action =
    iterSystemCertsPEM (readX509 >=> action)

iterSystemCertsPEM :: (String -> IO ()) -> IO ()
iterSystemCertsPEM action = do
    (_, Just hdl, _, ph) <- createProcess cmd {std_out = CreatePipe}
    loop [] hdl
    _ <- waitForProcess ph
    return ()
  where
    loop ls hdl = do
        eof <- hIsEOF hdl
        if not eof then do
            s <- hGetLine hdl
            let ls' = s : ls
            if s == endCert then do
                action (unlines $ reverse ls')
                loop [] hdl
            else
                loop ls' hdl
        else if null ls then
            return ()
        else
            throwIO $ ErrorCall "Incomplete certificate"
    endCert = "-----END CERTIFICATE-----"
    cmd = proc "security"
        ["export", "-t", "certs", "-f", "pemseq", "-k", rootCAKeyChain]
    rootCAKeyChain = "/System/Library/Keychains/SystemRootCertificates.keychain"
