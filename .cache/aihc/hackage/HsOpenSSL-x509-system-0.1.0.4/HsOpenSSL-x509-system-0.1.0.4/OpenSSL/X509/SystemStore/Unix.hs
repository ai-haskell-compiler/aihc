{-# LANGUAGE ScopedTypeVariables #-}

module OpenSSL.X509.SystemStore.Unix
    ( contextLoadSystemCerts
    ) where

import OpenSSL.Session (SSLContext, contextSetCADirectory, contextSetCAFile)
import qualified System.Posix.Files as U
import Control.Exception (try, IOException)
import System.IO.Unsafe (unsafePerformIO)

contextLoadSystemCerts :: SSLContext -> IO ()
contextLoadSystemCerts =
    unsafePerformIO $ loop defaultSystemPaths
  where
    loop ((isDir, path) : rest) = do
        mst <- try $ U.getFileStatus path
            :: IO (Either IOException U.FileStatus)
        case mst of
            Right st | isDir, U.isDirectory st ->
                return (flip contextSetCADirectory path)
            Right st | not isDir, U.isRegularFile st ->
                return (flip contextSetCAFile path)
            _ -> loop rest
    loop [] = return (const $ return ()) -- throw an exception instead?
{-# NOINLINE contextLoadSystemCerts #-}

-- A True value indicates that the path must be a directory.
-- According to [1], the fedora path should be tried before /etc/ssl/certs
-- because of [2].
--
-- [1] https://www.happyassassin.net/2015/01/12/a-note-about-ssltls-trusted-certificate-stores-and-platforms/
-- [2] https://bugzilla.redhat.com/show_bug.cgi?id=1053882
defaultSystemPaths :: [(Bool, FilePath)]
defaultSystemPaths =
    [ (False, "/etc/pki/tls/certs/ca-bundle.crt"      ) -- red hat, fedora, centos
    , (True , "/etc/ssl/certs"                        ) -- other linux, netbsd
    , (True , "/system/etc/security/cacerts"          ) -- android
    , (False, "/etc/ssl/cert.pem"                     ) -- openbsd/freebsd
    , (False, "/usr/share/ssl/certs/ca-bundle.crt"    ) -- older red hat
    , (False, "/usr/local/share/certs/ca-root-nss.crt") -- freebsd (security/ca-root-nss)
    , (True , "/usr/local/share/certs"                ) -- freebsd
    ]
