{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Test.Certs.Temp
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Enables configuration and generation of temporary certificates
-}
module Test.Certs.Temp
  ( -- * generate certificates
    withCertPaths
  , withCertFilenames
  , withCertPathsInTmp
  , withCertPathsInTmp'
  , generateAndStore

    -- * configuration
  , Config (..)
  , defaultConfig

    -- * certificate filenames
  , CertPaths (..)
  , keyPath
  , certificatePath
  , defaultBasenames
  )
where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Numeric.Natural (Natural)
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.RSA as SSL
import qualified OpenSSL.Random as SSL
import qualified OpenSSL.X509 as SSL
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)


-- | Specifies the location to write the temporary certificates
data CertPaths = CertPaths
  { cpKey :: !FilePath
  -- ^ the basename of the private key file
  , cpCert :: !FilePath
  -- ^ the basename of the certificate file
  , cpDir :: !FilePath
  -- ^ the directory containing the certificate files
  }
  deriving (Eq, Show)


-- | The path of the generated key file
keyPath :: CertPaths -> FilePath
keyPath cp = cpDir cp </> cpKey cp


-- | The path of the generated certificate file
certificatePath :: CertPaths -> FilePath
certificatePath cp = cpDir cp </> cpCert cp


{- | A @CertPaths using some default basenames for the certificate files.

defaults are:
* for 'cpKey', @key.pem@
* for 'cpCert', @certificate.pem@
-}
defaultBasenames :: FilePath -> CertPaths
defaultBasenames cpDir =
  CertPaths
    { cpDir
    , cpKey = "key.pem"
    , cpCert = "certificate.pem"
    }


-- | Configure some details of the generated certificates
data Config = Config
  { cCommonName :: !Text
  -- ^ the certificate common name
  , cDurationDays :: !Natural
  -- ^ the certificate's duration in days
  , cProvince :: !(Maybe Text)
  , cCity :: !(Maybe Text)
  , cOrganization :: !(Maybe Text)
  , cCountry :: !(Maybe Text)
  }
  deriving (Eq, Show)


-- | A default value for @'Config'@: CN=localhost, duration is 365 days.
defaultConfig :: Config
defaultConfig =
  Config
    { cCountry = Nothing
    , cProvince = Nothing
    , cCity = Nothing
    , cOrganization = Nothing
    , cCommonName = "localhost"
    , cDurationDays = 365
    }


asDistinguished :: Config -> [(String, String)]
asDistinguished c =
  let dnMaybe k f = (fmap ((k,) . Text.unpack) . f)
   in catMaybes
        [ dnMaybe "C" cCountry c
        , dnMaybe "ST" cProvince c
        , dnMaybe "L" cCity c
        , dnMaybe "O" cOrganization c
        , dnMaybe "CN" (Just . cCommonName) c
        ]


validityNow :: Natural -> IO (UTCTime, UTCTime)
validityNow ndays = do
  start <- getCurrentTime
  let end = (nominalDay * fromIntegral ndays) `addUTCTime` start
  pure (start, end)


testKeySize :: Int
testKeySize = 4096


testExponent :: Integer
testExponent = 257


genCerts :: Config -> IO (String, String)
genCerts config = do
  -- set up values to use in the certificate fields
  let mkSerialNum = BS.foldl (\a w -> a * 256 + fromIntegral w) 0
      distinguished = asDistinguished config
  serialNumber <- mkSerialNum <$> SSL.randBytes 8
  (start, end) <- validityNow $ cDurationDays config

  -- generate an RSA key pair
  kp <- SSL.generateRSAKey' testKeySize $ fromIntegral testExponent

  -- create and sign a certificate using the private key of the key pair
  cert <- SSL.newX509
  SSL.setVersion cert 2
  SSL.setSerialNumber cert serialNumber
  SSL.setIssuerName cert distinguished
  SSL.setSubjectName cert distinguished
  SSL.setNotBefore cert start
  SSL.setNotAfter cert end
  SSL.setPublicKey cert kp
  SSL.signX509 cert kp Nothing

  -- the PEM representation of the private key
  privString <- SSL.writePKCS8PrivateKey kp Nothing

  -- the PEM representation of the certificate
  certString <- SSL.writeX509 cert

  pure (certString, privString)


storeCerts :: CertPaths -> String -> String -> IO ()
storeCerts cp rsaKey signedCert = do
  writeFile (keyPath cp) rsaKey
  writeFile (certificatePath cp) signedCert


-- | Generate and store certificate files as specified as @'CertPaths'@
generateAndStore :: CertPaths -> Config -> IO ()
generateAndStore cp config = do
  (certificate, privKey) <- genCerts config
  storeCerts cp privKey certificate


-- | Like 'withCertPaths', but allows the @CertPath@ filenames to be specified
withCertFilenames
  :: (FilePath -> CertPaths)
  -> FilePath
  -> Config
  -> (CertPaths -> IO a)
  -> IO a
withCertFilenames mkCertPath parentDir config useCerts =
  withTempDirectory parentDir "temp-certs" $ \tmpDir -> do
    let certPaths = mkCertPath tmpDir
    generateAndStore certPaths config
    useCerts certPaths


{- | Create certificates in a temporary directory below @parentDir@, specify the
locations using @CertPaths@, use them, then delete them
-}
withCertPaths :: FilePath -> Config -> (CertPaths -> IO a) -> IO a
withCertPaths = withCertFilenames defaultBasenames


-- | Like 'withCertPaths' with the system @TEMP@ dir as the @parentDir@
withCertPathsInTmp :: Config -> (CertPaths -> IO a) -> IO a
withCertPathsInTmp config action = do
  parentDir <- getCanonicalTemporaryDirectory
  withCertPaths parentDir config action


-- | Like 'withCertPathsInTmp' using a default @'Config'@
withCertPathsInTmp' :: (CertPaths -> IO a) -> IO a
withCertPathsInTmp' = withCertPathsInTmp defaultConfig
