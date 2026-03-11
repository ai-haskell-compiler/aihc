{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Certs.TempSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Certs.TempSpec (spec) where

import Data.Either (isRight)
import Network.TLS (credentialLoadX509)
import System.Directory (getCurrentDirectory)
import Test.Certs.Temp
import Test.Hspec


spec :: Spec
spec = describe "Test.Certs.Temp" $ do
  context "loading certificates generated via" $ do
    context "withCertsPathInTmp" $ do
      it "should succeed withCertsPathsInTmp" $ do
        shouldGenerateCerts $ withCertPathsInTmp testConfig

    context "withCertsPathInTmp'" $ do
      it "should succeed" $ do
        shouldGenerateCerts withCertPathsInTmp'

    context "withCertsPath" $ do
      it "should succeed" $ do
        cwd <- getCurrentDirectory
        shouldGenerateCerts $ withCertPaths cwd testConfig

    context "withCertFilenames" $ do
      it "should succeed" $ do
        cwd <- getCurrentDirectory
        shouldGenerateCerts $ withCertFilenames altBasenames cwd testConfig


shouldGenerateCerts :: ((CertPaths -> IO Bool) -> IO Bool) -> IO ()
shouldGenerateCerts actionF = actionF hasCertsAt >>= (`shouldBe` True)


hasCertsAt :: CertPaths -> IO Bool
hasCertsAt cp = do
  let cert = certificatePath cp
      key = keyPath cp
  isRight <$> credentialLoadX509 cert key


-- | A default value for @'Config'@
testConfig :: Config
testConfig =
  Config
    { cCountry = Just "JP"
    , cProvince = Just "Fukuoka"
    , cCity = Just "Itoshima"
    , cOrganization = Just "haskell:test-certs"
    , cCommonName = "localhost"
    , cDurationDays = 365
    }


altBasenames :: FilePath -> CertPaths
altBasenames cpDir =
  CertPaths
    { cpDir
    , cpKey = "privkey.pem"
    , cpCert = "cert.pem"
    }
