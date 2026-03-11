{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cookie.JarSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Cookie.JarSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.List (sortBy)
import Data.String (fromString)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime
  )
import Data.Word (Word16)
import Network.HTTP.Client
  ( Cookie (..)
  , CookieJar
  , compareCookies
  , createCookieJar
  , destroyCookieJar
  , equalCookie
  )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , chooseInteger
  , forAll
  , listOf1
  , suchThat
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import Web.Cookie.Jar


spec :: Spec
spec = describe "Module Web.Cookie.Jar" $ do
  context "parsing with cookieParser" $ do
    context "and building with cookieBuilder" $ do
      it "should almost roundtrip" prop_almostRoundtripCookie

  context "parsing with cookieJarParser" $ do
    context "and building with jarBuilder" $ do
      it "should almost roundtrip" (prop_almostRoundtripCookieJar jarBuilder)

    context "and building netscapeJarBuilder" $ do
      it "should almost roundtrip" (prop_almostRoundtripCookieJar netscapeJarBuilder)

  context "when accessing persisted jars" $ around useTmp $ do
    context "writeJar then readJar" $ do
      it "should almost roundtrip" (prop_almostRoundtripSavedJars writeJar)
    context "writeNetscapeJar then readJar" $ do
      it "should almost roundtrip" (prop_almostRoundtripSavedJars writeNetscapeJar)


useTmp :: (FilePath -> IO a) -> IO a
useTmp = withSystemTempDirectory "web-cookiejar"


genJarWithPath :: Gen (FilePath, CookieJar)
genJarWithPath = do
  let mkPath i = "cookie-jar-" ++ i ++ ".txt"
  index <- mkPath . show <$> genWord16
  (,) index <$> genCookieJar


prop_almostRoundtripSavedJars :: (FilePath -> CookieJar -> IO ()) -> FilePath -> Property
prop_almostRoundtripSavedJars writer root = monadicIO $ do
  (jarBase, jar) <- pick genJarWithPath
  let jarPath = root ++ "/" ++ jarBase

  -- this match is incomplete, that's ok, the test fails if it produces a Left
  Right jar' <- run $ do
    writer jarPath jar
    readJar jarPath
  assert $ almostEqJar jar jar'


prop_almostRoundtripCookieJar :: (CookieJar -> Builder) -> Property
prop_almostRoundtripCookieJar toBuilder =
  forAll (cookieJarWithX toBuilder <$> genCookieJar) $ \(j, _txt, j') ->
    either (const False) (almostEqJar j) j'


cookieJarWithX
  :: (CookieJar -> Builder)
  -> CookieJar
  -> (CookieJar, ByteString, Either String CookieJar)
cookieJarWithX toBuilder j =
  let txt = asByteString $ toBuilder j
   in (j, txt, parseCookieJar txt)


genCookieJar :: Gen CookieJar
genCookieJar = createCookieJar <$> listOf1 genCookie


almostEqJar :: CookieJar -> CookieJar -> Bool
almostEqJar jar1 jar2 =
  let
    cookiesOf = sortBy compareCookies . map fixup . destroyCookieJar
   in
    and $ zipWith equalCookie (cookiesOf jar1) (cookiesOf jar2)


prop_almostRoundtripCookie :: Property
prop_almostRoundtripCookie =
  forAll (cookieWithLine <$> genCookie) $ \(c, line) ->
    either (const False) (almostEq c) $ parseOnly cookieParser line


asByteString :: Builder -> ByteString
asByteString = L.toStrict . toLazyByteString


cookieWithLine :: Cookie -> (Cookie, ByteString)
cookieWithLine c = (c, asByteString $ cookieBuilder c)


genCookie :: Gen Cookie
genCookie = do
  (creation, expiry) <- genCreationAndExpiry
  Cookie
    <$> genWithSuffix "name_"
    <*> genWithSuffix "value_"
    <*> pure expiry
    <*> genWithSuffix "domain_"
    <*> genWithSuffix "path_"
    <*> pure creation
    <*> pure creation
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


genCreationAndExpiry :: Gen (UTCTime, UTCTime)
genCreationAndExpiry = do
  creation <- genUTCTime
  expiry <- genUTCTime `suchThat` (> creation)
  pure (creation, expiry)


almostEq :: Cookie -> Cookie -> Bool
almostEq c1 c2 = fixup c1 `equalCookie` fixup c2


fixup :: Cookie -> Cookie
fixup c =
  let epoch = posixSecondsToUTCTime 0
   in c
        { cookie_persistent = True
        , cookie_last_access_time = epoch
        , cookie_creation_time = epoch
        }


genWord16 :: Gen Word16
genWord16 = arbitrary


genUTCTime :: Gen UTCTime
genUTCTime = posixSecondsToUTCTime . fromInteger <$> chooseInteger (1, 365 * 86400 * 20)


genWithSuffix :: ByteString -> Gen ByteString
genWithSuffix bs = (bs <>) . fromString . show <$> genWord16
