{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cookie.JarSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Cookie.WaiSpec (spec) where

import Control.Monad (join)
import Control.Monad.Cont (cont, runCont)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, intDec, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.String (fromString)
import Data.String.Conv (toS)
import Data.Time (Day (..), UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime
  )
import Data.Word (Word16)
import Network.HTTP.Client
  ( Cookie (..)
  , CookieJar
  , Manager
  , Response (..)
  , createCookieJar
  , defaultManagerSettings
  , destroyCookieJar
  , httpLbs
  , newManager
  , parseRequest
  )
import Network.HTTP.Types.Header (ResponseHeaders, hSetCookie)
import Network.HTTP.Types.Status (Status (statusCode), status200)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Port, testWithApplication)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , chooseInteger
  , listOf1
  , suchThat
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import Web.Cookie (SetCookie (..), defaultSetCookie, renderSetCookieBS)
import Web.Cookie.Jar


spec :: Spec
spec = describe "Module Web.Cookie.Jar" $ do
  context "usingCookiesFromFile" $ around useTmpAndCookieApp $ do
    context "when applied to a request that returns a cookie" $ do
      it "should store it in the cookie jar" prop_fetchUsingCookiesUpdatesJar


useTmp :: (FilePath -> IO a) -> IO a
useTmp = withSystemTempDirectory "web-cookiejar"


useTmpAndCookieApp :: ((FilePath, Port, Manager) -> IO a) -> IO a
useTmpAndCookieApp = runCont $ do
  manager <- cont (mkManager >>=)
  cookieDir <- cont useTmp
  port <- cont withCookieApp
  pure (cookieDir, port, manager)


prop_fetchUsingCookiesUpdatesJar :: (FilePath, Port, Manager) -> Property
prop_fetchUsingCookiesUpdatesJar (root, port, manager) = monadicIO $ do
  core <- pick genCore
  (jarBase, _jar) <- pick genJarWithPath
  let jarPath = root ++ "/" ++ jarBase
      testUrl = asLocalHostUrl port core

  (stored, ok) <- run $ do
    ok <- (== 200) <$> fetch jarPath manager testUrl
    jar <- readJarX jarPath
    let stored = isIn core jar
    pure (stored, ok)
  assert $ stored && ok


fetch :: FilePath -> Manager -> ByteString -> IO Int
fetch cookiePath manager url = do
  let doReq = usingCookiesFromFile' cookiePath $ flip httpLbs manager
  rq <- parseRequest $ toS url
  statusCode . responseStatus <$> doReq rq


genJarWithPath :: Gen (FilePath, CookieJar)
genJarWithPath = do
  let mkPath i = "cookie-jar-" ++ i ++ ".txt"
  index <- mkPath . show <$> genWord16
  (,) index <$> genCookieJar


genCookieJar :: Gen CookieJar
genCookieJar = createCookieJar <$> listOf1 genCookie


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


genWord16 :: Gen Word16
genWord16 = arbitrary


genUTCTime :: Gen UTCTime
genUTCTime = posixSecondsToUTCTime . fromInteger <$> chooseInteger (1, 365 * 86400 * 20)


genWithSuffix :: ByteString -> Gen ByteString
genWithSuffix bs = (bs <>) . fromString . show <$> genWord16


withCookieApp :: (Port -> IO a) -> IO a
withCookieApp = testWithApplication (pure cookieApp)


cookieApp :: Application
cookieApp rq respond =
  let headers = maybe [] applyCookieCmd $ mbCookieCmd rq
   in respond $ Wai.responseLBS status200 headers "ok"


applyCookieCmd :: CookieCmd -> ResponseHeaders
applyCookieCmd cmd =
  let theCookie = case cmd of
        Clear x ->
          defaultSetCookie
            { setCookieName = x
            , setCookieMaxAge = Just $ secondsToDiffTime 0
            , setCookieExpires = Just expireTime
            }
        Change x y -> defaultSetCookie {setCookieName = x, setCookieValue = y}
   in [(hSetCookie, renderSetCookieBS theCookie)]


{--
look for cookieName param
look for cookieValue param

if no cookieName exists: Nothing
if cookieName exists but not CookieCmd: (Just $ Clear cookieName)
if both exist (Just $ Change cookieName cookieValue)
-}
mbCookieCmd :: Wai.Request -> Maybe CookieCmd
mbCookieCmd req =
  let valueOfMb p = join (lookup p (Wai.queryString req))
      nameItem = valueOfMb nameParam
      valueItem = valueOfMb valueParam
   in case (nameItem, valueItem) of
        (Nothing, _) -> Nothing
        (Just x, Nothing) -> Just (Clear x)
        (Just x, Just y) -> Just (Change x y)


-- | Arbitrary cookie expiry time set back in history after unix time 0
expireTime :: UTCTime
expireTime = UTCTime (ModifiedJulianDay 50000) 0


genPathNameValue :: Gen (ByteString, ByteString, ByteString)
genPathNameValue = do
  base <- genWithSuffix "base"
  path <- genWithSuffix $ "test/path/" <> base <> "/"
  name <- genWithSuffix "name_"
  value <- genWithSuffix "value_"
  pure (path, name, value)


genCore :: Gen Core
genCore =
  let mk (cPath, cName, cValue) = Core {cPath, cName, cValue}
   in mk <$> genPathNameValue


asLocalHostUrl :: Port -> Core -> ByteString
asLocalHostUrl port Core {cPath, cName, cValue} =
  let builder =
        "http://localhost"
          <> ":"
          <> intDec port
          <> "/"
          <> byteString cPath
          <> "?"
          <> byteString nameParam
          <> "="
          <> byteString cName
          <> "&"
          <> byteString valueParam
          <> "="
          <> byteString cValue
   in L.toStrict $ toLazyByteString builder


isIn :: Core -> CookieJar -> Bool
isIn core jar =
  let matches Core {cPath, cName, cValue} c =
        BS.isPrefixOf (cookie_path c) ("/" <> cPath)
          && cName == cookie_name c
          && cValue == cookie_value c
   in any (matches core) (destroyCookieJar jar)


nameParam, valueParam :: ByteString
nameParam = "cookieName"
valueParam = "cookieValue"


mkManager :: IO Manager
mkManager = newManager defaultManagerSettings


data Core = Core
  { cPath :: !ByteString
  , cName :: !ByteString
  , cValue :: !ByteString
  }
  deriving (Eq, Show)


data CookieCmd
  = Change
      !ByteString
      !ByteString
  | Clear
      !ByteString
  deriving (Eq, Show)
