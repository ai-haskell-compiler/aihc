{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status500, statusCode)
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.WarpTLS (tlsSettings)
import Network.Wai.Middleware.Delegate
  ( ProxySettings (..)
  , defaultSettings
  , delegateToProxy
  )
import System.Environment (lookupEnv)
import System.IO
import Test.Certs.Temp (certificatePath, keyPath, withCertPathsInTmp')
import Test.Fetch (fetch)
import Test.Hspec
import Test.Hspec.TmpProc
  ( HList (..)
  , HandlesOf
  , HostIpAddress
  , Pinged (..)
  , Proc (..)
  , ProcHandle
  , SvcURI
  , hAddr
  , handleOf
  , tdescribe
  , toPinged
  , (&:)
  , (&:&)
  )
import qualified Test.Hspec.TmpProc as TmpProc
import Test.HttpReply
import Test.NginxGateway (NginxGateway (..), mkBadTlsManager)
import Test.TestRequests
  ( RequestBuilder (..)
  , buildRequest
  , nil
  , secure
  , testNotProxiedRequests
  , testOverRedirectedRequests
  , testRequests
  )


defaultTestSettings :: ProxySettings
defaultTestSettings = defaultSettings {proxyHost = "httpbin.org", proxyTimeout = 2}


redirectTestSettings :: ProxySettings
redirectTestSettings = defaultTestSettings {proxyRedirectCount = 2}


setProxyHost :: ByteString -> ProxySettings -> ProxySettings
setProxyHost proxyHost ps = ps {proxyHost}


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  dumpDebug <- isJust <$> lookupEnv "DEBUG"
  hspec $ tdescribe "accessing http-bin in docker" $ do
    forM_ insecureRequestSpecs $ flip runRequestSpecsTest dumpDebug
    forM_ secureRequestSpecs $ flip runRequestSpecsTest dumpDebug


mkSampleApp :: ProxySettings -> IO Application
mkSampleApp s = do
  -- delegate everything but /status/418
  let handleFunnyStatus req = rawPathInfo req /= "/status/418"
      dummyApp _ respond = respond $ responseLBS status500 [] "I should have been proxied"
  manager <- mkBadTlsManager
  return $ delegateToProxy s manager handleFunnyStatus dummyApp


mkSampleApp' :: (HostOf a) => ProxySettings -> HandlesOf a -> IO Application
mkSampleApp' settings f = mkSampleApp $ setProxyHost (hostOf f) settings


testWithSecureProxy :: ((ReverseProxyFixture, Port) -> IO a) -> IO a
testWithSecureProxy action = withCertPathsInTmp' $ \cp -> do
  let tls = tlsSettings (certificatePath cp) (keyPath cp)
      app = mkSampleApp' defaultTestSettings
  TmpProc.testWithTLSApplication tls nginxAndHttpBin app action


onDirectAndProxy :: (HttpReply -> HttpReply -> IO ()) -> Bool -> Int -> RequestBuilder -> IO ()
onDirectAndProxy f debug testProxyPort builder = do
  let proxiedBuilder = builder {rbHost = "localhost", rbPort = Just testProxyPort}
  directReq <- buildRequest builder
  proxiedReq <- buildRequest proxiedBuilder

  when debug $ do
    putStrLn "---------------"
    putStrLn "Direct Request:"
    putStrLn "---------------"
    print directReq
    putStrLn "----------------"
    putStrLn "Proxied Request:"
    putStrLn "----------------"
    print proxiedReq
  proxied <- fetch proxiedReq
  direct <- fetch directReq
  when debug $ do
    putStrLn "Direct:"
    putStrLn "-------"
    print direct
    putStrLn "Proxied:"
    putStrLn "--------"
    print proxied
  f direct proxied


check ::
  (HostOf a) =>
  (HttpReply -> HttpReply -> IO ()) ->
  (RequestBuilder -> RequestBuilder) ->
  Bool ->
  RequestBuilder ->
  (HandlesOf a, Int) ->
  IO ()
check assertReplies modifier debug core (f, p) =
  let
    builder = modifier $ hostBuilder f core
   in
    onDirectAndProxy assertReplies debug p builder


type RequestSpecs = [(String, RequestBuilder -> RequestBuilder)]


data RequestSpecsTest a = RequestSpecsTest
  { stToDesc :: String -> String
  , stSettings :: ProxySettings
  , stWithApp :: forall b. HList a -> ProxySettings -> ((HandlesOf a, Port) -> IO b) -> IO b
  , stAssertReplies :: HttpReply -> HttpReply -> IO ()
  , stProc :: HList a
  , stScheme :: String
  , stCore :: RequestBuilder
  , stRequestSpecs :: RequestSpecs
  }


withSecureApp ::
  (HostOf a, TmpProc.AreProcs a) =>
  HList a ->
  ProxySettings ->
  ((HandlesOf a, Port) -> IO b) ->
  IO b
withSecureApp procs settings action = withCertPathsInTmp' $ \cp -> do
  let tls = tlsSettings (certificatePath cp) (keyPath cp)
      app = mkSampleApp' settings
  TmpProc.testWithTLSApplication tls procs app action


withInsecureApp ::
  (HostOf a, TmpProc.AreProcs a) =>
  HList a ->
  ProxySettings ->
  ((HandlesOf a, Port) -> IO b) ->
  IO b
withInsecureApp procs settings = TmpProc.testWithApplication procs $ mkSampleApp' settings


runRequestSpecsTest ::
  (TmpProc.AreProcs procs, HostOf procs) =>
  RequestSpecsTest procs ->
  Bool ->
  Spec
runRequestSpecsTest st debug =
  let RequestSpecsTest
        { stToDesc
        , stSettings
        , stAssertReplies
        , stProc
        , stScheme
        , stCore
        , stRequestSpecs
        , stWithApp
        } = st
      desc = stToDesc stScheme
      withApp = stWithApp stProc stSettings
   in aroundAll withApp $ describe desc $ do
        for_ stRequestSpecs $ \(title, modifier) -> do
          it (stScheme ++ " " ++ title) $ check stAssertReplies modifier debug stCore


insecureRequestSpecs :: [RequestSpecsTest '[HttpBin]]
insecureRequestSpecs = [insecureRedirects, insecureNotProxied, insecureProxy]


insecureProxy :: RequestSpecsTest '[HttpBin]
insecureProxy =
  RequestSpecsTest
    { stToDesc = \s -> "Simple " ++ s ++ " proxying:"
    , stSettings = defaultTestSettings
    , stAssertReplies = assertHttpRepliesAreEq
    , stProc = onlyHttpBin
    , stScheme = "HTTP"
    , stCore = nil
    , stRequestSpecs = testRequests
    , stWithApp = withInsecureApp
    }


insecureRedirects :: RequestSpecsTest '[HttpBin]
insecureRedirects =
  RequestSpecsTest
    { stToDesc = \s -> "Proxy over " ++ s ++ " with too many redirects differs"
    , stSettings = redirectTestSettings
    , stAssertReplies = assertHttpRepliesDiffer
    , stProc = onlyHttpBin
    , stScheme = "HTTP"
    , stCore = nil
    , stRequestSpecs = testOverRedirectedRequests
    , stWithApp = withInsecureApp
    }


insecureNotProxied :: RequestSpecsTest '[HttpBin]
insecureNotProxied =
  RequestSpecsTest
    { stToDesc = \s -> "Proxy on " ++ s ++ " should fail"
    , stSettings = defaultTestSettings
    , stAssertReplies = assertHttpRepliesDiffer
    , stProc = onlyHttpBin
    , stScheme = "HTTP"
    , stCore = nil
    , stRequestSpecs = testNotProxiedRequests
    , stWithApp = withInsecureApp
    }


secureRequestSpecs :: [RequestSpecsTest '[NginxGateway, HttpBin]]
secureRequestSpecs = [secureNotProxied, secureProxy]


secureNotProxied :: RequestSpecsTest '[NginxGateway, HttpBin]
secureNotProxied =
  RequestSpecsTest
    { stToDesc = \s -> "Proxy on " ++ s ++ " should fail"
    , stSettings = defaultTestSettings
    , stAssertReplies = assertHttpRepliesDiffer
    , stProc = nginxAndHttpBin
    , stScheme = "HTTPS"
    , stCore = sNil
    , stRequestSpecs = testNotProxiedRequests
    , stWithApp = withSecureApp
    }


secureProxy :: RequestSpecsTest '[NginxGateway, HttpBin]
secureProxy =
  RequestSpecsTest
    { stToDesc = \s -> "Simple " ++ s ++ " proxying:"
    , stSettings = defaultTestSettings
    , stAssertReplies = assertHttpRepliesAreEq
    , stProc = nginxAndHttpBin
    , stScheme = "HTTPS"
    , stCore = sNil
    , stRequestSpecs = testRequests
    , stWithApp = withSecureApp
    }


sNil :: RequestBuilder
sNil = secure nil


class HostOf a where
  hostOf :: HandlesOf a -> ByteString


instance HostOf '[HttpBin] where
  hostOf f = encodeUtf8 $ hAddr $ handleOf @"tmp-http-bin" Proxy f


instance HostOf '[NginxGateway, HttpBin] where
  hostOf f = encodeUtf8 $ hAddr $ handleOf @"nginx-test" Proxy f


hostBuilder :: (HostOf a) => HandlesOf a -> RequestBuilder -> RequestBuilder
hostBuilder f builder = builder {rbHost = hostOf f}


type ReverseProxyFixture = HandlesOf '[NginxGateway, HttpBin]


aGateway :: NginxGateway
aGateway =
  NginxGateway
    { ngCommonName = "localhost"
    , ngTargetPort = 80
    , ngTargetName = "tmp-http-bin"
    }


nginxAndHttpBin :: HList '[NginxGateway, HttpBin]
nginxAndHttpBin = aGateway &:& HttpBin


-- | A data type representing a connection to a HttpBin server.
data HttpBin = HttpBin


type HttpBinFixture = HandlesOf '[HttpBin]


onlyHttpBin :: HList '[HttpBin]
onlyHttpBin = HttpBin &: HNil


-- | Run HttpBin using tmp-proc.
instance Proc HttpBin where
  type Image HttpBin = "kennethreitz/httpbin"
  type Name HttpBin = "tmp-http-bin"
  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


-- | Make a uri access the http-bin server.
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


ping' :: ProcHandle a -> IO Pinged
ping' handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- handleGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a Get on localhost.
handleGet :: ProcHandle a -> Text -> IO Int
handleGet handle urlPath = do
  let theUri = "http://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
  statusCode . HC.responseStatus <$> HC.httpLbs getReq manager
