{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module VCRSpec (spec) where

import Imports

import Test.Hspec
import Test.HUnit.Lang
import Test.Mockery.Directory
import System.Timeout

import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types

import WebMock

import VCR
import VCR.Serialize (loadTape)

makeRequest :: String -> [Header] -> IO (Client.Response LazyByteString)
makeRequest url headers = do
  manager <- getGlobalManager
  request <- Client.parseUrlThrow url
  Client.httpLbs request { Client.requestHeaders = headers } manager

httpException :: Client.HttpException -> Bool
httpException _ = True

hUnitFailure :: FailureReason -> HUnitFailure -> Bool
hUnitFailure expectd (HUnitFailure _ actual) = expectd == actual

expectedButGot :: Request -> Request -> HUnitFailure -> Bool
expectedButGot expected actual= hUnitFailure (ExpectedButGot Nothing (show expected) (show actual))

unexpectedRequest :: Request -> HUnitFailure -> Bool
unexpectedRequest = hUnitFailure . Reason . mappend "Unexpected HTTP request: " . show

infix 1 `shouldReturnStatus`
infix 1 `shouldReturnBody`

shouldReturnStatus :: HasCallStack => String -> Status -> IO ()
shouldReturnStatus url expected = do
  Client.responseStatus <$> makeRequest url [] `shouldReturn` expected

shouldReturnBody :: HasCallStack => String -> LazyByteString -> IO ()
shouldReturnBody url expected = do
  Client.responseBody <$> makeRequest url [] `shouldReturn` expected

authRequest :: Request
authRequest = "http://httpbin.org/status/200" {
  requestHeaders = [(hAuthorization, "Bearer sk-RfAZfajzapKps4anC6ej8rhSnMxf5sLd")]
}

redactedAuthRequest :: Request
redactedAuthRequest = authRequest {
  requestHeaders = [(hAuthorization, "********")]
}

makeAuthRequest :: IO ()
makeAuthRequest = void $ makeRequest authRequest.requestUrl authRequest.requestHeaders

respondWith :: Status -> Request -> IO Response
respondWith responseStatus _ = return $ "" { responseStatus }

spec :: Spec
spec = around_ inTempDirectory do
  describe "with" do
    context "when mode is AnyOrder" do
      let tape = "tape.yaml"

      it "records requests in the order they were made" do
        mockRequestChain [respondWith status200, respondWith status202, respondWith status201] do
          VCR.with tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/202" `shouldReturnStatus` status202
            "http://httpbin.org/status/201" `shouldReturnStatus` status201
        loadTape tape.file `shouldReturn` [
            ("http://httpbin.org/status/200", "" { responseStatus = status200 })
          , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
          , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
          ]

      context "with repeated requests to a resource" do
        it "records the first request, replays subsequent requests" do
          mockRequestChain [\ _ -> return ""] do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          length <$> loadTape tape.file `shouldReturn` 1

      context "with an existing tape" do
        it "replays existing requests from the tape" do
          mockRequest "http://httpbin.org/status/200" "" do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          disableRequests do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          length <$> loadTape tape.file `shouldReturn` 1

        it "records new requests to the tape" do
            mockRequest "http://httpbin.org/status/200" "" do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
            mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
              VCR.with tape do
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
            length <$> loadTape tape.file `shouldReturn` 2

      context "with concurrent requests" do
        it "records the requests concurrently, without blocking" do
          maybe (expectationFailure "<<timeout>>") return <=< timeout 100_000 . protectRequestAction $ do
            unsafeMockRequest (\ _ -> threadDelay 10_000 >> return "")
            VCR.with tape do
              forConcurrently_ [200 .. 299 :: Int] $ \ status -> do
                "http://httpbin.org/status/" <> show status `shouldReturnBody` ""
          length <$> loadTape tape.file `shouldReturn` 100

      context "on exception" do
        it "writes the tape to disk" do
          mockRequest "http://httpbin.org/status/500" "" { responseStatus = status500 } do
            VCR.with tape (makeRequest "http://httpbin.org/status/500" [])
              `shouldThrow` httpException
          length <$> loadTape tape.file `shouldReturn` 1

      context "with an Authorization header" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            VCR.with tape makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

    context "when mode is Sequential" do
      let tape = "tape.yaml" { mode = Sequential }

      context "with repeated requests to a resource" do
        it "records all interactions" do
          mockRequest "http://httpbin.org/status/200" "" do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          length <$> loadTape tape.file `shouldReturn` 3

      context "with an existing tape" do
        it "replays request in order" do
          mockRequestChain [respondWith status200, respondWith status202, respondWith status201] do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/202" `shouldReturnStatus` status202
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
          disableRequests $ do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/202" `shouldReturnStatus` status202
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
          loadTape tape.file `shouldReturn` [
              ("http://httpbin.org/status/200", "" { responseStatus = status200 })
            , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
            , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
            ]

        context "with an out of order request" do
          it "fails" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
            disableRequests $ do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/202" `shouldReturnStatus` status200
              `shouldThrow`
                  expectedButGot
                    (Request "GET" "http://httpbin.org/status/201" [] "")
                    (Request "GET" "http://httpbin.org/status/202" [] "")

        context "with a missing request" do
          it "fails" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
            disableRequests $ do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
              `shouldThrow` hUnitFailure (Reason "Expected 3 requests, but only received 2!")

        context "with additional requests" do
          it "records additional requests" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
              VCR.with tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
              loadTape tape.file `shouldReturn` [
                  ("http://httpbin.org/status/200", "" { responseStatus = status200 })
                , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
                , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
                ]

      context "on exception" do
        it "writes the tape to disk" do
          mockRequest "http://httpbin.org/status/500" "" { responseStatus = status500 } do
            VCR.with tape (makeRequest "http://httpbin.org/status/500" [])
              `shouldThrow` httpException
          length <$> loadTape tape.file `shouldReturn` 1

      context "with an Authorization header" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            VCR.with tape makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

  describe "record" do
    context "when mode is AnyOrder" do
      let tape = "tape.yaml"

      context "with repeated requests to a resource" do
        it "records only the last interaction" do
          mockRequestChain [\ _ -> return "foo", \ _ -> return "bar", \ _ -> return "baz"] do
            VCR.record tape do
              "http://httpbin.org/status/200" `shouldReturnBody` "foo"
              "http://httpbin.org/status/200" `shouldReturnBody` "bar"
              "http://httpbin.org/status/200" `shouldReturnBody` "baz"
          loadTape tape.file `shouldReturn` [("http://httpbin.org/status/200", "baz")]

      context "with an existing tape" do
        it "records new requests to the tape" do
            mockRequest "http://httpbin.org/status/200" "" do
              VCR.record tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
            mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
              VCR.record tape do
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
            length <$> loadTape tape.file `shouldReturn` 2

        it "updates existing requests, keeping the original order" do
          mockRequestChain (replicate 3 $ \ _ -> return "") do
            VCR.record tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/202" `shouldReturnStatus` status200
              "http://httpbin.org/status/201" `shouldReturnStatus` status200

          mockRequestChain [\ _ -> return "foo"] do
            VCR.record tape do
              "http://httpbin.org/status/202" `shouldReturnStatus` status200

          loadTape tape.file `shouldReturn` [
              ("http://httpbin.org/status/200", "")
            , ("http://httpbin.org/status/202", "foo")
            , ("http://httpbin.org/status/201", "")
            ]

    context "when mode is Sequential" do
      let tape = "tape.yaml" { mode = Sequential }

      context "with repeated requests to a resource" do
        it "records all interactions" do
          mockRequest "http://httpbin.org/status/200" "" do
            VCR.with tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          length <$> loadTape tape.file `shouldReturn` 3

      context "with an existing tape" do
        it "overwrites the existing tape" do
            mockRequest "http://httpbin.org/status/200" "" do
              VCR.record tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
              loadTape tape.file `shouldReturn` [
                  ("http://httpbin.org/status/200", "")
                ]
            mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
              VCR.record tape do
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
              loadTape tape.file `shouldReturn` [
                  ("http://httpbin.org/status/201", "" { responseStatus = status201 })
                ]

  describe "play" do
    context "when mode is AnyOrder" do
      let tape = "tape.yaml"

      it "replays existing requests from the tape" do
        mockRequest "http://httpbin.org/status/200" "" do
          VCR.record tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        disableRequests do
          VCR.play tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        length <$> loadTape tape.file `shouldReturn` 1

      it "fails on new requests" do
          mockRequest "http://httpbin.org/status/200" "" do
            VCR.record tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
            VCR.play tape do
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
            `shouldThrow` unexpectedRequest "http://httpbin.org/status/201"

    context "when mode is Sequential" do
      let tape = "tape.yaml" { mode = Sequential }

      it "replays request in order" do
        mockRequestChain [respondWith status200, respondWith status202, respondWith status201] do
          VCR.record tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/202" `shouldReturnStatus` status202
            "http://httpbin.org/status/201" `shouldReturnStatus` status201
        disableRequests $ do
          VCR.play tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/202" `shouldReturnStatus` status202
            "http://httpbin.org/status/201" `shouldReturnStatus` status201

      context "when not all requests are used" do
          it "fails" do
            mockRequest "http://httpbin.org/status/200" "" do
              VCR.record tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
            VCR.play tape do "http://httpbin.org/status/200" `shouldReturnStatus` status200
              `shouldThrow` hUnitFailure (Reason "Expected 2 requests, but only received 1!")

      context "with additional requests" do
        it "fails" do
          mockRequest "http://httpbin.org/status/200" "" do
            VCR.record tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          mockRequestChain [\ _ -> return ""] do
            VCR.play tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
            `shouldThrow` unexpectedRequest "http://httpbin.org/status/200"
