{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Stability: unstable
module WebMock (
  Request (..)
, Response (..)
, disableRequests
, unsafeMockRequest
, mockRequest
, mockRequestChain
, mkRequestActions
, mkRequestAction
, module Network.HTTP.Types

, withRequestAction
, protectRequestAction

, toSimpleRequest
, fromSimpleResponse
, toSimpleResponse
) where

import Imports

import Control.Exception
import Data.ByteString.Lazy qualified as L
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client.Internal (Manager, BodyReader)
import Network.HTTP.Client.Internal qualified as Client
import Network.HTTP.Types
import Network.URI (uriToString)
import Test.HUnit

import WebMock.Util

data Request = Request {
  requestMethod  :: Method
, requestUrl     :: String
, requestHeaders :: RequestHeaders
, requestBody    :: LazyByteString
} deriving (Eq, Ord)

instance IsString Request where
  fromString url = Request "GET" url [] ""

instance Show Request where
  show Request{..} = unlines [
      "Request {"
    , "  requestMethod = " ++ show requestMethod
    , ", requestUrl = " ++ show requestUrl
    , ", requestHeaders = " ++ show requestHeaders
    , ", requestBody = " ++ show requestBody
    , "}"
    ]

data Response = Response {
  responseStatus  :: Status
, responseHeaders :: ResponseHeaders
, responseBody    :: LazyByteString
} deriving (Eq, Show)

instance IsString Response where
  fromString body = Response status200 [] (L.fromStrict $ encodeUtf8 $ T.pack body)

unsafeMockRequest :: (Request -> IO Response) -> IO ()
unsafeMockRequest f = atomicWriteIORef Client.requestAction requestAction
  where
    requestAction :: RequestAction
    requestAction request _manager = do
      toSimpleRequest request >>= f >>= fromSimpleResponse request

mockRequest :: HasCallStack => Request -> Response -> IO a -> IO a
mockRequest expectedRequest response action = protectRequestAction do
  unsafeMockRequest \ request -> do
    request @?= expectedRequest
    return response
  action

disableRequests :: HasCallStack => IO a -> IO a
disableRequests action = do
  let
    requestAction :: IO Response -> Request -> IO Response
    requestAction _makeRequest request = do
      unexpectedRequest request
  withRequestAction requestAction action

mockRequestChain :: HasCallStack => [Request -> IO Response] -> IO a -> IO a
mockRequestChain interactions action = do
  ref <- newIORef interactions

  let
    requestAction :: IO Response -> Request -> IO Response
    requestAction _makeRequest request = do
      atomicModifyIORef' ref (drop 1 &&& listToMaybe) >>= \ case
        Just interaction -> interaction request
        Nothing -> unexpectedRequest request

    checkLeftover :: IO ()
    checkLeftover = do
      leftover <- length <$> atomicReadIORef ref
      when (leftover /= 0) do
        let
          total = length interactions
          actual = total - leftover
        assertFailure $ "Expected " ++ show total ++ " requests, but only received " ++ show actual ++ "!"

  withRequestAction requestAction action <* checkLeftover

unexpectedRequest :: HasCallStack => Request -> IO a
unexpectedRequest request = assertFailure $ "Unexpected HTTP request: " ++ show request

mkRequestActions :: HasCallStack => [(Request, Response)] -> [Request -> IO Response]
mkRequestActions = map (uncurry mkRequestAction)

mkRequestAction :: HasCallStack => Request -> Response -> Request -> IO Response
mkRequestAction expected response actual = do
  actual @?= expected
  return response

type RequestAction = Client.Request -> Manager -> IO (Client.Response BodyReader)

withRequestAction :: (IO Response -> Request -> IO Response) -> IO a -> IO a
withRequestAction action = bracket setup restore . const
  where
    lift :: RequestAction -> RequestAction
    lift makeClientRequest request manager = do
      toSimpleRequest request >>= makeRequest >>= fromSimpleResponse request
      where
        makeRequest :: Request -> IO Response
        makeRequest = action $ makeClientRequest request manager >>= toSimpleResponse

    setup :: IO RequestAction
    setup = atomicModifyIORef' Client.requestAction \ old -> (lift old, old)

    restore :: RequestAction -> IO ()
    restore = atomicWriteIORef Client.requestAction

protectRequestAction :: IO a -> IO a
protectRequestAction = bracket save restore . const
  where
    save :: IO RequestAction
    save = atomicReadIORef Client.requestAction

    restore :: RequestAction -> IO ()
    restore = atomicWriteIORef Client.requestAction

toSimpleRequest :: Client.Request -> IO Request
toSimpleRequest r = do
  body <- requestBodyToByteString (Client.requestBody r)
  return $ Request {
    requestMethod = Client.method r
  , requestUrl = uriToString id (Client.getUri r) ""
  , requestHeaders = Client.requestHeaders r
  , requestBody = body
  }

toSimpleResponse :: Client.Response BodyReader -> IO Response
toSimpleResponse r = do
  c <- Client.brConsume (Client.responseBody r)
  Client.responseClose r
  return $ Response {
    responseStatus = Client.responseStatus r
  , responseHeaders = Client.responseHeaders r
  , responseBody = L.fromChunks c
  }

fromSimpleResponse :: Client.Request -> Response -> IO (Client.Response BodyReader)
fromSimpleResponse request Response{..} = do
  body <- Client.constBodyReader (L.toChunks responseBody)
  return $ Client.Response {
    Client.responseStatus = responseStatus
  , Client.responseVersion = http11
  , Client.responseHeaders = responseHeaders
  , Client.responseBody = body
  , Client.responseCookieJar = mempty
  , Client.responseClose' = Client.ResponseClose pass
  , Client.responseOriginalRequest = request { Client.requestBody = mempty }
  , Client.responseEarlyHints = mempty
  }
