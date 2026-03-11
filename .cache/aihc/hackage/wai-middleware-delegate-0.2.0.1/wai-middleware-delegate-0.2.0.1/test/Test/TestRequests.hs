{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TestRequests
  ( RequestBuilder (..)
  , buildRequest
  , testRequests
  , testGetRequests
  , testPostRequests
  , testNotProxiedRequests
  , testOverRedirectedRequests
  , nil
  , secure
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
  ( Request
  , RequestBody (..)
  , host
  , method
  , parseRequest
  , redirectCount
  , requestBody
  )
import Network.HTTP.Types (Method, methodGet, methodPost)


data RequestBuilder = RequestBuilder
  { rbMethod :: !Method
  , rbSecure :: !Bool
  , rbPath :: !String
  , rbBody :: !(Maybe RequestBody)
  , rbHost :: !BS.ByteString
  , rbPort :: !(Maybe Int)
  }


nil :: RequestBuilder
nil =
  RequestBuilder
    { rbMethod = methodGet
    , rbSecure = False
    , rbPath = "/"
    , rbBody = Nothing
    , rbHost = "httpbin.org"
    , rbPort = Nothing
    }


secure :: RequestBuilder -> RequestBuilder
secure x = x {rbSecure = True}


testRequests :: [(String, RequestBuilder -> RequestBuilder)]
testRequests = testGetRequests <> testPostRequests


testGetRequests :: [(String, RequestBuilder -> RequestBuilder)]
testGetRequests =
  [
    ( "GET"
    , \builder -> builder {rbPath = "/get"}
    )
  ,
    ( "GET (with a query)"
    , \builder -> builder {rbPath = "/get?a=10&b=whatever"}
    )
  ,
    ( "GET (multiple redirects)"
    , \builder -> builder {rbPath = "/redirect/3"}
    )
  ,
    ( "GET (with a body)"
    , \builder ->
        builder
          { rbPath = "/get"
          , rbBody = Just $ RequestBodyBS "Hello httpbin!"
          }
    )
  ,
    ( "GET (forbidden resource)"
    , \builder -> builder {rbPath = "/status/403"}
    )
  ,
    ( "GET (missing resource)"
    , \builder -> builder {rbPath = "/status/404"}
    )
  ]


testNotProxiedRequests :: [(String, RequestBuilder -> RequestBuilder)]
testNotProxiedRequests =
  [
    ( "GET (funny resource - differs on proxy)"
    , \builder -> builder {rbPath = "/status/418"}
    )
  ]


testOverRedirectedRequests :: [(String, RequestBuilder -> RequestBuilder)]
testOverRedirectedRequests =
  [
    ( "GET (multiple redirects)"
    , \builder -> builder {rbPath = "/redirect/3"}
    )
  ]


testPostRequests :: [(String, RequestBuilder -> RequestBuilder)]
testPostRequests =
  [
    ( "POST"
    , \builder -> builder {rbMethod = methodPost, rbPath = "/post"}
    )
  ,
    ( "POST (with a query)"
    , \builder -> builder {rbMethod = methodPost, rbPath = "/post?a=10&b=whatever"}
    )
  ,
    ( "POST (with a body)"
    , \builder ->
        builder
          { rbPath = "/post"
          , rbBody = Just $ RequestBodyBS "Hello httpbin!"
          , rbMethod = methodPost
          }
    )
  ,
    ( "POST (forbidden resource)"
    , \builder -> builder {rbMethod = methodPost, rbPath = "/status/403"}
    )
  ,
    ( "POST (missing resource)"
    , \builder -> builder {rbMethod = methodPost, rbPath = "/status/404"}
    )
  ]


-- | Simplifies creation of the requests used in testing
buildRequest :: RequestBuilder -> IO Request
buildRequest RequestBuilder {rbMethod, rbSecure, rbPath, rbBody, rbHost, rbPort} = do
  let scheme
        | rbSecure = "https"
        | otherwise = "http"
      portStr = maybe "" (\x -> ":" ++ show x) rbPort
      url = scheme ++ "://" ++ C8.unpack rbHost ++ portStr ++ rbPath
  req <- parseRequest url
  return $
    req
      { method = rbMethod
      , requestBody = fromMaybe (requestBody req) rbBody
      , host = rbHost
      , redirectCount = 0
      }
