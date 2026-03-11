{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | internal module to support modules in GitLab.API
module GitLab.WebRequests.GitLabWebCalls
  ( GitLabParam,
    gitlabGetOne,
    gitlabGetMany,
    gitlabPost,
    gitlabPut,
    gitlabDelete,
    gitlabUnsafe,
    gitlabGetByteStringResponse,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as MR
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Text.Read

newtype GitLabException = GitLabException String
  deriving (Eq, Show)

instance Exception.Exception GitLabException

type GitLabParam = (BS.ByteString, Maybe BS.ByteString)

gitlabGetOne ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabGetOne urlPath params =
  request
  where
    request =
      gitlabHTTPOne
        "GET"
        "application/x-www-form-urlencoded"
        urlPath
        params
        []

gitlabGetMany ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) [a])
gitlabGetMany urlPath params =
  gitlabHTTPMany
    "GET"
    "application/x-www-form-urlencoded"
    urlPath
    params
    []

gitlabPost ::
  (FromJSON a) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabPost urlPath params = do
  request
  where
    request =
      gitlabHTTPOne
        "POST"
        "application/x-www-form-urlencoded"
        urlPath
        []
        params

gitlabPut ::
  FromJSON a =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabPut urlPath params = do
  request
  where
    request =
      gitlabHTTPOne
        "PUT"
        "application/x-www-form-urlencoded"
        urlPath
        []
        params

gitlabDelete ::
  FromJSON a =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Either (Response BSL.ByteString) (Maybe a))
gitlabDelete urlPath params = do
  request
  where
    request =
      gitlabHTTPOne
        "DELETE"
        "application/x-www-form-urlencoded"
        urlPath
        []
        params

-- | Assumes that HTTP error code responses, e.g. 404, 409, won't be
-- returned as (Left response) value.
gitlabUnsafe :: GitLab (Either a (Maybe b)) -> GitLab b
gitlabUnsafe query = do
  result <- query
  case result of
    Left _err -> error "gitlabUnsafe error"
    Right Nothing -> error "gitlabUnsafe error"
    Right (Just x) -> return x

-- | Lower level query that returns the raw bytestring response from a
-- GitLab HTTP query. Useful for downloading project archives files.
gitlabGetByteStringResponse ::
  -- | the URL to post to
  Text ->
  -- | the data to post
  [GitLabParam] ->
  GitLab (Response BSL.ByteString)
gitlabGetByteStringResponse urlPath params =
  request
  where
    request =
      gitlabHTTP
        "GET"
        "application/x-www-form-urlencoded"
        urlPath
        params
        []

---------------------
-- internal functions

gitlabHTTP ::
  -- | HTTP method (PUT, POST, DELETE, GET)
  BS.ByteString ->
  -- | Content type (content-type)
  BS.ByteString ->
  -- | the URL
  Text ->
  -- | the URL parameters for GET calls
  [GitLabParam] ->
  -- | the content paramters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab (Response BSL.ByteString)
gitlabHTTP httpMethod contentType urlPath urlParams contentParams = do
  cfg <- MR.asks serverCfg
  manager <- MR.asks httpManager
  let url' = url cfg <> "/api/v4" <> urlPath <> T.decodeUtf8 (renderQuery True urlParams)
  let authHeader = case token cfg of
        AuthMethodToken t -> ("PRIVATE-TOKEN", T.encodeUtf8 t)
        AuthMethodOAuth t -> ("Authorization", "Bearer " <> T.encodeUtf8 t)
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = httpMethod,
            requestHeaders =
              [ authHeader,
                ("content-type", contentType)
              ],
            requestBody = RequestBodyBS (renderQuery False contentParams)
          }
  liftIO $ tryGitLab 0 request (retries cfg) manager Nothing

gitlabHTTPOne ::
  FromJSON a =>
  -- | HTTP method (PUT, POST, DELETE, GET)
  BS.ByteString ->
  -- | Content type (content-type)
  BS.ByteString ->
  -- | the URL
  Text ->
  -- | the URL query data for GET calls
  [GitLabParam] ->
  -- | the content parameters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab
    (Either (Response BSL.ByteString) (Maybe a))
gitlabHTTPOne httpMethod contentType urlPath urlParams contentParams = do
  response <-
    gitlabHTTP
      httpMethod
      contentType
      urlPath
      urlParams
      contentParams
  if successStatus (responseStatus response)
    then return (Right (parseOne (responseBody response)))
    else return (Left response)

gitlabHTTPMany ::
  (FromJSON a) =>
  -- | HTTP method (PUT, POST, DELETE, GET)
  BS.ByteString ->
  -- | Content type (content-type)
  BS.ByteString ->
  -- | the URL
  Text ->
  -- | the URL query data for GET calls
  [GitLabParam] ->
  -- | the content parameters for POST, PUT and DELETE calls
  [GitLabParam] ->
  GitLab
    (Either (Response BSL.ByteString) [a])
gitlabHTTPMany httpMethod contentType urlPath urlParams contentParams = do
  go 1 []
  where
    go :: FromJSON a => Int -> [a] -> GitLab (Either (Response BSL.ByteString) [a])
    go pageNum accum = do
      response <-
        gitlabHTTP
          httpMethod
          contentType
          urlPath
          (urlParams <> [("per_page", Just "100"), ("page", Just (T.encodeUtf8 (T.pack (show pageNum))))])
          contentParams
      if successStatus (responseStatus response)
        then do
          case parseMany (responseBody response) of
            Nothing -> return (Right accum)
            Just moreResults -> do
              let accum' = accum <> moreResults
              if hasNextPage response
                then go (pageNum + 1) accum'
                else return (Right accum')
        else return (Left response)

hasNextPage :: Response a -> Bool
hasNextPage resp =
  let hdrs = responseHeaders resp
   in findPages hdrs
  where
    findPages [] = False
    findPages (("X-Next-Page", bs) : _) = isJust $ readNP bs
    findPages (_ : xs) = findPages xs
    readNP :: BS.ByteString -> Maybe Int
    readNP bs = readMaybe (T.unpack (T.decodeUtf8 bs))

successStatus :: Status -> Bool
successStatus (Status n _msg) =
  n >= 200 && n <= 226

tryGitLab ::
  -- | the current retry count
  Int ->
  -- | the GitLab request
  Request ->
  -- | maximum number of retries permitted
  Int ->
  -- | HTTP manager
  Manager ->
  -- | the exception to report if maximum retries met
  Maybe HttpException ->
  IO (Response BSL.ByteString)
tryGitLab i request maxRetries manager lastException
  | i == maxRetries = error (show lastException)
  | otherwise =
      httpLbs request manager
        `Exception.catch` \ex -> tryGitLab (i + 1) request maxRetries manager (Just ex)

parseOne :: FromJSON a => BSL.ByteString -> Maybe a
parseOne bs =
  case eitherDecode bs of
    Left _err -> Nothing
    Right x -> Just x

parseMany :: FromJSON a => BSL.ByteString -> Maybe [a]
parseMany bs =
  case eitherDecode bs of
    Left _err -> Nothing
    Right xs -> Just xs
