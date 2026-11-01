-- | HTTP GET through http-client. The wasm32-wasip3 build of this package
-- has the same interface over wasi:http.
--
-- A request that fails in a way that can pass on a second try is made
-- again, up to 'retryCount' more times, with a delay that doubles between
-- tries. The Hackage CDN now and then closes a connection in the middle of
-- a chunked body or answers with a 5xx status, and one such failure must
-- not fail a plan of a few hundred requests.
module Aihc.Http
  ( httpGet,
    httpGetToFile,
    retryCount,
    retryDelays,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, fromException, throwIO, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Network.HTTP.Client
  ( HttpException (InvalidUrlException),
    Request (responseTimeout),
    brRead,
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
    withResponse,
  )
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types.Status (Status, statusCode)
import System.IO (IOMode (WriteMode), withBinaryFile)

-- | How many times a request is made again after a failure that can pass.
retryCount :: Int
retryCount = 5

-- | The delays before each new try, in microseconds. The first try waits
-- for one second, and each later try waits twice as long as the one before.
-- All the tries together wait for 31 seconds at most.
retryDelays :: [Int]
retryDelays = take retryCount (iterate (* 2) 1000000)

-- | Get a URL and return the whole body. A status outside 200 to 299 is an
-- error, as is any failure of the connection.
httpGet :: String -> IO (Either String LBS.ByteString)
httpGet url = do
  result <- try $ do
    manager <- getGlobalManager
    request <- parseRequest url
    withRetries url $ do
      response <- httpLbs request manager
      pure (responseStatus response, responseBody response)
  pure $ case result of
    Left (err :: SomeException) -> Left (displayException err)
    Right answer -> answer

-- | Get a URL and stream the body to a file, replacing the file. The body
-- is never held in memory, so this is the call for a large download. The
-- response may take up to 300 seconds. A failure is an 'IOError' and leaves
-- the file in an unspecified state.
httpGetToFile :: String -> FilePath -> IO ()
httpGetToFile url path = do
  manager <- getGlobalManager
  request <- parseRequest url
  let patient = request {responseTimeout = responseTimeoutMicro (300 * 1000 * 1000)}
  answer <- withRetries url $
    withResponse patient manager $ \response -> do
      let status = responseStatus response
      case statusError url status of
        -- The body of a failed status is not the file.
        Just _ -> pure (status, ())
        Nothing -> do
          withBinaryFile path WriteMode $ \handle ->
            let copyChunks = do
                  chunk <- brRead (responseBody response)
                  unless (BS.null chunk) $ do
                    BS.hPut handle chunk
                    copyChunks
             in copyChunks
          pure (status, ())
  either (ioError . userError) pure answer

-- | Make a request, and make it again after a failure that can pass. The
-- result is the body of a 2xx answer, or the reason of the last failure.
--
-- A failure that can pass is an exception of the connection, such as a
-- reset, a timeout, or a bad chunk header, or a status that says the server
-- is busy or had a fault. A status that says the request itself is wrong,
-- such as 404, is final: Hackage answers 404 for a package it does not
-- have, and that answer must come back at once.
withRetries :: String -> IO (Status, a) -> IO (Either String a)
withRetries url attempt = go retryDelays
  where
    go delays = do
      result <- try attempt
      case result of
        Right (status, body)
          | Nothing <- statusError url status -> pure (Right body)
        Right (status, _)
          | not (statusCanPass status) -> pure (Left (failedStatus status))
          | delay : rest <- delays -> threadDelay delay >> go rest
          | otherwise -> pure (Left (failedStatus status <> tried))
        Left (err :: SomeException)
          | Just (InvalidUrlException _ _) <- fromException err -> throwIO err
          | delay : rest <- delays -> threadDelay delay >> go rest
          | otherwise -> pure (Left (displayException err <> tried))
    failedStatus status = fromMaybe "" (statusError url status)
    tried = " (after " <> show (retryCount + 1) <> " tries)"

-- | A status that a later try can turn into an answer: the server is busy,
-- had a fault, or gave up on the request.
statusCanPass :: Status -> Bool
statusCanPass status = code >= 500 || code == 408 || code == 429
  where
    code = statusCode status

statusError :: String -> Status -> Maybe String
statusError url status
  | code >= 200 && code < 300 = Nothing
  | otherwise = Just ("HTTP " ++ show code ++ " for " ++ url)
  where
    code = statusCode status
