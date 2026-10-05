-- | HTTP GET through http-client. The wasm32-wasip3 build of this package
-- has the same interface over wasi:http.
module Aihc.Http
  ( httpGet,
    httpGetToFile,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client
  ( Request (responseTimeout),
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

-- | Get a URL and return the whole body. A status outside 200 to 299 is an
-- error, as is any failure of the connection.
httpGet :: String -> IO (Either String LBS.ByteString)
httpGet url = do
  result <- try $ do
    manager <- getGlobalManager
    request <- parseRequest url
    response <- httpLbs request manager
    pure (statusError url (responseStatus response), responseBody response)
  pure $ case result of
    Left (err :: SomeException) -> Left (displayException err)
    Right (Just failure, _) -> Left failure
    Right (Nothing, body) -> Right body

-- | Get a URL and stream the body to a file, replacing the file. The body
-- is never held in memory, so this is the call for a large download. The
-- response may take up to 300 seconds. A failure is an 'IOError' and leaves
-- the file in an unspecified state.
httpGetToFile :: String -> FilePath -> IO ()
httpGetToFile url path = do
  manager <- getGlobalManager
  request <- parseRequest url
  let patient = request {responseTimeout = responseTimeoutMicro (300 * 1000 * 1000)}
  withResponse patient manager $ \response -> do
    mapM_ (ioError . userError) (statusError url (responseStatus response))
    withBinaryFile path WriteMode $ \handle ->
      let copyChunks = do
            chunk <- brRead (responseBody response)
            unless (BS.null chunk) $ do
              BS.hPut handle chunk
              copyChunks
       in copyChunks

statusError :: String -> Status -> Maybe String
statusError url status
  | code >= 200 && code < 300 = Nothing
  | otherwise = Just ("HTTP " ++ show code ++ " for " ++ url)
  where
    code = statusCode status
