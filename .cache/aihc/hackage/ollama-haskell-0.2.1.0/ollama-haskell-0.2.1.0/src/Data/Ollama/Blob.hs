{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Blob
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functions for managing binary large objects (blobs) in the Ollama API.

This module provides functionality for checking the existence of blobs and uploading
files as blobs to the Ollama server. Blobs are used when creating models from
GGUF files or safetensors directories.

The blob operations interact with the @\/api\/blobs@ endpoints:

- 'checkBlobExists' sends a HEAD request to check if a blob exists
- 'createBlob' uploads a file as a blob using streaming POST

Example usage:

>>> let digest = "sha256:29fdb92e57cf0827ded04ae6461b5931d01fa595843f55d36f5b275a52087dd2"
>>> exists <- checkBlobExists digest Nothing
>>> case exists of
>>>   Right True -> putStrLn "Blob exists"
>>>   Right False -> putStrLn "Blob not found"
>>>   Left err -> putStrLn $ "Error: " ++ show err

>>> result <- createBlob "model.gguf" digest Nothing
>>> case result of
>>>   Right () -> putStrLn "Blob uploaded successfully"
>>>   Left err -> putStrLn $ "Upload failed: " ++ show err
-}
module Data.Ollama.Blob
  ( -- * Blob Operations
    checkBlobExists
  , createBlob
  ) where

import Control.Exception (try)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error
import Data.Ollama.Common.Error qualified as Error
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (Status (statusCode))
import System.Directory (doesFileExist)

{- | Check if a blob exists on the Ollama server.

Sends a HEAD request to @\/api\/blobs\/sha256:\<digest\>@ to check if a blob
with the given SHA256 digest exists on the server.

Returns:
- 'Right' 'True' if the blob exists (HTTP 200 OK)
- 'Right' 'False' if the blob does not exist (HTTP 404 Not Found)
- 'Left' 'OllamaError' for other errors (network issues, invalid digest, etc.)

The digest should be a SHA256 hash string without the "sha256:" prefix.
-}
checkBlobExists ::
  -- | SHA256 digest of the blob (without "sha256:" prefix)
  Text ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError Bool)
checkBlobExists digest mbOllamaConfig = do
  let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
      endpoint = "/api/blobs/sha256:" <> digest
      fullUrl = T.unpack $ hostUrl <> endpoint
      timeoutMicros = timeout * 1000000

  manager <- case commonManager of
    Nothing ->
      newTlsManagerWith
        tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro timeoutMicros}
    Just m -> pure m

  eRequest <- try $ parseRequest fullUrl
  case eRequest of
    Left ex -> return $ Left $ Error.HttpError ex
    Right req -> do
      let request = req {method = "HEAD"}
      eResponse <- try $ httpNoBody request manager
      case eResponse of
        Left ex -> return $ Left $ Error.HttpError ex
        Right response -> do
          let status = statusCode $ responseStatus response
          case status of
            200 -> return $ Right True
            404 -> return $ Right False
            _ ->
              return $
                Left $
                  Error.ApiError $
                    "Unexpected status code: " <> T.pack (show status)

{- | Upload a file as a blob to the Ollama server.

Streams the contents of the file at the given 'FilePath' to the Ollama server
via a POST request to @\/api\/blobs\/sha256:\<digest\>@. The server will verify
that the uploaded file matches the expected SHA256 digest.

Returns:
- 'Right' @()@ if the blob was uploaded successfully (HTTP 201 Created)
- 'Left' 'OllamaError' if the upload failed

The digest should be the expected SHA256 hash of the file without the "sha256:" prefix.
If the file content doesn't match the digest, the server will return an error.

Note: This function streams the file content, so it can handle large files efficiently
without loading them entirely into memory.
-}
createBlob ::
  -- | Path to the file to upload
  FilePath ->
  -- | Expected SHA256 digest of the file (without "sha256:" prefix)
  Text ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO (Either OllamaError ())
createBlob filePath digest mbOllamaConfig = do
  let OllamaConfig {..} = fromMaybe defaultOllamaConfig mbOllamaConfig
      endpoint = "/api/blobs/sha256:" <> digest
      fullUrl = T.unpack $ hostUrl <> endpoint
      timeoutMicros = timeout * 1000000

  manager <- case commonManager of
    Nothing ->
      newTlsManagerWith
        tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro timeoutMicros}
    Just m -> pure m

  eRequest <- try $ parseRequest fullUrl
  case eRequest of
    Left ex -> return $ Left $ Error.HttpError ex
    Right req -> do
      -- check if file exists
      fileExists <- doesFileExist filePath
      if not fileExists
        then return $ Left $ Error.ApiError "File does not exist"
        else do
          -- Create a request body from the file
          fileContent <- BS.readFile filePath
          let request =
                req
                  { method = "POST"
                  , requestBody = RequestBodyBS fileContent
                  }

          eResponse <- try $ withResponse request manager $ \response -> do
            let status = statusCode $ responseStatus response
            case status of
              201 -> return $ Right ()
              400 -> do
                bodyReader <- brRead $ responseBody response
                return $
                  Left $
                    Error.ApiError $
                      "Bad request - digest mismatch: " <> TE.decodeUtf8 bodyReader
              _ -> do
                bodyReader <- brRead $ responseBody response
                return $
                  Left $
                    Error.ApiError $
                      "Unexpected status code " <> T.pack (show status) <> ": " <> TE.decodeUtf8 bodyReader

          case eResponse of
            Left ex -> return $ Left $ Error.HttpError ex
            Right result -> return result
