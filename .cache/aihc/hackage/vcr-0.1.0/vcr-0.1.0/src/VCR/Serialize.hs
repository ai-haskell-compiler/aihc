{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Stability: unstable
module VCR.Serialize (
  saveTape
, loadTape
) where

import Imports

import Control.Exception
import Data.Either
import Data.Text (Text)
import Data.Ord
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.CaseInsensitive qualified as CI
import Data.Yaml
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml
import System.IO.Error
import System.Directory
import System.FilePath

import WebMock hiding (withRequestAction)

saveTape :: FilePath -> [(Request, Response)] -> IO ()
saveTape file interactions = do
  ensureDirectory file
  B.writeFile file $ Yaml.encodePretty conf (toInteractions interactions)
  where
    conf = Yaml.setConfCompare (comparing f) Yaml.defConfig

    f :: Text -> Int
    f name = fromMaybe maxBound (lookup name fieldOrder)

    toInteractions :: [(Request, Response)] -> [Interaction]
    toInteractions = map \ (request, response) -> (Interaction request response)

fieldOrder :: [(Text, Int)]
fieldOrder = flip zip [1..] [
    "request"
  , "response"

  , "method"
  , "url"

  , "status"
  , "headers"
  , "body"

  , "code"
  , "message"

  , "name"
  , "value"
  ]

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectoryIfMissing True . takeDirectory

loadTape :: FilePath -> IO [(Request, Response)]
loadTape file = fromRight [] <$> tryJust (guard . isDoesNotExistError) (unsafeLoadTape file)

unsafeLoadTape :: FilePath -> IO [(Request, Response)]
unsafeLoadTape file = B.readFile file >>= fmap fromInteractions . Yaml.decodeThrow
  where
    fromInteractions :: [Interaction] -> [(Request, Response)]
    fromInteractions = map \ (Interaction request response) -> (request, response)

data Interaction = Interaction Request Response

instance ToJSON Interaction where
  toJSON (Interaction request response) = object [
        "request" .= requestToJSON request
      , "response" .= responseToJSON response
      ]
    where
      requestToJSON :: Request -> Value
      requestToJSON Request{..} = object [
          "method" .= B.unpack requestMethod
        , "url" .= requestUrl
        , "headers" .= headersToJSON requestHeaders
        , "body" .= L.unpack requestBody
        ]

      responseToJSON :: Response -> Value
      responseToJSON Response{..} = object [
          "status" .= statusToJSON responseStatus
        , "headers" .= headersToJSON responseHeaders
        , "body" .= L.unpack responseBody
        ]
        where
          statusToJSON :: Status -> Value
          statusToJSON (Status code message) = object [
              "code" .= code
            , "message" .= B.unpack message
            ]

      headersToJSON :: RequestHeaders -> Value
      headersToJSON = toJSON . map headerToJSON
        where
          headerToJSON :: Header -> Value
          headerToJSON (name, value) = object [
              "name" .= B.unpack (CI.original name)
            , "value" .= B.unpack value
            ]

instance FromJSON Interaction where
  parseJSON = withObject "Interaction" \ o -> Interaction
    <$> (o .: "request" >>= requestFromJSON)
    <*> (o .: "response" >>= responseFromJSON)
    where
      requestFromJSON :: Value -> Parser Request
      requestFromJSON = withObject "Request" \ o ->
        Request
        <$> (B.pack <$> o .: "method")
        <*> o .: "url"
        <*> (o .: "headers" >>= headersFromJSON)
        <*> (L.pack <$> o .: "body")

      responseFromJSON :: Value -> Parser Response
      responseFromJSON = withObject "Response" \ o -> Response
        <$> (o .: "status" >>= statusFromJSON)
        <*> (o .: "headers" >>= headersFromJSON)
        <*> (L.pack <$> o .: "body")
        where
          statusFromJSON :: Object -> Parser Status
          statusFromJSON o = Status
            <$> (o .: "code")
            <*> (B.pack <$> o .: "message")

      headersFromJSON :: [Object] -> Parser RequestHeaders
      headersFromJSON = mapM headerFromJSON
        where
          headerFromJSON :: Object -> Parser Header
          headerFromJSON o = (,)
            <$> (CI.mk . B.pack <$> o .: "name")
            <*> (B.pack <$> o .: "value")
