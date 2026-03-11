{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Http.Push (
    pushMetrics,
    parseURI,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString.Builder (toLazyByteString)
import Data.Map (foldMapWithKey)
import Data.Text (Text, unpack)
import Network.HTTP.Client (
    Request (..),
    RequestBody (..),
    getUri,
    httpNoBody,
    parseRequest,
    requestBody,
    requestFromURI,
    requestHeaders,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPut)
import Network.URI (
    URI (..),
    URIAuth,
    nullURI,
 )

import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.MetricId (Labels (..))
import System.Metrics.Prometheus.Registry (RegistrySample)


-- | Parses a uri such that
-- @
--   parseURI "https://example.com"
--      ===
--   Just (URI "https:" "//example.com"
-- @
parseURI :: String -> Maybe URI
parseURI = fmap getUri . parseRequest


pushMetrics ::
    -- | PushGateway URI name, including port number (ex: @parseUri https://myGateway.com:8080@)
    URI ->
    -- | Job name
    Text ->
    -- | Label set to use as a grouping key for metrics
    Labels ->
    -- | Microsecond push frequency
    Int ->
    -- | Action to get latest metrics
    IO RegistrySample ->
    IO ()
pushMetrics gatewayURI jobName labels frequencyMicros getSample = do
    manager <- newTlsManager
    gn <- maybe (error "Invalid URI Authority") pure gatewayName
    requestUri <- requestFromURI $ buildUri scheme gn jobName labels
    forever $ getSample >>= flip httpNoBody manager . request requestUri >> threadDelay frequencyMicros
  where
    URI scheme gatewayName _ _ _ = gatewayURI
    request req sample =
        req
            { method = methodPut
            , requestBody = RequestBodyLBS . toLazyByteString $ encodeMetrics sample
            , requestHeaders = [(hContentType, "text/plain; version=0.0.4")]
            }


buildUri :: String -> URIAuth -> Text -> Labels -> URI
buildUri scheme gatewayName jobName (Labels ls) =
    nullURI
        { uriScheme = scheme
        , uriAuthority = Just gatewayName
        , uriPath = "/metrics/job/" ++ unpack jobName ++ foldMapWithKey labelPath ls
        }
  where
    labelPath k v = "/" ++ unpack k ++ "/" ++ unpack v
