{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Client.Overrides.Internal.Logger
    ( logResponse
    , logRequest
    , logRequestOverride
    ) where

import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (Response, Request)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP

import Network.HTTP.Client.Overrides.Internal.Types

-----------
-- Response
-----------

logResponse :: Config -> Response body -> IO ()
logResponse config response = case logResponses $ logOptions config of
    Just Simple   -> hPutStrLn stderr $ simpleResponse response
    Just Detailed -> hPutStrLn stderr $ detailedResponse response
    _             -> return ()

simpleResponse :: Response body -> String
simpleResponse r = intercalate " " $
    [ "Response:"
    , show (HTTP.responseVersion r)
    , show (HTTP.statusCode $ HTTP.responseStatus r)
    , BS.unpack (HTTP.statusMessage $ HTTP.responseStatus r)
    ]

detailedResponse :: Response body -> String
detailedResponse r = intercalate "\n" $
    [ "Response {"
    , "  responseStatus  = " ++ show (HTTP.responseStatus r)
    , "  responseVersion = " ++ show (HTTP.responseVersion r)
    , "  responseHeaders = ["
    ]
    ++
    map (\h -> "    " ++ show h) (HTTP.responseHeaders r)
    ++
    [ "  ]"
    , "}"
    ]

----------
-- Request
----------

logRequest :: Config -> Request -> IO ()
logRequest config request = case logRequests $ logOptions config of
    Just Simple   -> hPutStrLn stderr $ simpleRequest request
    Just Detailed -> hPutStrLn stderr $ detailedRequest request
    _             -> return ()

simpleRequest :: Request -> String
simpleRequest r = intercalate " " $
    [ "Request:"
    , BS.unpack (HTTP.method r)
    , show (HTTP.getUri r)
    , show (HTTP.requestVersion r)
    ]

detailedRequest :: Request -> String
detailedRequest = intercalate "\n" . lines . show

-------------------
-- Request Override
-------------------

logRequestOverride :: Config -> RequestOverride -> Request -> Request -> IO ()
logRequestOverride config o r r' = case logRequestOverrides $ logOptions config of
    Just Simple   -> hPutStrLn stderr $ simpleRequestOverride r r'
    Just Detailed -> hPutStrLn stderr $ detailedRequestOverride o r r'
    _             -> return ()

simpleRequestOverride :: Request -> Request -> String
simpleRequestOverride r r' = intercalate " " $
    [ "Overriding request:"
    , show (HTTP.getUri r)
    , "->"
    , show (HTTP.getUri r')
    ]

detailedRequestOverride :: RequestOverride -> Request -> Request -> String
detailedRequestOverride o r r' = intercalate "\n" $
    [ simpleRequestOverride r r' ++ " according to rule:"
    , show o
    ]
