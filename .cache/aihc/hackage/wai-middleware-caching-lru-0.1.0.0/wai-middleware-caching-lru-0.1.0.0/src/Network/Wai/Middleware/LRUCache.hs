{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.LRUCache
    ( cache
    , cacheNoBody
    , newCacheBackend
    , defaultCacheBackend
    ) where

import Network.Wai.Middleware.Cache (CacheBackend(..))
import qualified Network.Wai.Middleware.Cache as Cache

import           Blaze.ByteString.Builder  (Builder, toLazyByteString)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LZ
import           Data.Cache.LRU            (LRU, newLRU)
import qualified Data.Cache.LRU            as LRU
import           Data.IORef
import           Data.Text                 (Text)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Network.HTTP.Types.Status (Status)
import           Network.Wai               (Middleware, Request, Response,
                                            requestBody, pathInfo, requestMethod,
                                            rawQueryString, responseLBS,
                                            responseHeaders, responseStatus,
                                            responseToStream)
import qualified Data.ByteString.Char8 as S8

--------------------------------------------------------------------------------
data CacheKey = CacheKey { _pathInfo :: [Text]
                         , _reqBody  :: ByteString
                         , _rawQueryString :: ByteString
                         } deriving (Show, Eq, Ord)

data CacheValue = CacheValue { _body :: LZ.ByteString
                             , _headers :: ResponseHeaders
                             , _status :: Status
                             } deriving (Show)

type CacheContainer = IORef (LRU CacheKey CacheValue)

type LRUCacheBackend = CacheBackend CacheContainer CacheKey CacheValue

newCacheContainer :: Maybe Integer -> IO CacheContainer
newCacheContainer size = newIORef (newLRU size)

-- | Cache Backend which cache all GET requests with at most 10k different queries
-- You should use `cacheNoBody` instead of `cache`
defaultCacheBackend :: IO LRUCacheBackend
defaultCacheBackend = newCacheBackend (Just 10000)
                                      (\r _ -> return (requestMethod r == "GET"))
                                      (\_ _ -> return ())
                                      (\_ _ -> return ())

newCacheBackend :: Maybe Integer
                -> (Request -> ByteString -> IO Bool)
                -> (Request -> Response -> IO ())
                -> (Request -> Response -> IO ())
                -> IO LRUCacheBackend
newCacheBackend size toCacheF actionOnCacheF actionOnCacheMissF = do
    cacheContainer <- newCacheContainer size
    return CacheBackend {
            keyFromReq = keyFromReqF
            , toCache = toCacheF
            , addToCache = addToCacheF
            , actionOnCache = actionOnCacheF
            , actionOnCacheMiss = actionOnCacheMissF
            , responseToCacheVal = respToCacheValue
            , cacheValToResponse = cacheValToResponseF
            , lookupCache = lookupCacheF
            , cacheContainer = cacheContainer
            }

keyFromReqF req body = return (CacheKey (pathInfo req) body (rawQueryString req))

cacheValToResponseF cv = responseLBS (_status cv) (_headers cv) (_body cv)

lookupCacheF cacheContainer cacheKey = do
        cc <- readIORef cacheContainer
        return (snd (LRU.lookup cacheKey cc))

respToCacheValue :: Response -> IO CacheValue
respToCacheValue resp = do
  bodyLBS <- responseToLBS resp
  return (CacheValue bodyLBS (responseHeaders resp) (responseStatus resp))

addToCacheF :: CacheContainer -> CacheKey -> CacheValue -> IO ()
addToCacheF cc ckey resp = atomicModifyIORef' cc (\c -> (LRU.insert ckey resp c,()))

responseToLBS :: Response -> IO LZ.ByteString
responseToLBS response = do
  let (_,_,f) = responseToStream response
  f $ \streamingBody -> do
    builderRef <- newIORef mempty
    let add :: Builder -> IO ()
        add b = atomicModifyIORef builderRef $ \builder -> (builder `mappend` b,())
        flush :: IO ()
        flush = return ()
    streamingBody add flush
    fmap toLazyByteString (readIORef builderRef)

cache :: LRUCacheBackend -> Middleware
cache = Cache.cache

cacheNoBody :: LRUCacheBackend -> Middleware
cacheNoBody = Cache.cacheNoBody
