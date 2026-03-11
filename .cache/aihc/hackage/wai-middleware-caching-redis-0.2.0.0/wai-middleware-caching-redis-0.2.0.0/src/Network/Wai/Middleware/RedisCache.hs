{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.Wai.Middleware.RedisCache
    ( cache
    , cacheNoBody
    , newCacheBackend
    , defaultCacheBackend
    , defaultConnectInfo
    , ConnectInfo(..)
    , PortID (..)
    ) where

import Network.Wai.Middleware.Cache (CacheBackend(..))
import qualified Network.Wai.Middleware.Cache as Cache

import           Blaze.ByteString.Builder  (Builder, toLazyByteString)
import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as LZ
import           Data.IORef
import           Data.Text                 (Text)
import           Database.Redis            (ConnectInfo(..), Connection, PortID(..), connect,
                                            get, runRedis, set, defaultConnectInfo)
import           Network.HTTP.Types.Header (ResponseHeaders)
import           Network.HTTP.Types.Status (Status (..))
import           Network.Wai               (Middleware, Request, Response,
                                            pathInfo, requestMethod, rawQueryString,
                                            requestBody, responseHeaders,
                                            responseLBS, responseStatus,
                                            responseToStream)

--------------------------------------------------------------------------------
data CacheKey = CacheKey { _pathInfo       :: [Text]
                         , _reqBody        :: ByteString
                         , _rawQueryString :: ByteString
                         } deriving (Show, Eq, Ord)

deriving instance Read Status

data CacheValue = CacheValue { _body    :: LZ.ByteString
                             , _headers :: ResponseHeaders
                             , _status  :: Status
                             } deriving (Show,Read)


type CacheContainer = Connection
type RedisCacheBackend = CacheBackend CacheContainer CacheKey CacheValue

newCacheContainer :: ConnectInfo -> IO CacheContainer
newCacheContainer = connect

newCacheBackend :: ConnectInfo
                -> (Request -> ByteString -> IO Bool)
                -> (Request -> Response -> IO ())
                -> (Request -> Response -> IO ())
                -> IO RedisCacheBackend
newCacheBackend connectInfo toCacheF actionOnCacheF actionOnCacheMissF = do
    cacheContainer <- newCacheContainer connectInfo
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

-- | Cache Backend which cache all GET requests using local redis on standard port
-- You should use `cacheNoBody` instead of `cache`
defaultCacheBackend :: IO RedisCacheBackend
defaultCacheBackend = newCacheBackend defaultConnectInfo
                                      (\r _ -> return (requestMethod r == "GET"))
                                      (\_ _ -> return ())
                                      (\_ _ -> return ())

respToCacheValue :: Response -> IO CacheValue
respToCacheValue resp = do
  bodyLBS <- responseToLBS resp
  return (CacheValue bodyLBS (responseHeaders resp) (responseStatus resp))


keyFromReqF :: Request -> ByteString -> IO CacheKey
keyFromReqF req body = return (CacheKey (pathInfo req) body (rawQueryString req))

cacheValToResponseF :: CacheValue -> Response
cacheValToResponseF cv = responseLBS (_status cv) (_headers cv) (_body cv)

addToCacheF :: CacheContainer -> CacheKey -> CacheValue -> IO ()
addToCacheF cc ckey resp = void $ runRedis cc $
  set (S8.pack (show ckey)) (S8.pack (show resp))

getRequestBody :: Request -> IO (Request, [S8.ByteString])
getRequestBody req = do
  let loop front = do
         bs <- requestBody req
         if S8.null bs
             then return $ front []
             else loop $ front . (bs:)
  body <- loop id
  -- logging the body here consumes it, so fill it back up
  -- obviously not efficient, but this is the development logger
  --
  -- Note: previously, we simply used CL.sourceList. However,
  -- that meant that you could read the request body in twice.
  -- While that in itself is not a problem, the issue is that,
  -- in production, you wouldn't be able to do this, and
  -- therefore some bugs wouldn't show up during testing. This
  -- implementation ensures that each chunk is only returned
  -- once.
  ichunks <- newIORef body
  let rbody = atomicModifyIORef ichunks $ \chunks ->
         case chunks of
             [] -> ([], S8.empty)
             x:y -> (y, x)
  let req' = req { requestBody = rbody }
  return (req', body)

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

readMaybe :: (Read a) => ByteString -> Maybe a
readMaybe bs =
  case reads (S8.unpack bs) of
     [(x,"")] -> Just x
     _ -> Nothing

lookupCacheF :: CacheContainer -> CacheKey -> IO (Maybe CacheValue)
lookupCacheF cc cacheKey = do
  res <- runRedis cc $ get bsCacheKey
  return $ either (const Nothing) bsToMCacheVal res
  where
    bsToMCacheVal (Just bs) = readMaybe bs
    bsToMCacheVal Nothing = Nothing
    bsCacheKey = (S8.pack . show) cacheKey

cache :: RedisCacheBackend -> Middleware
cache = Cache.cache

cacheNoBody :: RedisCacheBackend -> Middleware
cacheNoBody = Cache.cacheNoBody
