{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Cache
  ( cache
  , cacheNoBody
  , CacheBackend(..)
  , responseToLBS
  ) where

import           Blaze.ByteString.Builder (Builder, toLazyByteString)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as S8
import qualified Data.ByteString.Lazy     as LZ
import           Data.IORef
import           Network.Wai              (Middleware, Request, Response,
                                           requestBody, responseToStream,
                                           responseStatus, mapResponseHeaders)
import Network.HTTP.Types.Status (statusCode)

--------------------------------------------------------------------------------
-- | The data structure that should contains everything you need to create
-- a cache backend
data CacheBackend cacheContainer cacheKey cacheVal =
  CacheBackend {
  keyFromReq           :: Request -> ByteString -> IO cacheKey -- ^ Get cacheKey from request and its body
  , toCache            :: Request -> ByteString -> IO Bool -- ^ Function to check whether cache or not
  , addToCache         :: cacheContainer -> cacheKey -> cacheVal -> IO () -- ^ Adding to cache
  , actionOnCache      :: Request -> Response -> IO () -- ^ Action to perform before each caching request
  , actionOnCacheMiss  :: Request -> Response -> IO () -- ^ Action to perfom before each cache miss
  , responseToCacheVal :: Response -> IO cacheVal -- ^ Transform response to cached value
  , cacheValToResponse :: cacheVal -> Response -- ^ Transform cached value to response
  , lookupCache        :: cacheContainer -> cacheKey -> IO (Maybe cacheVal) -- ^ cache lookup
  , cacheContainer     :: cacheContainer -- ^ A cache container
  }

--------------------------------------------------------------------------------
-- Cache Backend Agnostic Cache Middleware
-- This version duplicate the body of the request making it quite far less efficient
-- than the cacheNoBody function
cache :: CacheBackend cc ck cv -- ^ A cache backend
      -> Middleware
cache cb app req sendResponse = do
  (req',body) <- getRequestBody req
  caching <- toCache cb req' body
  if not caching
     then app req' sendResponse
     else do
       (req'',_) <- getRequestBody req'
       cacheKey <- keyFromReq cb req'' body
       found <- lookupCache cb (cacheContainer cb) cacheKey
       maybe (app req'' (addToCacheAndRespond cb sendResponse req cacheKey))
         (respondFromCache cb sendResponse req'')
         found

--------------------------------------------------------------------------------
-- Cache Backend Agnostic Cache Middleware
-- This version don't provide the request body for create key or deciding
-- whether to cache. But it should be more efficient
cacheNoBody :: CacheBackend cc ck cv -- ^ A cache backend
               -> Middleware
cacheNoBody cb app req sendResponse = do
  caching <- toCache cb req S8.empty
  if not caching
     then app req sendResponse
     else do
       cacheKey <- keyFromReq cb req S8.empty
       found <- lookupCache cb (cacheContainer cb) cacheKey
       maybe (app req (addToCacheAndRespond cb sendResponse req cacheKey))
         (respondFromCache cb sendResponse req)
         found

addXCacheHeader :: Response -> Response
addXCacheHeader = mapResponseHeaders (("X-Cached","true"):)

respondFromCache :: CacheBackend cc ck cv
                 -> (Response -> IO b)
                 -> Request
                 -> cv
                 -> IO b
respondFromCache cb sendResponse r cachedVal = do
  let response = cacheValToResponse cb cachedVal
  actionOnCache cb r response
  sendResponse (addXCacheHeader response)

addToCacheAndRespond :: CacheBackend cc ck cv
                     -> (Response -> IO b)
                     -> Request
                     -> ck
                     -> Response
                     -> IO b
addToCacheAndRespond cb sendResponse req key r = do
  let code = statusCode (responseStatus r)
  if (code >= 200) && (code < 400)
    then do
      cacheVal <- responseToCacheVal cb r
      addToCache cb (cacheContainer cb) key cacheVal
      actionOnCacheMiss cb req r
      sendResponse (cacheValToResponse cb cacheVal)
    else
      sendResponse r

getRequestBody :: Request -> IO (Request, S8.ByteString)
getRequestBody req = do
  let loop front = do
         bs <- requestBody req
         if S8.null bs
             then return $ front []
             else loop $ front . (bs:)
  body <- loop id
  -- logging the body here consumes it, so fill it back up
  -- obviously not efficient
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
  return (req', S8.concat body)

-- | Helper for your cache backend
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
