{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.Cache (Cache, newCache, fetch) where

import Control.Concurrent.MVar
import Control.Monad (liftM)
import Data.Typeable (Typeable)

-- | A thread-safe write-once cache. If you need more functionality,
-- (e.g. multiple write, cache clearing) use an 'MVar' instead.
newtype Cache a = Cache (MVar (Maybe a))
  deriving (Eq, Typeable)

-- | Fetch the value stored in the cache,
-- or call the supplied fallback and store the result,
-- if the cache is empty.
fetch :: Cache a -> IO a -> IO a
fetch (Cache var) action = go where
  go = readMVar var >>= \m -> case m of
    Just a -> return a
    Nothing -> do
      modifyMVar_ var $ \m' -> case m' of
        Just a -> return (Just a)
        Nothing -> liftM Just action
      go

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = do
  var <- newMVar Nothing
  return (Cache var)
