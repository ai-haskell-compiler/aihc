module UnliftIO.Streams.List
  ( chunkList
  , chunkListWith
  , fromList
  , toList
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Prelude hiding (map)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.List as SL

{-# INLINE chunkList #-}
chunkList :: (MonadUnliftIO m) => Int -> InputStream a -> m (InputStream [a])
chunkList n is = liftIO $ SL.chunkList n is

{-# INLINE chunkListWith #-}
chunkListWith :: (MonadUnliftIO m) => (a -> Int -> Bool) -> InputStream a -> m (InputStream [a])
chunkListWith p is = liftIO $ SL.chunkListWith p is

{-# INLINE toList #-}
toList :: (MonadUnliftIO m) => InputStream a -> m [a]
toList = liftIO . SL.toList

{-# INLINE fromList #-}
fromList :: (MonadUnliftIO m) => [a] -> m (InputStream a)
fromList = liftIO . SL.fromList
