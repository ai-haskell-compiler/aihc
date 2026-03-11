module UnliftIO.Streams.Combinators
  ( filter
  , fold
  , foldM
  , map
  , mapM
  , unfoldM
  , zip
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO, liftIO)
import           Prelude hiding (filter, map, mapM, zip)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

{-# INLINE filter #-}
filter :: (MonadUnliftIO m) => (a -> Bool) -> InputStream a -> m (InputStream a)
filter f is = liftIO $ SC.filter f is

{-# INLINE fold #-}
fold :: (MonadUnliftIO m) => (s -> a -> s) -> s -> InputStream a -> m s
fold f s0 is = liftIO $ SC.fold f s0 is

{-# INLINE foldM #-}
foldM :: MonadUnliftIO m => (s -> a -> m s) -> s -> InputStream a -> m s
foldM f s0 is =
  withRunInIO $ \io ->
    SC.foldM (\s a -> io $ f s a) s0 is

{-# INLINE map #-}
map :: MonadUnliftIO m => (a -> b) -> InputStream a -> m (InputStream b)
map f is = liftIO $ SC.map f is

{-# INLINE mapM #-}
mapM :: MonadUnliftIO m => (a -> m b) -> InputStream a -> m (InputStream b)
mapM f is =
  withRunInIO $ \io ->
    SC.mapM (io . f) is

{-# INLINE unfoldM #-}
unfoldM :: MonadUnliftIO m => (b -> m (Maybe (a, b))) -> b -> m (InputStream a)
unfoldM f s =
  withRunInIO $ \io ->
    SC.unfoldM (io . f) s

{-# INLINE zip #-}
zip :: MonadUnliftIO m => InputStream a -> InputStream b -> m (InputStream (a, b))
zip as bs = liftIO $ SC.zip as bs
