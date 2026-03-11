module UnliftIO.Pool
  ( Pool
  , P.PoolConfig
  , P.setNumStripes
  , LocalPool
  , mkDefaultPoolConfig
  , newPool
  , withResource
  , takeResource
  , tryWithResource
  , tryTakeResource
  , destroyResource
  , putResource
  , destroyAllResources
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO, unliftIO)
import qualified Data.Pool as P
import           Data.Pool (PoolConfig)

type Pool = P.Pool

type LocalPool = P.LocalPool

mkDefaultPoolConfig :: MonadUnliftIO m => m a -> (a -> m ()) -> Double -> Int -> m (PoolConfig a)
mkDefaultPoolConfig create destroy keepAlive maxOpen =
  withRunInIO $ \io ->
    pure $ P.defaultPoolConfig (io create) (io . destroy) keepAlive maxOpen

-- | Lifted version of 'P.newPool'
--
-- @since 0.4.2.0
newPool :: MonadUnliftIO m => PoolConfig a -> m (Pool a)
newPool config =
  withRunInIO $ \io ->
    liftIO $ P.newPool config

withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool action =
  withRunInIO $ \io ->
    liftIO $ P.withResource pool $ \a ->
      io $ action a

takeResource :: MonadUnliftIO m => Pool a -> m (a, LocalPool a)
takeResource pool = liftIO $ P.takeResource pool

tryWithResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m (Maybe b)
tryWithResource pool action =
  withRunInIO $ \io ->
    liftIO $ P.tryWithResource pool $ \a ->
      io $ action a

tryTakeResource :: MonadUnliftIO m => Pool a -> m (Maybe (a, LocalPool a))
tryTakeResource pool = liftIO $ P.tryTakeResource pool

destroyResource :: MonadUnliftIO m => Pool a -> LocalPool a -> a -> m ()
destroyResource pool localPool a = liftIO $ P.destroyResource pool localPool a

putResource :: MonadUnliftIO m => LocalPool a -> a -> m ()
putResource localPool a = liftIO $ P.putResource localPool a

destroyAllResources :: MonadUnliftIO m => Pool a -> m ()
destroyAllResources pool = liftIO $ P.destroyAllResources pool
