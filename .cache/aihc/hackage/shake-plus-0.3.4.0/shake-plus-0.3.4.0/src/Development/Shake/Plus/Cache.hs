{- |
   Module     : Development.Shake.Plus.Cache
   License    : MIT
   Stability  : experimental

Cache utilities in "Development.Shake" lifted to `MonadAction`.
-}
module Development.Shake.Plus.Cache (
  newCache
, newCacheIO
) where

import qualified Development.Shake
import           Development.Shake.Plus.Core
import           RIO

-- | Lifted version of `Development.Shake.newCache` using `RAction`.
newCache :: (MonadRules m, MonadReader r m, Eq k, Hashable k)
         => (k -> RAction r v)
         -> m (k -> RAction r v)
newCache ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.newCache (runRAction r . ract)

-- | Lifted version of `Development.Shake.newCacheIO` using `RAction`.
newCacheIO :: (MonadIO m, MonadReader r m, Eq k, Hashable k) => (k -> RAction r v) -> m (k -> RAction r v)
newCacheIO f = ask >>= \r -> liftIO $ (liftAction . ) <$> Development.Shake.newCacheIO (\y -> runRAction r (f y))
