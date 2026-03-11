{- |
   Module     : Development.Shake.Plus.Forward
   License    : MIT
   Stability  : experimental

Forward mode utilities in "Development.Shake.Forward" lifted to `MonadAction` and `MonadRules`.
-}
module Development.Shake.Plus.Forward (
  shakeForward
, shakeArgsForward
, Development.Shake.Forward.forwardOptions
, forwardRule
, cache
, cacheAction
, cacheActionWith
) where

import           Development.Shake.Classes
import qualified Development.Shake.Forward
import           Development.Shake.Plus.Command
import           Development.Shake.Plus.Core
import           RIO

-- | Lifted version of `Development.Shake.Forward.shakeForward` using `RAction`.
shakeForward :: MonadIO m => ShakeOptions -> r -> RAction r () -> m ()
shakeForward opts env ract = liftIO $ Development.Shake.Forward.shakeForward opts (runRAction env ract)

-- | Lifted version of `Development.Shake.Forward.shakeArgsForward` using `RAction`.
shakeArgsForward :: MonadIO m => ShakeOptions -> r -> RAction r () -> m ()
shakeArgsForward opts env ract = liftIO $ Development.Shake.Forward.shakeArgsForward opts (runRAction env ract)

-- | Lifted version of `Development.Shake.forwardRule` using `RAction`.
forwardRule :: (MonadReader r m, MonadRules m) => RAction r () -> m ()
forwardRule ract = ask >>= \r -> liftRules $ Development.Shake.Forward.forwardRule (runRAction r ract)

-- | Lifted version of `Development.Shake.cache`.
cache :: MonadAction m => (forall r. CmdArguments r => r) -> m ()
cache = \c -> liftAction c

-- | Lifted version of `Development.Shake.Forward.cacheAction`.
cacheAction :: (MonadUnliftAction m, Typeable a, Binary a, Show a, Typeable b, Binary b, Show b) => a -> m b -> m b
cacheAction x a = withRunInAction $ \run -> Development.Shake.Forward.cacheAction x (run a)

-- | Lifted version of `Development.Shake.Forward.cacheActionWith`.
cacheActionWith :: (MonadUnliftAction m, Typeable a, Binary a, Show a, Typeable b, Binary b, Show b, Typeable c, Binary c, Show c) => a -> b -> m c -> m c
cacheActionWith a b m = withRunInAction $ \run -> Development.Shake.Forward.cacheActionWith a b (run m)
