{- |
   Module     : Development.Shake.Plus.Core
   License    : MIT
   Stability  : experimental

Core definitions of shake-plus.
-}
module Development.Shake.Plus.Core (
  MonadAction(..)
, MonadRules(..)
, UnliftAction(..)
, MonadUnliftAction(..)
, withUnliftAction
, askUnliftAction
, toAction
, RAction
, ShakePlus
, runRAction
, runShakePlus
, parallel
, forP
, par
, Development.Shake.Action
, Development.Shake.Rules
, Development.Shake.FilePattern
, Development.Shake.RuleResult
, Development.Shake.ShakeValue
, Development.Shake.shake
, Development.Shake.shakeArgs
, Development.Shake.shakeOptions
, Development.Shake.ShakeOptions(..)
) where

import           Control.Exception
import           Development.Shake (Action, FilePattern, Rules)
import qualified Development.Shake
import           RIO

-- | Monads in which `Action`s may be embedded.
class MonadIO m => MonadAction m where
  liftAction :: Action a -> m a

instance MonadAction Action where
  liftAction = id

instance MonadAction m => MonadAction (ReaderT r m) where
  liftAction = lift . liftAction

newtype UnliftAction m = UnliftAction { unliftAction :: forall a. m a -> Action a }

-- | Monads which allow their actions to be run in 'Action'.
--
-- For the same reasons as `MonadUnliftIO` this is limited to 'ReaderT'
-- and `IdentityT` transformers on top of `Action'.
class MonadAction m => MonadUnliftAction m where
  {-# INLINE withRunInAction #-}
  withRunInAction :: ((forall a. m a -> Action a) -> Action b) -> m b
  withRunInAction inner = askUnliftAction >>= \u -> liftAction (inner (unliftAction u))

instance MonadUnliftAction Action where
  {-# INLINE withRunInAction #-}
  withRunInAction inner = inner id

instance MonadUnliftAction m => MonadUnliftAction (ReaderT r m) where
  {-# INLINE withRunInAction #-}
  withRunInAction inner =
    ReaderT $ \r ->
    withRunInAction $ \run ->
    inner (run . flip runReaderT r)

class Monad m => MonadRules m where
  liftRules :: Rules a -> m a

instance MonadRules Rules where
  liftRules = id

instance MonadRules m => MonadRules (ReaderT r m) where
  liftRules = lift . liftRules

withUnliftAction :: MonadUnliftAction m => (UnliftAction m -> Action a) -> m a
withUnliftAction inner = askUnliftAction >>= liftAction . inner

askUnliftAction :: MonadUnliftAction m => m (UnliftAction m)
askUnliftAction = withRunInAction (\run -> return (UnliftAction run))

toAction :: MonadUnliftAction m => m a -> m (Action a)
toAction m = withRunInAction $ \run -> return $ run m

-- | Concrete `Action` runner, hardcoded to `ReaderT r Action a`.
newtype RAction r a = RAction (ReaderT r Action a)
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadAction, MonadUnliftAction, MonadFail)

-- | Concrete `Rules` collector, hardcoded to `ReaderT r Rules a`.
newtype ShakePlus r a = ShakePlus (ReaderT r Rules a)
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadRules)

-- | Run an `RAction` with an environment, consuming it for a result.
runRAction :: MonadAction m => env -> RAction env a -> m a
runRAction r (RAction (ReaderT f)) = liftAction (f r)

-- | Run a `ShakePlus` with an environment, consuming it for some Shake `Rules`.
runShakePlus :: MonadRules m => env -> ShakePlus env a -> m a
runShakePlus r (ShakePlus (ReaderT f)) = liftRules (f r)

instance MonadThrow (RAction r) where
  throwM = liftIO . Control.Exception.throwIO

instance MonadThrow (ShakePlus r) where
  throwM = liftIO . Control.Exception.throwIO

-- | Unlifted `Development.Shake.parallel`.
parallel :: MonadUnliftAction m => [m a] -> m [a]
parallel xs = withRunInAction $ \run -> Development.Shake.parallel $ fmap run xs

-- | Unlifted `Development.Shake.forP`.
forP :: MonadUnliftAction m => [a] -> (a -> m b) -> m [b]
forP x f = withRunInAction $ \run -> Development.Shake.forP x $ run . f

-- | Unlifted `Development.Shake.par`.
par :: MonadUnliftAction m => m a -> m b -> m (a, b)
par a b = withRunInAction $ \run -> Development.Shake.par (run a) (run b)
