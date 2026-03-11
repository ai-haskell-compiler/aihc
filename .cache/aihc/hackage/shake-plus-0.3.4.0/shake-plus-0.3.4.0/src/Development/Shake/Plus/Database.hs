{- |
   Module     : Development.Shake.Plus.Database
   License    : MIT
   Stability  : experimental

Utilities in "Development.Shake.Database" lifted to `MonadIO` and `MonadUnliftIO`.
-}
module Development.Shake.Plus.Database (
  Development.Shake.Database.ShakeDatabase
, shakeOpenDatabase
, shakeWithDatabase
, shakeOneShotDatabase
, shakeRunDatabase
, shakeLiveFilesDatabase
, shakeProfileDatabase
, shakeErrorsDatabase
, shakeRunAfter
) where

import           Development.Shake          (Action, Rules, ShakeOptions)
import           Development.Shake.Database (ShakeDatabase)
import qualified Development.Shake.Database
import           Path
import           RIO

-- | Lifted `Development.Shake.Database.shakeOpenDatabase`
shakeOpenDatabase :: MonadIO m => ShakeOptions -> Rules () -> m (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = liftIO $ Development.Shake.Database.shakeOpenDatabase opts rules

-- | Unlifted `Development.Shake.Database.shakeWithDatabase`
shakeWithDatabase :: MonadUnliftIO m => ShakeOptions -> Rules () -> (ShakeDatabase -> m a) -> m a
shakeWithDatabase opts rules inner = withRunInIO $ \run -> Development.Shake.Database.shakeWithDatabase opts rules $ run . inner

-- | Lifted `Development.Shake.Database.shakeOneShotDatabase`
shakeOneShotDatabase :: MonadIO m => ShakeDatabase -> m ()
shakeOneShotDatabase = liftIO . Development.Shake.Database.shakeOneShotDatabase

-- | Lifted `Development.Shake.Database.shakeRunDatabase`
shakeRunDatabase :: MonadIO m => ShakeDatabase -> [Action a] -> m ([a], [IO ()])
shakeRunDatabase db actions = liftIO $ shakeRunDatabase db actions

-- | Lifted `Development.Shake.Database.shakeLiveFilesDatabase`
shakeLiveFilesDatabase :: MonadIO m => ShakeDatabase -> m [FilePath]
shakeLiveFilesDatabase = liftIO . Development.Shake.Database.shakeLiveFilesDatabase

-- | Lifted `Development.Shake.Database.shakeProfileDatabase` with well-typed path.
shakeProfileDatabase :: MonadIO m => ShakeDatabase -> Path a File -> m ()
shakeProfileDatabase db file = liftIO $ Development.Shake.Database.shakeProfileDatabase db (toFilePath file)

-- | Lifted `Development.Shake.Database.shakeErrorsDatabase`
shakeErrorsDatabase :: MonadIO m => ShakeDatabase -> m [(String, SomeException)]
shakeErrorsDatabase = liftIO . Development.Shake.Database.shakeErrorsDatabase

-- | Unlifted `Development.Shake.Database.shakeRunAfter`
shakeRunAfter :: MonadUnliftIO m => ShakeOptions -> [m ()] -> m ()
shakeRunAfter opts inners = withRunInIO $ \run -> Development.Shake.Database.shakeRunAfter opts $ fmap run inners
