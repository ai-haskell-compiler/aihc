module Control.Monad.IO.Class
  ( MonadIO (..),
  )
where

import Prelude (IO, Monad)

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = idIO

idIO :: IO a -> IO a
idIO action = action
