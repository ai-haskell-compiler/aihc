{- |
   Module     : Text.Pandoc.Throw
   License    : MIT
   Stability  : experimental

MonadThrow behaviour for Pandoc
-}
module Text.Pandoc.Throw (
  runPandocPureThrow
, runPandocIOThrow
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Text.Pandoc

-- | Run a `PandocPure` expression, lifting errors to `MonadThrow`.
runPandocPureThrow :: MonadThrow m => PandocPure a -> m a
runPandocPureThrow = either throwM return . runPure

-- | Run a `PandocIO` expression, lifting errors to `MonadThrow`.
runPandocIOThrow :: (MonadIO m, MonadThrow m) => PandocIO a -> m a
runPandocIOThrow p = do
  result <- liftIO $ runIO p
  either throwM return result
