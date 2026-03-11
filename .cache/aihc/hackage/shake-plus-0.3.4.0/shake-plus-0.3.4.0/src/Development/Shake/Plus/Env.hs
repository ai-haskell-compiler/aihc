{- |
   Module     : Development.Shake.Plus.Env
   License    : MIT
   Stability  : experimental

Environenment variable utilities in "Development.Shake" lifted to `MonadAction`.
-}
module Development.Shake.Plus.Env (
  getEnv
, getEnvWithDefault
, getEnvError
) where

import           Control.Exception.Extra
import qualified Development.Shake
import           Development.Shake.Plus
import           RIO

-- | Lifted version of `Development.Shake.getEnv`
getEnv :: MonadAction m => String -> m (Maybe String)
getEnv = liftAction . Development.Shake.getEnv

-- | Lifted version of `Development.Shake.getEnvWithDefault`
getEnvWithDefault :: MonadAction m => String -> String -> m String
getEnvWithDefault def var = liftAction $ Development.Shake.getEnvWithDefault def var

-- | Lifted version of `Development.Shake.getEnvError`
getEnvError :: (Partial, MonadAction m) => String -> m String
getEnvError = liftAction . Development.Shake.getEnvError
