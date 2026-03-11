{- |
   Module     : Development.Shake.Plus.Config
   License    : MIT
   Stability  : experimental

Utilities in "Development.Shake.Config" lifted to `MonadAction` and well-typed `Path`s.
-}
module Development.Shake.Plus.Config (
  readConfigFile
, readConfigFileWithEnv
, usingConfigFile
, usingConfig
, getConfig
, getConfigKeys
) where

import           Development.Shake
import qualified Development.Shake.Config
import           Development.Shake.Plus.Core
import           Path
import           RIO

-- | Lifted `Development.Shake.Config.readConfigFile` with well-typed path.
readConfigFile :: MonadIO m => Path b File -> m (HashMap String String)
readConfigFile = liftIO . Development.Shake.Config.readConfigFile . toFilePath

-- | Lifted `Development.Shake.Config.readConfigFileWithEnv` with well-typed path.
readConfigFileWithEnv :: MonadIO m => [(String, String)] -> Path b File -> m (HashMap String String)
readConfigFileWithEnv vars file = liftIO $ Development.Shake.Config.readConfigFileWithEnv vars (toFilePath file)

-- | Lifted `Development.Shake.Config.usingConfigFile` with well-typed path.
usingConfigFile :: MonadRules m => Path b File -> m ()
usingConfigFile = liftRules . Development.Shake.Config.usingConfigFile . toFilePath

-- | Lifted `Development.Shake.Config.usingConfig`.
usingConfig :: MonadRules m => HashMap String String -> m ()
usingConfig = liftRules . Development.Shake.Config.usingConfig

-- | Lifted `Development.Shake.Config.getConfig`.
getConfig :: MonadAction m => String -> m (Maybe String)
getConfig = liftAction . Development.Shake.Config.getConfig

-- | Lifted `Development.Shake.Config.getConfigKeys`.
getConfigKeys :: MonadAction m => m [String]
getConfigKeys = liftAction Development.Shake.Config.getConfigKeys
