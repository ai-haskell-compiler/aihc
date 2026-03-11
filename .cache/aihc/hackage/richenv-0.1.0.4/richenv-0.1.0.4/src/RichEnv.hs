-- | This module provides functions to set environment variables or retrieve an environment variable list according to a 'RichEnv' object input, which defines:
--
-- * A list of environment variables to be set with their values.
-- * Mapping the name from one existing environment variable name to another (If there's an environment variable @__FOO=bar__@, a mapping @__(\"SOME\", \"FOO\")__@ will generate an environment variable definition @__SOME__@ with the contents of the variable @__FOO__@).
-- * Mapping the prefixes of existing environment variables to a new prefix (If there's an environment variable @__FOO_VAR=bar__@, a prefix mapping @__(\"SOME\", [\"FOO\"])__@ will generate an environment variable definition @__SOME_VAR__@ with the contents of the variable @__FOO_VAR__@).
module RichEnv
  ( -- * Types
    RichEnv (..),
    Environment,

    -- * Environment transformations
    toEnvList,
    toEnvMap,

    -- * Functions using 'IO' to get the environment from the current process
    setRichEnv,
    setRichEnvFromCurrent,
    toEnvListFromCurrent,
    toEnvMapFromCurrent,
    clearEnvironment,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import RichEnv.Setters (richEnvToValues, valuesToEnv, valuesToEnvList)
import RichEnv.Types (Environment, RichEnv (..), fromEnvironment, toEnvironment)
import RichEnv.Types.Values (Values (unValues))
import System.Environment (getEnvironment, unsetEnv)

-- | Get a key-value list of environment variables processing the passed environment with the 'RichEnv' input.
--
-- > toEnvList re env = valuesToEnvList (toEnvValues re env)
toEnvList :: RichEnv -> Environment -> Environment
toEnvList re = valuesToEnvList . toEnvValues re

-- | Get a hashmap of environment variables processing the passed environment with the 'RichEnv' input. The idea is that the output could be passed to functions like [Yaml](https://hackage.haskell.org/package/yaml)'s [applyEnvValue](https://hackage.haskell.org/package/yaml/docs/Data-Yaml-Config.html#v:applyEnvValue).
--
-- > toEnvMap re env = unValues (toEnvValues re env)
toEnvMap :: RichEnv -> Environment -> HM.HashMap Text Text
toEnvMap re = unValues . toEnvValues re

-- | Builds a 'Values' object from the 'RichEnv' input and a list of environment variables.
toEnvValues :: RichEnv -> Environment -> Values
toEnvValues = richEnvToValues

-- | Sets the environment variables available for the current process abiding to the 'RichEnv' rules.
setRichEnv :: RichEnv -> Environment -> IO ()
setRichEnv re env = do
  clearEnvironment $ fromEnvironment env
  valuesToEnv (richEnvToValues re env)

-- | Sets the environment variables available for the current process by checking the current environment variables and applying the 'RichEnv' rules.
--
-- > setRichEnvFromCurrent re = getEnvironment >>= setRichEnv re . toEnvironment
setRichEnvFromCurrent :: RichEnv -> IO ()
setRichEnvFromCurrent re = getEnvironment >>= setRichEnv re . toEnvironment

-- | Get a key-value list of environment variables processing the current environment with the 'RichEnv' input.
--
-- > toEnvListFromCurrent re = toEnvList re . toEnvironment <$> getEnvironment
toEnvListFromCurrent :: RichEnv -> IO Environment
toEnvListFromCurrent re = toEnvList re . toEnvironment <$> getEnvironment

-- | Get a hashmap of environment variables processing the current environment with the 'RichEnv' input. The idea is that the output could be passed to functions like [Yaml](https://hackage.haskell.org/package/yaml)'s [applyEnvValue](https://hackage.haskell.org/package/yaml/docs/Data-Yaml-Config.html#v:applyEnvValue).
--
-- > toEnvMapFromCurrent re = toEnvMap re . toEnvironment <$> getEnvironment
toEnvMapFromCurrent :: RichEnv -> IO (HM.HashMap Text Text)
toEnvMapFromCurrent re = toEnvMap re . toEnvironment <$> getEnvironment

-- | Builds a 'Values' object from the 'RichEnv' input and the current environment variables.
--
-- > toEnvValuesFromCurrent re = toEnvValues re . toEnvironment <$> getEnvironment
_toEnvValuesFromCurrent :: RichEnv -> IO Values
_toEnvValuesFromCurrent re = toEnvValues re . toEnvironment <$> getEnvironment

-- | Clears all environment variables of the current process.
clearEnvironment ::
  [(String, String)] ->
  IO ()
clearEnvironment = mapM_ (unsetEnv . fst)
