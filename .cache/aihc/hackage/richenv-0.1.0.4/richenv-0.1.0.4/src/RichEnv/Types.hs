-- | This module contains the basic types used by the library and their typeclass instances.
module RichEnv.Types
  ( -- * Types
    RichEnv (..),
    defaultRichEnv,
    Environment,
    Mappings (..),
    Values (..),
    Prefixes (..),

    -- * Environment transformations
    toEnvironment,
    fromEnvironment,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import RichEnv.Types.Mappings (Mappings (..))
import RichEnv.Types.Prefixes (Prefixes (..))
import RichEnv.Types.Values (Values (..))

-- | A list of key-value pairs representing environment variables.
type Environment = [(Text, Text)]

-- | Get back a @[(String, String)]@ from an 'Environment'.
fromEnvironment :: Environment -> [(String, String)]
fromEnvironment = fmap (bimap T.unpack T.unpack)

-- | Transform the type returned from 'System.Environment.getEnvironment' (@[(String, String)]@) to use 'Text' instead.
toEnvironment :: [(String, String)] -> Environment
toEnvironment = fmap (bimap T.pack T.pack)

-- | Type that represents a set of rules that generate environment variables. A value of this type can be retrieved from a configuration file (e.g. YAML) due to its 'FromJSON' instance, or persisted into one with 'ToJSON'.
data RichEnv = RichEnv
  { -- | A list of environment variables to be set with their values.
    values :: Values,
    -- | Mappings from one existing environment variable name to another.
    mappings :: Mappings,
    -- | Mappings from different prefixes of existing environment variables to new prefixes.
    prefixes :: Prefixes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup RichEnv where
  (<>) :: RichEnv -> RichEnv -> RichEnv
  (<>) (RichEnv a b c) (RichEnv d e f) = RichEnv (a <> d) (b <> e) (c <> f)

instance Monoid RichEnv where
  mempty :: RichEnv
  mempty = RichEnv mempty mempty mempty

-- | Default 'RichEnv' value. With everything empty.
defaultRichEnv :: RichEnv
defaultRichEnv = mempty
