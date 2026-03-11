{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Main.Options
    ( AppConfig(..)
    , quietFlag
    , incrementVerbosityFlag
    )
  where

import GHC.Generics (Generic)

import Data.Verbosity (Verbosity)
import qualified Data.Verbosity as Verbosity (Verbosity(Silent), increment')
import Data.Verbosity.Class (HasVerbosity, modifyVerbosity, setVerbosity)
import qualified Options.Applicative as Options


-- | Application configuration.
data AppConfig = AppConfig
    { verbosity :: Verbosity
--  , ...
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasVerbosity)

-- | Option for suppressing unnecessary output.
--
-- > -q, --quiet
-- >     Quiet mode. Suppress normal diagnostic or result output.
quietFlag :: HasVerbosity a => Options.Parser (a -> a)
quietFlag = Options.flag id (setVerbosity Verbosity.Silent) $ mconcat
    [ Options.long "quiet"
    , Options.short 'q'
    , Options.help "Quiet mode. Suppress normal diagnostic or result output."
    ]

-- | Flag for incrementing verbosity by one level. It can be used multiple
-- times to increase it more.
--
-- > -v
-- >     Increment verbosity by one level. Can be used multiple times.
--
-- See 'Verbosity.increment'' for more details.
--
-- Note that this definition uses 'Options.flag'' under the hood to allow using
-- 'Control.Applicative.some' and 'Control.Applicative.many' combinators.  In
-- other words, it will fail when used without these combinators or
-- 'Control.Applicative.optional'.
incrementVerbosityFlag :: HasVerbosity a => Options.Parser (a -> a)
incrementVerbosityFlag =
    Options.flag' (modifyVerbosity Verbosity.increment') $ mconcat
        [ Options.short 'v'
        , Options.help "Increment verbosity by one level. Can be used multiple times."
        ]
