{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Module:       $HEADER$
-- Description:  Example of using @DerivingStrategies@ along with default
--               HasVerbosity implementation based on generic-lens package.
-- Copyright:    (c) 2015-2019, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Example of using @DerivingStrategies@ along with default 'HasVerbosity'
-- implementation based on @generic-lens@ package.
module Example.Config
    ( Config
    , module Data.Verbosity.Class
    )
  where

import GHC.Generics (Generic)

import Data.Verbosity.Class


data Config = Config
    { _appVerbosity :: Verbosity
--  , ...
    }
  deriving stock (Generic, Show)
  deriving anyclass (HasVerbosity)
