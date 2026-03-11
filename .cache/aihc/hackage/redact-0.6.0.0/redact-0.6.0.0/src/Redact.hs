------------------------------------------------------------------------------
-- |
-- Module      : Redact
-- Description : metadata
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Redact
  ( -- * Constants
    version
  ) where

-- https://hackage.haskell.org/package/base
import Data.Version (showVersion)

-- (redact:cabal)
import qualified Paths_redact as Project

------------------------------------------------------------------------------
-- $Constants

-- | Redact version string (\"@redact-haskell X.X.X.X@\")
--
-- @since 0.4.0.0
version :: String
version = "redact-haskell " ++ showVersion Project.version
