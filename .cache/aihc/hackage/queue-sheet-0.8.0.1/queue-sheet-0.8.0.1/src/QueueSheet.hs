------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet
-- Description : queue sheet metadata
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module QueueSheet
  ( -- * Constants
    version
  ) where

-- https://hackage.haskell.org/package/base
import Data.Version (showVersion)

-- (queue-sheet:cabal)
import qualified Paths_queue_sheet as Project

------------------------------------------------------------------------------
-- $Constants

-- | QueueSheet version string (\"@queue-sheet-haskell X.X.X.X@\")
--
-- @since 0.3.0.0
version :: String
version = "queue-sheet-haskell " ++ showVersion Project.version
