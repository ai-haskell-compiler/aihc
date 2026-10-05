-- |
-- Module      : Aihc.Haddock.Hackage
-- Description : The Hackage source of a build with the Cabal flag +hackage
--
-- The Cabal flag @hackage@ selects this module or the one under
-- @hackage/disabled@. Both have the same interface.
module Aihc.Haddock.Hackage
  ( defaultHackageSource,
  )
where

import Aihc.Hackage.Fetch (newHackageSource)
import Aihc.Hackage.Source (HackageSource)

-- | Read the Hackage index and download the releases that a plan chooses.
defaultHackageSource :: IO (Maybe HackageSource)
defaultHackageSource = Just <$> newHackageSource
