-- |
-- Module      : Aihc.Cli.Hackage
-- Description : The Hackage source of a build with the Cabal flag +hackage
--
-- The Cabal flag @hackage@ selects this module or the one under
-- @hackage/disabled@. Both have the same interface.
module Aihc.Cli.Hackage
  ( defaultHackageSource,
  )
where

import Aihc.Hackage.Fetch (newHackageSource)
import Aihc.Hackage.Source (HackageSource)

-- | Query Hackage for the packages a plan resolves and download the
-- releases it chooses. The function shows the progress messages and the
-- warnings.
defaultHackageSource :: (String -> IO ()) -> IO (Maybe HackageSource)
defaultHackageSource message = Just <$> newHackageSource message
