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
import System.IO (hPutStrLn, stderr)

-- | Query Hackage for the packages a plan resolves and download the
-- releases it chooses.
defaultHackageSource :: IO (Maybe HackageSource)
defaultHackageSource = Just <$> newHackageSource (hPutStrLn stderr)
