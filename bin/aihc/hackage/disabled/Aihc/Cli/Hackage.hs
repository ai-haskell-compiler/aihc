-- |
-- Module      : Aihc.Cli.Hackage
-- Description : The Hackage source of a build with the Cabal flag -hackage
--
-- The Cabal flag @hackage@ selects this module or the one under
-- @hackage/enabled@. Both have the same interface. This build does not
-- depend on @aihc-hackage-fetch@, so it has no network code.
module Aihc.Cli.Hackage
  ( defaultHackageSource,
  )
where

import Aihc.Hackage.Source (HackageSource)

-- | No Hackage source: a plan uses only local packages and core libraries.
defaultHackageSource :: IO (Maybe HackageSource)
defaultHackageSource = pure Nothing
