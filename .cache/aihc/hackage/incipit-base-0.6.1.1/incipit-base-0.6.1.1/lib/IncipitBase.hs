{-# options_haddock prune #-}

-- |This module is intended to be used by the packages [incipit-core](https://hackage.haskell.org/package/incipit-core)
-- and [incipit](https://hackage.haskell.org/package/incipit), which are Preludes for packages using
-- [Polysemy](https://hackage.haskell.org/package/polysemy).
--
-- Aside from a few additional names from @base@, this module also exports the string system from
-- [relude](https://hackage.haskell.org/package/relude), some tracing functions, and a small set of other combinators.
module IncipitBase (
  module Incipit.Base,
  module Incipit.Debug,
  module Incipit.Either,
  module Incipit.Libraries,
  module Incipit.List,
  module Incipit.Misc,
  module Incipit.String,
  module Incipit.TypeError,
) where

import Incipit.Base
import Incipit.Debug
import Incipit.Either
import Incipit.Libraries
import Incipit.List
import Incipit.Misc
import Incipit.String
import Incipit.TypeError
