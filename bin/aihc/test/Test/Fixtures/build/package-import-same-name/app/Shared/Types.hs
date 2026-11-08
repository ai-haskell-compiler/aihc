-- | A module with the name of a module of @same-a@. It imports that
-- module with a package qualifier, as @base-compat-batteries@ imports
-- the modules of @base-compat@.
module Shared.Types (Wrapped (..), unwrap) where

import "same-a" Shared.Types (Wrapped (..))

unwrap :: Wrapped -> Int
unwrap (Wrapped n) = n
