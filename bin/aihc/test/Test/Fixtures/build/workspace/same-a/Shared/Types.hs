-- | A module with the name of a module of @same-b@, so that a package that
-- depends on both sees two modules of one name.
module Shared.Types (Wrapped (..)) where

newtype Wrapped = Wrapped Int
