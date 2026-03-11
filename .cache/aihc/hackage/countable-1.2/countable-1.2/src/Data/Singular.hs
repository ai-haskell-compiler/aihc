module Data.Singular where

import Data.Countable
import Data.Searchable

-- | There is exactly one value.
class (Finite a, AtLeastOneCountable a) => Singular a where
    single :: a

instance Singular () where
    single = ()

instance (Singular a, Singular b) => Singular (a, b) where
    single = (single, single)
