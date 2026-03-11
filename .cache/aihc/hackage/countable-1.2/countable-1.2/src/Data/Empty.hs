module Data.Empty where

import Data.Searchable
import Data.Void

-- | There are no values.
class Finite n => Empty n where
    never :: n -> a

instance Empty Void where
    never = absurd

instance (Empty a, Empty b) => Empty (Either a b) where
    never (Left a) = never a
    never (Right a) = never a

instance (Empty a, Finite b) => Empty (a, b) where
    never (a, _) = never a
