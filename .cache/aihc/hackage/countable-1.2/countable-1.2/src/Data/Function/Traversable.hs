{-# OPTIONS -fno-warn-orphans #-}

module Data.Function.Traversable
    (
    ) where

import Data.Function.Foldable ()
import Data.Searchable

instance Finite a => Traversable ((->) a) where
    sequenceA = assemble
