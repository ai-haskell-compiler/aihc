{-# OPTIONS -fno-warn-orphans #-}

module Data.Function.Foldable
    (
    ) where

import Data.Foldable hiding (find)
import Data.Monoid
import Data.Searchable
import Prelude

instance Finite t => Foldable ((->) t) where
    foldMap am ta = mconcat (fmap (am . ta) allValues)
