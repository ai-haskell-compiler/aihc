{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Registry.Internal.MultiMap () where

import Data.MultiMap (MultiMap)
import Data.MultiMap qualified as MM
import Protolude as P hiding (show)
import Prelude (show)

instance (Show k, Show v) => Show (MultiMap k v) where
  show = show . MM.assocs

instance (Ord k) => Semigroup (MultiMap k v) where
  (<>) m1 m2 = MM.fromList (MM.toList m1 <> MM.toList m2)

instance (Ord k) => Monoid (MultiMap k v) where
  mempty = MM.empty
  mappend = (<>)
