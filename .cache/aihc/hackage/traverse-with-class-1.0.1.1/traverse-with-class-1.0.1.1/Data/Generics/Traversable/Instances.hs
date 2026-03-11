{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module defines 'GTraversable' instances for standard types
-- exported by "Prelude"
module Data.Generics.Traversable.Instances () where

import Data.Generics.Traversable.Core
import Data.Generics.Traversable.TH
import Data.Ratio

instance GTraversable c ()
instance GTraversable c Bool
instance GTraversable c Int
instance GTraversable c Integer
instance GTraversable c Float
instance GTraversable c Double
instance GTraversable c (Ratio n)
instance GTraversable c Char
instance GTraversable c Ordering

deriveGTraversable ''Maybe
deriveGTraversable ''Either
deriveGTraversable ''(,)
deriveGTraversable ''(,,)

-- Uniform instance for lists
instance c a => GTraversable c [a] where
  gtraverse f = go where
    go [] = pure []
    go (x:xs) = (:) <$> f x <*> go xs

