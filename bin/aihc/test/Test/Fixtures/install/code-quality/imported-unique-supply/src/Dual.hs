{-# LANGUAGE PolyKinds #-}

module Dual (Dual (..)) where

import Control.Category
import Prelude ()
import Semigroupoid

newtype Dual k a b = Dual {getDual :: k b a}

instance Semigroupoid k => Semigroupoid (Dual k) where
  Dual f `o` Dual g = Dual (g `o` f)

instance Category k => Category (Dual k) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)
