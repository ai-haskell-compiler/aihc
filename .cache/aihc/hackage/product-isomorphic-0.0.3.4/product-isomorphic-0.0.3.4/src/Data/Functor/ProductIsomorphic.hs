-- |
-- Module      : Data.Functor.ProductIsomorphic
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This is the integrated interface module for product restricted functors.
module Data.Functor.ProductIsomorphic (
  module Data.Functor.ProductIsomorphic.Unsafe,
  module Data.Functor.ProductIsomorphic.Class,
  module Data.Functor.ProductIsomorphic.Instances,
  ) where

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor)
import Data.Functor.ProductIsomorphic.Class
import Data.Functor.ProductIsomorphic.Instances
import Data.Functor.ProductIsomorphic.TupleInstances ()
import Data.Functor.ProductIsomorphic.GenericInstances ()
