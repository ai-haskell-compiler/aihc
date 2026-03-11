-- |
-- Module      : Data.Functor.ProductIsomorphic.Unsafe
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines unsafe class interfaces.
module Data.Functor.ProductIsomorphic.Unsafe (
  ProductConstructor (..),
  ) where

-- | Define product isomorphic inference rule
--   to specify record constructor
class ProductConstructor c where
  productConstructor :: c
