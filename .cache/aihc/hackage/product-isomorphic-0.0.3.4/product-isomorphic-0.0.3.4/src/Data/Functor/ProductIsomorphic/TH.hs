-- |
-- Module      : Data.Functor.ProductIsomorphic.TH
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module exports templates to make product constructors.
module Data.Functor.ProductIsomorphic.TH (
  -- * Template of ProductConstructor
  defineProductConstructor, defineTupleProductConstructor,

  -- * Low-level API to get record info
  reifyRecordType,
  ) where

import Data.Functor.ProductIsomorphic.TH.Internal
  (defineProductConstructor, defineTupleProductConstructor, reifyRecordType, )
