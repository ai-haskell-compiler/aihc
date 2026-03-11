{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.GenericInstances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines instances for constructors used in generic-programming.
module Data.Functor.ProductIsomorphic.GenericInstances () where

import GHC.Generics
  (U1 (U1), K1 (K1), M1 (M1), (:*:) ((:*:)), )
-- import GHC.Generics (Generic, Rep, from, to, )

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor (..))


instance ProductConstructor (U1 p) where
  productConstructor = U1
  {-# INLINEABLE productConstructor #-}

instance ProductConstructor (c -> K1 i c p) where
  productConstructor = K1
  {-# INLINEABLE productConstructor #-}

instance ProductConstructor (f p -> M1 i c f p) where
  productConstructor = M1
  {-# INLINEABLE productConstructor #-}

instance ProductConstructor (f x -> g x -> (f :*: g) x) where
  productConstructor = (:*:)
  {-# INLINEABLE productConstructor #-}

{- -- why compile error?
instance Generic a => ProductConstructor (a -> Rep a x) where
  productConstructor = from

instance Generic a => ProductConstructor (Rep a x -> a) where
  productConstructor = to
 -}
