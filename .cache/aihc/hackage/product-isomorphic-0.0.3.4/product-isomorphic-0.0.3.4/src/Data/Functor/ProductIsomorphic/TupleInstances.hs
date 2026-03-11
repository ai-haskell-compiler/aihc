{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.TupleInstances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines instances of tuple types.
module Data.Functor.ProductIsomorphic.TupleInstances () where

import Control.Applicative ((<$>))

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor (..))
import Data.Functor.ProductIsomorphic.TH.Internal
  (defineTupleProductConstructor)


instance ProductConstructor () where
  productConstructor = ()

$(concat <$> mapM defineTupleProductConstructor [2..7])
