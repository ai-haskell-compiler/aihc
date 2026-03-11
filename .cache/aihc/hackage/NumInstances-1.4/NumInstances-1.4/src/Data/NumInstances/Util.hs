{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NumInstances.Util
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Utilities for number class instances for functions and tuples
----------------------------------------------------------------------

module Data.NumInstances.Util (noOv) where

noOv :: String -> String -> a
noOv ty meth = error $ meth ++ ": No overloading for " ++ ty
