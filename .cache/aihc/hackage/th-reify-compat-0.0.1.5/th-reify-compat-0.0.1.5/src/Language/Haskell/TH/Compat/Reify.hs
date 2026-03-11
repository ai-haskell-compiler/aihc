{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.Haskell.TH.Compat.Reify
-- Copyright   : 2016 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility definitions of
-- destructuring result type of reify for before temaplate-haskell-2.11
module Language.Haskell.TH.Compat.Reify (
  -- * Interfaces to destruct reify result
  unClassOpI, unDataConI, unVarI,

  -- * Interface to get operator fixity
  reifyFixity,

  -- * Interface of type alias name
  ParentName,
  ) where

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Compat.Reify.Current
#else
import Language.Haskell.TH.Compat.Reify.V210
#endif
