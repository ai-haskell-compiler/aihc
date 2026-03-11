{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.Haskell.TH.Compat.Constraint
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility definitions of
-- constraint context template for before temaplate-haskell-2.9
module Language.Haskell.TH.Compat.Constraint (
  classP,
  ) where

#if MIN_VERSION_template_haskell(2,10,0)
import Language.Haskell.TH.Compat.Constraint.Current
#else
import Language.Haskell.TH.Compat.Constraint.V209
#endif
