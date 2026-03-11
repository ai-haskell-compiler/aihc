{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.Haskell.TH.Compat.Data
-- Copyright   : 2016-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility definitions of
-- data-type declaration templates for before temaplate-haskell-2.11
module Language.Haskell.TH.Compat.Data (
  -- * Interfaces to construct data declarations
  dataD', newtypeD', dataInstD', newtypeInstD', plainTVspecified,

  -- * Interfaces to destruct data declarations
  unDataD, unNewtypeD, unDataInstD, unNewtypeInstD, unInstanceD, unTyVarBndr,
  ) where

#if MIN_VERSION_template_haskell(2,21,0)
import Language.Haskell.TH.Compat.Data.Current
#elif MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Compat.Data.V220
#elif MIN_VERSION_template_haskell(2,15,0)
import Language.Haskell.TH.Compat.Data.V216
#elif MIN_VERSION_template_haskell(2,12,0)
import Language.Haskell.TH.Compat.Data.V214
#elif MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Compat.Data.V211
#else
import Language.Haskell.TH.Compat.Data.V210
#endif

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Compat.TyVarBndr.Current
#else
import Language.Haskell.TH.Compat.TyVarBndr.V216
#endif
