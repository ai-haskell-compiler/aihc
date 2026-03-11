{-# LANGUAGE CPP #-}

-- |
-- Module      : Test.QuickCheck.CompatIO
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides a compatible ioProperty definition.
module Test.QuickCheck.CompatIO (
  ioProperty,
  ) where

#if MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck (ioProperty)
#else
import Test.QuickCheck.Property (Testable, Property, morallyDubiousIOProperty)

ioProperty :: Testable prop => IO prop -> Property
ioProperty = morallyDubiousIOProperty
#endif
