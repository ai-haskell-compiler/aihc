-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Special
-- Copyright   :  Daniel Mendler (c) 2016,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- The standard 'Arbitrary' instances don't generate special values.
-- This is fixed by this package which provides the newtype 'Special' with an 'Arbitrary' instance.
-- The special values are given by the 'SpecialValues' typeclass.
-----------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.QuickCheck.Special (
  Special(..),
  SpecialValues(..),
) where

import Data.SpecialValues (SpecialValues(..))
import Test.QuickCheck

-- | Additionally to the standard Arbitrary instances,
-- this generates special values with a small probability.
newtype Special a = Special { getSpecial :: a }
  deriving (Show, Read, Functor, Bounded, Enum, Eq, Ord, Num, Real, Integral)

instance (Arbitrary a, SpecialValues a) => Arbitrary (Special a) where
  shrink = fmap Special . shrink . getSpecial
  arbitrary = fmap Special $ frequency $ list specialValues
    where list s = (10 * length s, arbitrary) : fmap (\t -> (1, return t)) s

instance CoArbitrary a => CoArbitrary (Special a) where
  coarbitrary = coarbitrary . getSpecial
