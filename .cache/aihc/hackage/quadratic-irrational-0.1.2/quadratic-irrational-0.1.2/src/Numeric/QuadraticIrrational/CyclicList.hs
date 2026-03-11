-- |
-- Module      : Numeric.QuadraticIrrational.CyclicList
-- Description : A container for a possibly cyclic list.
-- Copyright   : © 2014 Johan Kiviniemi
-- License     : MIT
-- Maintainer  : Johan Kiviniemi <devel@johan.kiviniemi.name>
-- Stability   : provisional
-- Portability : portable

{-# LANGUAGE DeriveFunctor     #-}

module Numeric.QuadraticIrrational.CyclicList
  ( CycList(..)
  ) where

import Data.Foldable (toList)
import Data.Monoid ((<>))

-- | A container for a possibly cyclic list.
--
-- $setup
-- >>> import Data.Foldable (toList)
--
-- >>> toList (CycList "hello" "")
-- "hello"
--
-- >>> take 70 (toList (CycList "prefix " "cycle"))
-- "prefix cyclecyclecyclecyclecyclecyclecyclecyclecyclecyclecyclecyclecyc"
data CycList a = CycList [a] [a]
  deriving (Eq, Ord, Read, Show, Functor)

instance Foldable CycList where
  toList (CycList as []) = as
  toList (CycList as bs) = as <> cycle bs

  foldMap f (CycList as []) = foldMap f as
  foldMap f (CycList as bs) = foldMap f as <> cycleAppend (foldMap f bs)
    where
      cycleAppend x = x <> cycleAppend x
