{-# LANGUAGE CPP
           , FlexibleInstances
           , UndecidableInstances
           #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.Number.PartialOrd
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  semi-portable (OverlappingInstances,...)
--
-- The Prelude's 'Ord' class for dealing with ordered types is often
-- onerous to use because it requires 'Eq' as well as a total
-- ordering. While such total orderings are common, partial orderings
-- are more so. This module presents a class for partially ordered
-- types.
----------------------------------------------------------------
module Data.Number.PartialOrd
    (
    -- * Partial Ordering
      PartialOrd(..)
    -- * Functions
    , comparingPO
    ) where

-- Bugfix for Hugs (September 2006), see note below.
import Prelude hiding (isNaN)
import Hugs.RealFloat (isNaN)

----------------------------------------------------------------
-- | This class defines a partially ordered type. The method names
-- were chosen so as not to conflict with 'Ord' and 'Eq'. We use
-- 'Maybe' instead of defining new types @PartialOrdering@ and
-- @FuzzyBool@ because this way should make the class easier to
-- use.
--
-- Minimum complete definition: 'cmp'

class PartialOrd a where
    -- | like 'compare'
    cmp   :: a -> a -> Maybe Ordering

    -- | like ('>')
    gt    :: a -> a -> Maybe Bool
    gt x y = case x `cmp` y of
             Just GT -> Just True
             Just _  -> Just False
             Nothing -> Nothing

    -- | like ('>=')
    ge    :: a -> a -> Maybe Bool
    ge x y = case x `cmp` y of
             Just LT -> Just False
             Just _  -> Just True
             Nothing -> Nothing

    -- | like ('==')
    eq    :: a -> a -> Maybe Bool
    eq x y = case x `cmp` y of
             Just EQ -> Just True
             Just _  -> Just False
             Nothing -> Nothing

    -- | like ('/=')
    ne    :: a -> a -> Maybe Bool
    ne x y = case x `cmp` y of
             Just EQ -> Just False
             Just _  -> Just True
             Nothing -> Nothing

    -- | like ('<=')
    le    :: a -> a -> Maybe Bool
    le x y = case x `cmp` y of
             Just GT -> Just False
             Just _  -> Just True
             Nothing -> Nothing

    -- | like ('<')
    lt    :: a -> a -> Maybe Bool
    lt x y = case x `cmp` y of
             Just LT -> Just True
             Just _  -> Just False
             Nothing -> Nothing

    -- | like 'max'. The default instance returns the left argument
    -- when they're equal.
    maxPO    :: a -> a -> Maybe a
    maxPO x y = do o <- x `cmp` y
                   case o of
                       GT -> Just x
                       EQ -> Just x
                       LT -> Just y

    -- | like 'min'. The default instance returns the left argument
    -- when they're equal.
    minPO    :: a -> a -> Maybe a
    minPO x y = do o <- x `cmp` y
                   case o of
                       GT -> Just y
                       EQ -> Just x
                       LT -> Just x

infix 4 `gt`, `ge`, `eq`, `ne`, `le`, `lt`, `maxPO`, `minPO`

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    (Ord a) => PartialOrd a where
    cmp   x y = Just $! x `compare` y
    gt    x y = Just $! x >  y
    ge    x y = Just $! x >= y
    eq    x y = Just $! x == y
    ne    x y = Just $! x /= y
    le    x y = Just $! x <= y
    lt    x y = Just $! x <  y
    maxPO x y = Just $! x `max` y
    minPO x y = Just $! x `min` y


-- N.B. Hugs (Sept 2006) has a buggy definition for 'isNaN' which
-- always returns @False@. We use a fixed version, provided the CPP
-- was run with the right arguments. See "Hugs.RealFloat". If 'cmp'
-- returns @Just Eq@ for @notANumber@ then CPP was run wrongly.
--
-- The instances inherited from Ord are wrong. So we'll fix them.
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PartialOrd Float where
    cmp x y | isNaN x || isNaN y = Nothing
            | otherwise          = Just $! x `compare` y

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PartialOrd Double where
    cmp x y | isNaN x || isNaN y = Nothing
            | otherwise          = Just $! x `compare` y

----------------------------------------------------------------
-- TODO? add maximumPO\/minimumPO via left or right fold?

-- BUG: Haddock doesn't link the `comparing`
--
-- | Like @Data.Ord.comparing@. Helpful in conjunction with the
-- @xxxBy@ family of functions from "Data.List"
comparingPO :: (PartialOrd b) => (a -> b) -> a -> a -> Maybe Ordering
comparingPO p x y = p x `cmp` p y

----------------------------------------------------------------
----------------------------------------------------------- fin.
