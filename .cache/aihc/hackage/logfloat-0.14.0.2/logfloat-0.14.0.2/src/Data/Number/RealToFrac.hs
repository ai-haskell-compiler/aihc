-- Needed to ensure correctness, and because we can't guarantee rules fire
-- The MagicHash is for unboxed primitives (-fglasgow-exts also works)
--     We only need MagicHash if on GHC, but we can't hide it in an #ifdef
{-# LANGUAGE CPP
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- We don't put these in LANGUAGE, because it's CPP guarded for GHC only
{-# OPTIONS_GHC -XMagicHash #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Data.Number.RealToFrac
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  semi-portable (CPP, MPTC, OverlappingInstances)
--
-- This module presents a type class for generic conversion between
-- numeric types, generalizing @realToFrac@ in order to overcome
-- problems with pivoting through 'Rational'
----------------------------------------------------------------
module Data.Number.RealToFrac (RealToFrac(..)) where

import Prelude hiding    (realToFrac, isInfinite, isNaN)
import qualified Prelude (realToFrac)

import Data.Number.Transfinite

#ifdef __GLASGOW_HASKELL__
import GHC.Exts
    ( Int(..), Float(..), Double(..)
    , int2Double#
    , int2Float#
    , double2Float#
    , float2Double#
    )
#endif

----------------------------------------------------------------
-- | The 'Prelude.realToFrac' function is defined to pivot through
-- a 'Rational' according to the haskell98 spec. This is non-portable
-- and problematic as discussed in "Data.Number.Transfinite". Since
-- there is resistance to breaking from the spec, this class defines
-- a reasonable variant which deals with transfinite values
-- appropriately.
--
-- There is a generic instance from any Transfinite Real to any
-- Transfinite Fractional, using checks to ensure correctness. GHC
-- has specialized versions for some types which use primitive
-- converters instead, for large performance gains. (These definitions
-- are hidden from other compilers via CPP.) Due to a bug in Haddock
-- the specialized instances are shown twice and the generic instance
-- isn't shown at all. Since the instances are overlapped, you'll
-- need to give type signatures if the arguments to 'realToFrac'
-- are polymorphic. There's also a generic instance for any Real
-- Fractional type to itself, thus if you write any generic instances
-- beware of incoherence.
--
-- If any of these restrictions (CPP, GHC-only optimizations,
-- OverlappingInstances) are onerous to you, contact the maintainer
-- (we like patches).  Note that this /does/ work for Hugs with
-- suitable options (e.g. @hugs -98 +o -F'cpp -P'@). However, Hugs
-- doesn't allow @IncoherentInstances@ nor does it allow diamonds
-- with @OverlappingInstances@, which restricts the ability to add
-- additional generic instances.

class (Real a, Fractional b) => RealToFrac a b where
    realToFrac :: a -> b

instance (Real a, Fractional a) => RealToFrac a a where
    realToFrac = id

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    (Real a, Transfinite a, Fractional b, Transfinite b)
    => RealToFrac a b
    where
    realToFrac x
        | isNaN      x = notANumber
        | isInfinite x = if x > 0 then infinity
                                  else negativeInfinity
        | otherwise    = Prelude.realToFrac x


#ifdef __GLASGOW_HASKELL__
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Int Float where
    {-# INLINE realToFrac #-}
    realToFrac (I# i) = F# (int2Float# i)

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Int Double where
    {-# INLINE realToFrac #-}
    realToFrac (I# i) = D# (int2Double# i)


instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Integer Float where
    -- TODO: is there a more primitive way?
    {-# INLINE realToFrac #-}
    realToFrac j = Prelude.realToFrac j

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Integer Double where
    -- TODO: is there a more primitive way?
    {-# INLINE realToFrac #-}
    realToFrac j = Prelude.realToFrac j


instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Float Double where
    {-# INLINE realToFrac #-}
    realToFrac (F# f) = D# (float2Double# f)

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    RealToFrac Double Float where
    {-# INLINE realToFrac #-}
    realToFrac (D# d) = F# (double2Float# d)
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
