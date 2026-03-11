{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe               #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif
{-|
Module: Data.TypeNums
Copyright: (c) 2018-2021 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

This module provides a unified interface for natural numbers,
signed integers, and rationals at the type level, in a way fully
compatible with existing code using type-level naturals.

Natural numbers are expressed as always, e.g. @5@.  Negative integers
are written as @Neg 3@.  Ratios are written as @3 :% 2@.

There are some naming conflicts between this module and "GHC.TypeLits",
notably the comparison and arithmetic operators.  This module reexports
'Nat', 'KnownNat', 'natVal' and 'natVal'' so you may import just this
module and not "GHC.TypeLits".

If you wish to use other functionality from "GHC.TypeLits", this package
also provides the module "Data.TypeLits" that includes (almost) full
functionality from "GHC.TypeLits", but with the conflicts resolving in
this packages favour.
-}
module Data.TypeNums
  ( -- * Type level numbers
    -- ** Naturals
    G.Nat
  , G.KnownNat
  , G.natVal
  , G.natVal'
  , G.SomeNat(..)
  , G.someNatVal

    -- ** Integers
  , TInt(..)
  , KnownInt
  , intVal
  , intVal'
  , SomeInt(..)
  , someIntVal

    -- ** Rationals
  , Rat((:%))
  , KnownRat
  , ratVal
  , ratVal'
  , SomeRat(..)
  , someRatVal
  , Simplify

    -- * Type level numerical operations
    -- ** Comparisons
  , type (==?)
  , type (/=?)
  , type (==)
  , type (/=)
  , type (<=?)
  , type (<=)
  , type (<)
  , type (>=)
  , type (>)

    -- ** Arithmetic
    -- *** Unary Operations
  , Abs
  , Negate
  , Recip
  , Floor
  , Ceiling
  , Truncate

    -- *** Binary Operations
  , type (+)
  , type (-)
  , type (*)
  , type (/)
  , type (^)
  , DivMod
  , QuotRem
  , Div
  , Mod
  , Quot
  , Rem
  , GCD
  , IntLog
  , Log2
  ) where

import Data.TypeNums.Arithmetic
import Data.TypeNums.Comparison
import Data.TypeNums.Equality
import Data.TypeNums.Ints
import Data.TypeNums.Rats

import qualified GHC.TypeLits as G
