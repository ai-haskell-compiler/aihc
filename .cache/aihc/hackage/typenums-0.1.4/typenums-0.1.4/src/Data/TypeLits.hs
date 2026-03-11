{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Safe               #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

{-|
Module: Data.TypeLits
Copyright: (c) 2018-2021 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

This module provides the same interface as "GHC.TypeLits", but with
naming conflicts resolved in favour of this package.  For example,
'(<=)' resolves to the kind-polymorphic version from "Data.TypeNums".

If you are only working with type-level numbers, import "Data.TypeNums"
instead.  This module is purely for convenience for those who want to
use both functionality from "GHC.TypeLits" and functionality from
"Data.TypeNums".
-}

module Data.TypeLits
  (
    -- * Type level numbers
    -- ** Naturals
    G.Nat
  , G.KnownNat
  , G.natVal
  , G.natVal'
  , G.SomeNat(..)
  , G.someNatVal
  , G.sameNat

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
    -- ** Comparison
  , type (==?)
  , type (/=?)
  , type (<=?)
  , type (==)
  , type (/=)
  , type (<=)
  , type (<)
  , type (>=)
  , type (>)

    -- ** Arithmetic
    -- *** Unary operations
  , Abs
  , Negate
  , Recip
  , Floor
  , Ceiling
  , Truncate

    -- *** Binary operations
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

    -- * Symbols
  , G.Symbol
#if MIN_VERSION_base(4,10,0)
  , G.AppendSymbol
#endif
  , G.CmpSymbol
  , G.KnownSymbol
  , G.symbolVal
  , G.symbolVal'
  , G.SomeSymbol(..)
  , G.someSymbolVal
  , G.sameSymbol

    -- * User-defined type errors
  , G.TypeError
  , G.ErrorMessage(..)
  ) where

import Data.TypeNums
import qualified GHC.TypeLits as G
