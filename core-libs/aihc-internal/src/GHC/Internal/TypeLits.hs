{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}

-- | GHC declares the type-level literals here and re-exports them from
-- @GHC.TypeLits@. @aihc-internal@ depends on @aihc-base@ rather than the
-- other way round, so the declarations live in @GHC.TypeLits@ and this
-- module re-exports them.
module GHC.Internal.TypeLits
  ( Nat,
    Symbol,
    KnownNat,
    natSing,
    natVal,
    natVal',
    SNat,
    fromSNat,
    withSomeSNat,
    withKnownNat,
    KnownSymbol,
    symbolSing,
    symbolVal,
    symbolVal',
    SSymbol,
    fromSSymbol,
    withSomeSSymbol,
    withKnownSymbol,
    KnownChar,
    charSing,
    charVal,
    charVal',
    SChar,
    fromSChar,
    TypeError,
    ErrorMessage (..),
    type (<=),
    type (<=?),
    CmpNat,
    type (+),
    type (-),
    type (*),
    type (^),
    Div,
    Mod,
    Log2,
  )
where

import GHC.TypeLits (CmpNat, Div, ErrorMessage (..), KnownChar, KnownNat, KnownSymbol, Log2, Mod, Nat, SChar, SNat, SSymbol, Symbol, TypeError, charSing, charVal, charVal', fromSChar, fromSNat, fromSSymbol, natSing, natVal, natVal', symbolSing, symbolVal, symbolVal', withKnownNat, withKnownSymbol, withSomeSNat, withSomeSSymbol, type (*), type (+), type (-), type (<=), type (<=?), type (^))
