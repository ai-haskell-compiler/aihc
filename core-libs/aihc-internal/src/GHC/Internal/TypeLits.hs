{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- | GHC declares the type-level literals here and re-exports them from
-- @GHC.TypeLits@. @aihc-internal@ depends on @aihc-base@ rather than the
-- other way round, so the declarations live in @GHC.TypeLits@ and this
-- module re-exports them.
module GHC.Internal.TypeLits
  ( Nat,
    Symbol,
    KnownNat,
    natVal,
    natVal',
    KnownSymbol,
    symbolVal,
    symbolVal',
    SSymbol,
    fromSSymbol,
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

import GHC.TypeLits (CmpNat, Div, ErrorMessage (..), KnownNat, KnownSymbol, Log2, Mod, Nat, SSymbol, Symbol, TypeError, fromSSymbol, natVal, natVal', symbolVal, symbolVal', type (*), type (+), type (-), type (<=), type (<=?), type (^))
