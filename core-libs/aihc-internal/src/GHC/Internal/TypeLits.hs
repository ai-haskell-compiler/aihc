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
  )
where

import GHC.TypeLits (KnownNat, KnownSymbol, Nat, SSymbol, Symbol, fromSSymbol, natVal, natVal', symbolVal, symbolVal')
