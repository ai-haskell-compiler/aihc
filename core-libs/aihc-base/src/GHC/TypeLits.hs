{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | Type-level literals.
--
-- GHC declares these in @GHC.Internal.TypeLits@ and re-exports them here.
-- @aihc-internal@ depends on @aihc-base@ rather than the other way round,
-- so the declarations live here and the internal module re-exports them.
--
-- This module's @natVal@ returns an 'Integer', as GHC's does; the one in
-- "GHC.TypeNats" returns a 'Natural'.
module GHC.TypeLits
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

import GHC.Num.Integer (Integer)
import GHC.Prim (Proxy#)
import GHC.Real (toInteger)
import GHC.TypeNats (KnownNat, Nat)
import GHC.TypeNats qualified as Nats
import GHC.Types (Char, Constraint, Symbol, Type)

-- | The value of a known type-level natural, as an 'Integer'.
natVal :: forall n proxy. (KnownNat n) => proxy n -> Integer
natVal proxy = toInteger (Nats.natVal proxy)

-- | The value of a known type-level natural, through an unlifted proxy.
natVal' :: forall n. (KnownNat n) => Proxy# n -> Integer
natVal' proxy = toInteger (Nats.natVal' proxy)

-- | A type-level symbol whose value is known.
--
-- GHC gives the method the type @SSymbol s@. Here it is the value itself,
-- for the same reason as 'GHC.TypeNats.natSing'.
type KnownSymbol :: Symbol -> Constraint
class KnownSymbol s where
  symbolSing :: [Char]

-- | The value of a known type-level symbol.
symbolVal :: forall s proxy. (KnownSymbol s) => proxy s -> [Char]
symbolVal _ = symbolSing @s

-- | The value of a known type-level symbol, through an unlifted proxy.
symbolVal' :: forall s. (KnownSymbol s) => Proxy# s -> [Char]
symbolVal' _ = symbolSing @s

-- | A singleton for a known type-level symbol.
type SSymbol :: Symbol -> Type
newtype SSymbol s = UnsafeSSymbol [Char]

-- | The value a singleton stands for.
fromSSymbol :: SSymbol s -> [Char]
fromSSymbol (UnsafeSSymbol value) = value
