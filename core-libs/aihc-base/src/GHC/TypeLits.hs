{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

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
    natSing,
    natVal,
    natVal',
    SNat,
    fromSNat,
    withKnownNat,
    KnownSymbol,
    symbolSing,
    symbolVal,
    symbolVal',
    SSymbol,
    fromSSymbol,
    withKnownSymbol,
    KnownChar,
    charSing,
    charVal,
    charVal',
    SChar,
    fromSChar,
    withKnownChar,
    AppendSymbol,
    CharToNat,
    NatToChar,
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

import Data.Type.Ord (type (<=), type (<=?))
import GHC.Num.Integer (Integer)
import GHC.Prim (Proxy#)
import GHC.Real (toInteger)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeNats (CmpNat, Div, KnownNat, Log2, Mod, Nat, SNat, fromSNat, natSing, withKnownNat, type (*), type (+), type (-), type (^))
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
-- GHC gives the method the name @symbolSing@ and the type @SSymbol s@.
-- Here the method is the value itself, for the same reason as in
-- 'KnownNat'. The function 'symbolSing' gives the singleton.
type KnownSymbol :: Symbol -> Constraint
class KnownSymbol s where
  knownSymbolValue :: [Char]

-- | The singleton for a known type-level symbol.
symbolSing :: forall s. (KnownSymbol s) => SSymbol s
symbolSing = UnsafeSSymbol (knownSymbolValue @s)

-- | The value of a known type-level symbol.
symbolVal :: forall s proxy. (KnownSymbol s) => proxy s -> [Char]
symbolVal _ = knownSymbolValue @s

-- | The value of a known type-level symbol, through an unlifted proxy.
symbolVal' :: forall s. (KnownSymbol s) => Proxy# s -> [Char]
symbolVal' _ = knownSymbolValue @s

-- | A singleton for a known type-level symbol.
type SSymbol :: Symbol -> Type
newtype SSymbol s = UnsafeSSymbol [Char]

-- | The value a singleton stands for.
fromSSymbol :: SSymbol s -> [Char]
fromSSymbol (UnsafeSSymbol value) = value

-- | A type-level character whose value is known.
--
-- GHC gives the method the name @charSing@ and the type @SChar c@. Here
-- the method is the value itself, for the same reason as in 'KnownNat'.
-- The function 'charSing' gives the singleton.
type KnownChar :: Char -> Constraint
class KnownChar c where
  knownCharValue :: Char

-- | The singleton for a known type-level character.
charSing :: forall c. (KnownChar c) => SChar c
charSing = UnsafeSChar (knownCharValue @c)

-- | The value of a known type-level character.
charVal :: forall c proxy. (KnownChar c) => proxy c -> Char
charVal _ = knownCharValue @c

-- | The value of a known type-level character, through an unlifted proxy.
charVal' :: forall c. (KnownChar c) => Proxy# c -> Char
charVal' _ = knownCharValue @c

-- | A singleton for a known type-level character.
type SChar :: Char -> Type
newtype SChar c = UnsafeSChar Char

-- | The value a singleton stands for.
fromSChar :: SChar c -> Char
fromSChar (UnsafeSChar value) = value

-- | Supply class evidence from a singleton.
withKnownSymbol :: forall s r. SSymbol s -> ((KnownSymbol s) => r) -> r
withKnownSymbol (UnsafeSSymbol value) = withKnownSymbolValue# @s value

foreign import prim "withKnownSymbolValue#" withKnownSymbolValue# :: forall s r. [Char] -> ((KnownSymbol s) => r) -> r

-- | Supply class evidence from a singleton.
withKnownChar :: forall c r. SChar c -> ((KnownChar c) => r) -> r
withKnownChar (UnsafeSChar value) = withKnownCharValue# @c value

foreign import prim "withKnownCharValue#" withKnownCharValue# :: forall c r. Char -> ((KnownChar c) => r) -> r

type AppendSymbol :: Symbol -> Symbol -> Symbol
type family AppendSymbol a b

type CharToNat :: Char -> Nat
type family CharToNat c

type NatToChar :: Nat -> Char
type family NatToChar n
