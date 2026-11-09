{-# LANGUAGE DataKinds #-}
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
    sameNat,
    SNat,
    fromSNat,
    withSomeSNat,
    withKnownNat,
    KnownSymbol,
    symbolSing,
    symbolVal,
    symbolVal',
    sameSymbol,
    SSymbol,
    fromSSymbol,
    withSomeSSymbol,
    withKnownSymbol,
    KnownChar,
    charSing,
    charVal,
    charVal',
    sameChar,
    SChar,
    fromSChar,
    withKnownChar,
    Natural,
    AppendSymbol,
    CmpChar,
    CmpSymbol,
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

import Data.Type.Equality ((:~:) (..))
import Data.Type.Ord (type (<=), type (<=?))
import GHC.Magic.Dict (withDict)
import GHC.Num.Integer (Integer)
import GHC.Num.Natural (Natural)
import GHC.Prim (Proxy#)
import GHC.Real (toInteger)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeNats (CmpNat, Div, KnownNat, Log2, Mod, Nat, SNat, fromSNat, natSing, sameNat, withKnownNat, withSomeSNat, type (*), type (+), type (-), type (^))
import GHC.TypeNats qualified as Nats
import GHC.Types (Any, Char, Constraint, Ordering, Symbol, Type)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Bool (..), Eq (..), Maybe (..))

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

-- | Prove type equality when the symbols are equal.
sameSymbol :: forall a b proxy1 proxy2. (KnownSymbol a, KnownSymbol b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameSymbol left right =
  case symbolVal left == symbolVal right of
    True -> Just (unsafeCoerce (Refl :: a :~: a))
    False -> Nothing

-- | A singleton for a known type-level symbol.
type SSymbol :: Symbol -> Type
newtype SSymbol s = UnsafeSSymbol [Char]

-- | The value a singleton stands for.
fromSSymbol :: SSymbol s -> [Char]
fromSSymbol (UnsafeSSymbol value) = value

-- | Supply a singleton for a runtime symbol.
-- Keep the type index private to each call.
{-# NOINLINE withSomeSSymbol #-}
withSomeSSymbol :: [Char] -> (forall s. SSymbol s -> r) -> r
withSomeSSymbol value continuation = continuation (UnsafeSSymbol value :: SSymbol Any)

-- | Supply the dictionary for a singleton symbol.
withKnownSymbol :: forall s r. SSymbol s -> ((KnownSymbol s) => r) -> r
withKnownSymbol (UnsafeSSymbol value) = withDict @(KnownSymbol s) value

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

-- | Prove type equality when the characters are equal.
sameChar :: forall a b proxy1 proxy2. (KnownChar a, KnownChar b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
sameChar left right =
  case charVal left == charVal right of
    True -> Just (unsafeCoerce (Refl :: a :~: a))
    False -> Nothing

-- | A singleton for a known type-level character.
type SChar :: Char -> Type
newtype SChar c = UnsafeSChar Char

-- | The value a singleton stands for.
fromSChar :: SChar c -> Char
fromSChar (UnsafeSChar value) = value

-- | Supply the dictionary for a singleton character.
withKnownChar :: forall c r. SChar c -> ((KnownChar c) => r) -> r
withKnownChar (UnsafeSChar value) = withDict @(KnownChar c) value

type AppendSymbol :: Symbol -> Symbol -> Symbol
type family AppendSymbol a b

type CmpSymbol :: Symbol -> Symbol -> Ordering
type family CmpSymbol a b

type CmpChar :: Char -> Char -> Ordering
type family CmpChar a b

type CharToNat :: Char -> Nat
type family CharToNat c

type NatToChar :: Nat -> Char
type family NatToChar n
