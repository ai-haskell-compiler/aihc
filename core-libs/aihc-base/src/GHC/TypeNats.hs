{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- | Type-level natural numbers.
--
-- GHC declares these in @GHC.Internal.TypeNats@ and re-exports them here.
-- @aihc-internal@ depends on @aihc-base@ rather than the other way round,
-- so the declarations live here and the internal module re-exports them.
--
-- @someNatVal@ and @SomeNat@ are not available yet.
-- @withKnownNat@ uses @WithDict@ to construct the dictionary.
-- An aihc dictionary contains a constructor around its fields.
module GHC.TypeNats
  ( Natural,
    Nat,
    KnownNat,
    natSing,
    natVal,
    natVal',
    SNat,
    fromSNat,
    withSomeSNat,
    withKnownNat,
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

import GHC.Magic.Dict (withDict)
import GHC.Num.Natural (Natural)
import GHC.Prim (Proxy#)
import GHC.Types (Any, Constraint, Ordering, Type)

-- | The kind of type-level natural literals. GHC makes this a synonym for
-- the value type, so that @natVal@ can return one.
type Nat = Natural

-- | A type-level natural whose value is known.
--
-- GHC gives the method the name @natSing@ and the type @SNat n@. Here the
-- method is the value itself: the compiler builds this dictionary
-- directly, and a singleton wrapper around the value would cost it a
-- coercion for nothing. The function 'natSing' gives the singleton.
type KnownNat :: Nat -> Constraint
class KnownNat n where
  knownNatValue :: Natural

-- | The singleton for a known type-level natural.
natSing :: forall n. (KnownNat n) => SNat n
natSing = UnsafeSNat (knownNatValue @n)

-- | The value of a known type-level natural.
natVal :: forall n proxy. (KnownNat n) => proxy n -> Natural
natVal _ = knownNatValue @n

-- | The value of a known type-level natural, through an unlifted proxy.
natVal' :: forall n. (KnownNat n) => Proxy# n -> Natural
natVal' _ = knownNatValue @n

-- | Comparison of two type-level naturals. The solver computes it.
type CmpNat :: Nat -> Nat -> Ordering
type family CmpNat a b

-- The arithmetic families, which the solver computes when both arguments
-- are literals. Subtraction is partial, as it is in GHC: an application
-- that would go below zero stays stuck.
type family (+) (a :: Nat) (b :: Nat) :: Nat

type family (-) (a :: Nat) (b :: Nat) :: Nat

type family (*) (a :: Nat) (b :: Nat) :: Nat

type family (^) (a :: Nat) (b :: Nat) :: Nat

type Div :: Nat -> Nat -> Nat
type family Div a b

type Mod :: Nat -> Nat -> Nat
type family Mod a b

type Log2 :: Nat -> Nat
type family Log2 a

infixl 6 +, -

infixl 7 *, `Div`, `Mod`

infixr 8 ^

-- | A singleton for a known type-level natural.
type SNat :: Nat -> Type
newtype SNat n = UnsafeSNat Natural

-- | The value a singleton stands for.
fromSNat :: SNat n -> Natural
fromSNat (UnsafeSNat value) = value

-- | Supply a singleton for a runtime natural number.
-- Keep the type index private to each call.
{-# NOINLINE withSomeSNat #-}
withSomeSNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeSNat value continuation = continuation (UnsafeSNat value :: SNat Any)

-- | Supply the dictionary for a singleton natural number.
withKnownNat :: forall n r. SNat n -> ((KnownNat n) => r) -> r
withKnownNat (UnsafeSNat value) = withDict @(KnownNat n) value
