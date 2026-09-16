{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | Type-level natural numbers.
--
-- GHC declares these in @GHC.Internal.TypeNats@ and re-exports them here.
-- @aihc-internal@ depends on @aihc-base@ rather than the other way round,
-- so the declarations live here and the internal module re-exports them.
--
-- @someNatVal@ and its @SomeNat@ are not here yet. GHC writes them by
-- coercing a constrained value to a function of its dictionary, which
-- relies on a single-method dictionary being represented as its method;
-- an aihc dictionary is a constructor around its fields instead, so the
-- coercion would be wrong. See @docs/type-level-naturals.md@.
module GHC.TypeNats
  ( Natural,
    Nat,
    KnownNat,
    natVal,
    natVal',
    SNat,
    fromSNat,
  )
where

import GHC.Num.Natural (Natural)
import GHC.Prim (Proxy#)
import GHC.Types (Constraint, Type)

-- | The kind of type-level natural literals. GHC makes this a synonym for
-- the value type, so that @natVal@ can return one.
type Nat = Natural

-- | A type-level natural whose value is known.
--
-- GHC gives the method the type @SNat n@. Here it is the value itself: the
-- compiler builds this dictionary directly, and a singleton wrapper around
-- the value would cost it a coercion for nothing.
type KnownNat :: Nat -> Constraint
class KnownNat n where
  natSing :: Natural

-- | The value of a known type-level natural.
natVal :: forall n proxy. (KnownNat n) => proxy n -> Natural
natVal _ = natSing @n

-- | The value of a known type-level natural, through an unlifted proxy.
natVal' :: forall n. (KnownNat n) => Proxy# n -> Natural
natVal' _ = natSing @n

-- | A singleton for a known type-level natural.
type SNat :: Nat -> Type
newtype SNat n = UnsafeSNat Natural

-- | The value a singleton stands for.
fromSNat :: SNat n -> Natural
fromSNat (UnsafeSNat value) = value
