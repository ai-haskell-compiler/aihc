{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Arithmetic.Plus
  ( zeroL
  , zeroR
  , commutative
  , commutative#
  , associative
  ) where

import Arithmetic.Unsafe (type (:=:) (Eq))
import Arithmetic.Unsafe (type (:=:#) (Eq#))
import GHC.TypeNats (type (+))

-- | Any number plus zero is equal to the original number.
zeroR :: m :=: (m + 0)
zeroR = Eq

-- | Zero plus any number is equal to the original number.
zeroL :: m :=: (0 + m)
zeroL = Eq

-- | Addition is commutative.
commutative :: forall a b. a + b :=: b + a
commutative = Eq

commutative# :: forall a b. (# #) -> a + b :=:# b + a
commutative# _ = Eq# (# #)

-- | Addition is associative.
associative :: forall a b c. (a + b) + c :=: a + (b + c)
associative = Eq
