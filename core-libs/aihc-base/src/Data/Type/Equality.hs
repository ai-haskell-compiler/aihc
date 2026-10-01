{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Equality
  ( (:~:) (..),
    type (~),
    type (~~),
    (:~~:) (..),
    sym,
    trans,
    castWith,
    gcastWith,
  )
where

import GHC.Types (type (~), type (~~))

infix 4 :~:, :~~:

-- | Propositional equality. A value of type a :~: b proves that a and b are the same type.
data (a :: k) :~: (b :: k) where
  Refl :: forall k (a :: k). a :~: a

-- | Kind-heterogeneous propositional equality.
data (a :: k1) :~~: (b :: k2) where
  HRefl :: forall k (a :: k). a :~~: a

-- | Propositional equality is symmetric.
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

-- | Propositional equality is transitive.
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

-- | Type-safe cast, with propositional equality.
castWith :: (a :~: b) -> a -> b
castWith Refl x = x

-- | Generalized form of type-safe cast, with propositional equality.
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x
