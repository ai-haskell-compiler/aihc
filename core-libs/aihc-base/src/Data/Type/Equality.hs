{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Equality
  ( (:~:) (..),
    type (~),
    type (~~),
    (:~~:) (..),
    type (==),
    TestEquality (..),
    sym,
    trans,
    castWith,
    gcastWith,
  )
where

import Data.Maybe (Maybe (..))
import Data.Type.Bool (type (&&))
import GHC.Types (Bool (..), Type, type (~), type (~~))

infix 4 :~:, :~~:, ==

-- | Propositional equality. A value of type a :~: b proves that a and b are the same type.
data (a :: k) :~: (b :: k) where
  Refl :: a :~: a

-- | Kind-heterogeneous propositional equality.
data (a :: k1) :~~: (b :: k2) where
  HRefl :: a :~~: a

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

-- | A type family to compute Boolean equality. As in @base@, an application
-- is equal to an other application when the functions and the arguments
-- are equal.
type family (a :: k) == (b :: k) :: Bool where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

-- | A type with a decision of equality for the indices of its values.
class TestEquality (f :: k -> Type) where
  -- | Give a proof that the indices are equal, or 'Nothing'.
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality ((:~:) a) where
  testEquality Refl Refl = Just Refl

instance TestEquality ((:~~:) a) where
  testEquality HRefl HRefl = Just Refl
