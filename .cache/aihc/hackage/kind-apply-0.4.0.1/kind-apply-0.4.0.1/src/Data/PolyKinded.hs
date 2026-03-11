{-# language ConstraintKinds        #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language DataKinds              #-}
{-# language GADTs                  #-}
{-# language MultiParamTypeClasses  #-}
{-# language PolyKinds              #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
-- | Representation of types as constructor + list of types.
module Data.PolyKinded (
  -- * Lists of types and application
  LoT(..), (:@@:), LoT1, LoT2
, HeadLoT, TailLoT, SpineLoT
  -- ** Singleton for list of types
, SLoT(..), SForLoT(..), Proxy(..)
  -- * Splitting types
, SplitF, Nat(..), TyEnv(..), SplitN
) where

import           Data.Kind
import           Data.Proxy

-- $setup
-- >>> :set -XTypeOperators -XDataKinds -XNoStarIsType

infixr 5 :&&:
-- | @LoT k@ represents a list of types which can be applied
-- to a data type of kind @k@.
data LoT k where
  -- | Empty list of types.
  LoT0    ::                LoT Type
  -- | Cons a type with a list of types.
  (:&&:)  :: k -> LoT ks -> LoT (k -> ks)

-- | List of types with a single element.
type LoT1 a = a ':&&: 'LoT0
-- | List of types with two elements.
type LoT2 a b = a ':&&: b ':&&: 'LoT0

-- | Apply a list of types to a type constructor.
--
-- >>> :kind! Either :@@: (Int :&&: Bool :&&: LoT0)
-- Either :@@: (Int :&&: Bool :&&: LoT0) :: Type
-- = Either Int Bool
type family (f :: k) :@@: (tys :: LoT k) :: Type where
  f :@@: _  = f
  f :@@: as = f (HeadLoT as) :@@: TailLoT as

-- | Head of a non-empty list of types.
--
-- >>> :kind! HeadLoT (Int :&&: LoT0)
-- HeadLoT (Int :&&: LoT0) :: Type
-- = Int
type family HeadLoT (tys :: LoT (k -> k')) :: k where
  HeadLoT (a ':&&: _) = a

-- | Tail of a non-empty list of types.
--
-- >>> :kind! TailLoT (Int :&&: Bool :&&: LoT0)
-- TailLoT (Int :&&: Bool :&&: LoT0) :: LoT (Type -> Type)
-- = Bool :&&: LoT0
type family TailLoT (tys :: LoT (k -> k')) :: LoT k' where
  TailLoT (_ ':&&: as) = as

-- | Construct the spine of a list of types whose length is known.
--
-- It can be useful to introduce unification variables for lists of types which
-- will be fully instantiated during constraint resolution.
-- A constraint @p ~ SpineLoT p@ will thus instantiate the spine of @p@.
--
-- On concrete lists, this is the identity function.
type family SpineLoT (tys :: LoT k) = (tys' :: LoT k) | tys' -> tys where
  SpineLoT (a ':&&: as) = a ':&&: SpineLoT as
  SpineLoT 'LoT0        = 'LoT0

data SLoT (l :: LoT k) where
  SLoT0 :: SLoT 'LoT0
  SLoTA :: Proxy t -> SLoT ts -> SLoT (t ':&&: ts)

class SForLoT (l :: LoT k) where
  slot :: SLoT l
instance (l ~ 'LoT0) => SForLoT (l :: LoT Type) where
  slot = SLoT0
instance (l ~ (t ':&&: ts), SForLoT ts) => SForLoT (l :: LoT (k -> k')) where
  slot = SLoTA Proxy slot

-- | Split a type @t@ until the constructor @f@ is found.
--
-- >>> :kind! SplitF (Either Int Bool) Either
-- SplitF (Either Int Bool) Either :: LoT (Type -> Type -> Type)
-- = Int :&&: (Bool :&&: LoT0)
-- >>> :kind! SplitF (Either Int Bool) (Either Int)
-- SplitF (Either Int Bool) (Either Int) :: LoT (Type -> Type)
-- = Bool :&&: LoT0
type SplitF (t :: d) (f :: k) = SplitF' t f 'LoT0
type family SplitF' (t :: d) (f :: k) (p :: LoT l) :: LoT k where
  SplitF' f     f acc = acc
  SplitF' (t a) f acc = SplitF' t f (a ':&&: acc)

-- | Simple natural numbers.
data Nat = Z | S Nat

-- | A type constructor and a list of types that can be applied to it.
data TyEnv where
  TyEnv :: forall k. k -> LoT k -> TyEnv

-- | Split a type @t@ until its list of types has length @n@.
--
-- >>> :kind! SplitN (S (S Z)) (Either Int Bool)
-- SplitN (S (S Z)) (Either Int Bool) :: TyEnv
-- = 'TyEnv Either (Int :&&: (Bool :&&: LoT0))
-- >>> :kind! SplitN (S Z) (Either Int Bool)
-- SplitN (S Z) (Either Int Bool) :: TyEnv
-- = 'TyEnv (Either Int) (Bool :&&: LoT0)
type family SplitN (n :: Nat) t :: TyEnv where
  SplitN n t = SplitN' n t 'LoT0
type family SplitN' (n :: Nat) (t :: d) (p :: LoT d) :: TyEnv where
  SplitN' 'Z     t     acc = 'TyEnv t acc
  SplitN' ('S n) (t a) acc = SplitN' n t (a ':&&: acc)
