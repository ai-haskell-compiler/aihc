{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Type.Membership
  ( -- * Membership
  Membership
  , getMemberId
  , mkMembership
  , leadership
  , nextMembership
  , testMembership
  , compareMembership
  , impossibleMembership
  , reifyMembership
  , Member
  -- * Association
  , Assoc(..)
  , type (>:)
  , Lookup(..)
  , KeyOf
  , KeyIs
  , proxyKeyOf
  , stringKeyOf
  , TargetOf
  , proxyTargetOf
  , TargetIs
  , KeyTargetAre
  -- * Enumeration
  , Generate(..)
  , Forall(..)
  , ForallF
  -- * Reexports
  , Proxy(..)
  ) where

import Data.Constraint
import Data.Proxy
import Data.String
import GHC.TypeLits
import Type.Membership.Internal

-- | Every type-level list is an instance of 'Generate'.
class Generate (xs :: [k]) where
  -- | Enumerate all possible 'Membership's of @xs@.
  henumerate :: (forall x. Membership xs x -> r -> r) -> r -> r

  -- | Count the number of memberships.
  hcount :: proxy xs -> Int

  -- | Enumerate 'Membership's and construct an 'HList'.
  hgenerateList :: Applicative f
    => (forall x. Membership xs x -> f (h x)) -> f (HList h xs)

instance Generate '[] where
  henumerate _ r = r

  hcount _ = 0

  hgenerateList _ = pure HNil

instance Generate xs => Generate (x ': xs) where
  henumerate f r = f leadership $ henumerate (f . nextMembership) r

  hcount _ = 1 + hcount (Proxy :: Proxy xs)

  -- | Enumerate 'Membership's and construct an 'HList'.
  hgenerateList f = HCons <$> f leadership <*> hgenerateList (f . nextMembership)

-- | Every element in @xs@ satisfies @c@
class (ForallF c xs, Generate xs) => Forall (c :: k -> Constraint) (xs :: [k]) where
  -- | Enumerate all possible 'Membership's of @xs@ with an additional context.
  henumerateFor :: proxy c -> proxy' xs -> (forall x. c x => Membership xs x -> r -> r) -> r -> r

  hgenerateListFor :: Applicative f
    => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (HList h xs)

instance Forall c '[] where
  henumerateFor _ _ _ r = r

  hgenerateListFor _ _ = pure HNil

instance (c x, Forall c xs) => Forall c (x ': xs) where
  henumerateFor p _ f r = f leadership $ henumerateFor p (Proxy :: Proxy xs) (f . nextMembership) r

  hgenerateListFor p f = HCons <$> f leadership <*> hgenerateListFor p (f . nextMembership)

-- | HACK: Without this, the constraints are not propagated well.
type family ForallF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  ForallF c '[] = ()
  ForallF c (x ': xs) = (c x, Forall c xs)

-- | Take the type of the key
type family KeyOf (kv :: Assoc k v) :: k where
  KeyOf (k ':> v) = k

-- | Proxy-level 'KeyOf'. This is useful when using 'symbolVal'.
proxyKeyOf :: proxy kv -> Proxy (KeyOf kv)
proxyKeyOf _ = Proxy

-- | Get a string from a proxy of @'Assoc' 'Symbol' v@.
stringKeyOf :: (IsString a, KnownSymbol (KeyOf kv)) => proxy kv -> a
stringKeyOf = fromString . symbolVal . proxyKeyOf
{-# INLINE stringKeyOf #-}

-- | Constraint applied to 'KeyOf'
class (pk (KeyOf kv)) => KeyIs pk kv where

instance (pk k) => KeyIs pk (k ':> v)

-- | Take the type of the value
type family TargetOf (kv :: Assoc k v) :: v where
  TargetOf (k ':> v) = v

-- | Proxy-level 'TargetOf'.
proxyTargetOf :: proxy kv -> Proxy (TargetOf kv)
proxyTargetOf _ = Proxy

-- | Constraint applied to 'TargetOf'
class (pv (TargetOf kv)) => TargetIs pv kv where

instance (pv v) => TargetIs pv (k ':> v)

-- | Combined constraint for 'Assoc'
class (pk (KeyOf kv), pv (TargetOf kv)) => KeyTargetAre pk pv kv where

instance (pk k, pv v) => KeyTargetAre pk pv (k ':> v)
