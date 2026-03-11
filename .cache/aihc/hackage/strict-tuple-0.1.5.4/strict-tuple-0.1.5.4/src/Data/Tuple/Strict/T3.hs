{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T3
  ( T3 (..),
  )
where

import Control.DeepSeq (NFData, rnf)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2))
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Hashable.Lifted
  ( Hashable1,
    Hashable2,
    defaultLiftHashWithSalt,
    hashWithSalt1,
    liftHashWithSalt,
    liftHashWithSalt2,
  )
import Data.Semigroup
import GHC.Generics (Generic)

data T3 a b c
  = T3 a b c
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T3 a b)

-- | @since 0.1.3
deriving stock instance Functor (T3 a b)

-- | @since 0.1.3
deriving stock instance Traversable (T3 a b)

-- | @since 0.1.5
instance (Eq a, Eq b) => Eq1 (T3 a b) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance Eq a => Eq2 (T3 a) where
  liftEq2 e1 e2 (T3 a b c ) (T3 a' b' c') =
    a == a' && e1 b b' && e2 c c'

-- | @since 0.1.3
instance (Monoid a, Monoid b) => Applicative (T3 a b) where
  pure c = T3 mempty mempty c
  T3 a b f <*> T3 a' b' c = T3 (a <> a') (b <> b') (f c)

-- | @since 0.1.3
instance (Monoid a, Monoid b) => Monad (T3 a b) where
  return = pure
  T3 a b c >>= f = case f c of
    T3 a' b' c' -> T3 (a <> a') (b <> b') c'

instance (Hashable a, Hashable b, Hashable c) => Hashable (T3 a b c) where
  hash (T3 a b c) = hash a `hashWithSalt` b `hashWithSalt` c
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b) => Hashable1 (T3 a b) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable a => Hashable2 (T3 a) where
  liftHashWithSalt2 h1 h2 slt (T3 a b c) =
    (slt `hashWithSalt` a) `h1` b `h2` c

instance (Monoid a, Monoid b, Monoid c) => Monoid (T3 a b c) where
  mempty = T3 mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c) => NFData (T3 a b c) where
  rnf (T3 a b c) = rnf a `seq` rnf b `seq` rnf c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (T3 a b c) where
  T3 a1 b1 c1 <> T3 a2 b2 c2 = T3 (a1 <> a2) (b1 <> b2) (c1 <> c2)
  stimes ii (T3 a b c) = T3 (stimes ii a) (stimes ii b) (stimes ii c)

-- | @since 0.1.3
instance Bifunctor (T3 x) where
  bimap f g (T3 x a b) = T3 x (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T3 x) where
  bifoldMap f g (T3 _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T3 x) where
  bitraverse f g (T3 x a b) = T3 x <$> f a <*> g b
