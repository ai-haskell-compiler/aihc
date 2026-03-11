{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T4
  ( T4 (..),
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

data T4 a b c d
  = T4 a b c d
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T4 a b c)

-- | @since 0.1.3
deriving stock instance Functor (T4 a b c)

-- | @since 0.1.3
deriving stock instance Traversable (T4 a b c)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c) => Eq1 (T4 a b c) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b) => Eq2 (T4 a b) where
  liftEq2 e1 e2 (T4 a b c d) (T4 a' b' c' d') =
    a == a' && b == b' && e1 c c' && e2 d d'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c) => Applicative (T4 a b c) where
  pure d = T4 mempty mempty mempty d
  T4 a b c f <*> T4 a' b' c' d = T4 (a <> a') (b <> b') (c <> c') (f d)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c) => Monad (T4 a b c) where
  return = pure
  T4 a b c d >>= f = case f d of
    T4 a' b' c' d' -> T4 (a <> a') (b <> b') (c <> c') d'

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (T4 a b c d) where
  hash (T4 a b c d) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c) => Hashable1 (T4 a b c) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b) => Hashable2 (T4 a b) where
  liftHashWithSalt2 h1 h2 slt (T4 a b c d) =
    (slt `hashWithSalt` a `hashWithSalt` b) `h1` c `h2` d

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (T4 a b c d) where
  mempty = T4 mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d) => NFData (T4 a b c d) where
  rnf (T4 a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (T4 a b c d) where
  T4 a1 b1 c1 d1 <> T4 a2 b2 c2 d2 = T4 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
  stimes ii (T4 a b c d) = T4 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d)

-- | @since 0.1.3
instance Bifunctor (T4 x y) where
  bimap f g (T4 x y a b) = T4 x y (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T4 x y) where
  bifoldMap f g (T4 _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T4 x y) where
  bitraverse f g (T4 x y a b) = T4 x y <$> f a <*> g b
