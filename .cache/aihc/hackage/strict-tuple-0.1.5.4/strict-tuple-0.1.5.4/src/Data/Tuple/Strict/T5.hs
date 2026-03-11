{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T5
  ( T5 (..),
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

data T5 a b c d e
  = T5 a b c d e
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T5 a b c d)

-- | @since 0.1.3
deriving stock instance Functor (T5 a b c d)

-- | @since 0.1.3
deriving stock instance Traversable (T5 a b c d)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d) => Eq1 (T5 a b c d) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c) => Eq2 (T5 a b c) where
  liftEq2 e1 e2 (T5 a b c d e) (T5 a' b' c' d' e') =
    a == a' && b == b' && c == c' && e1 d d' && e2 e e'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Applicative (T5 a b c d) where
  pure e = T5 mempty mempty mempty mempty e
  T5 a b c d f <*> T5 a' b' c' d' e = T5 (a <> a') (b <> b') (c <> c') (d <> d') (f e)

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monad (T5 a b c d) where
  return = pure
  T5 a b c d e >>= f = case f e of
    T5 a' b' c' d' e' -> T5 (a <> a') (b <> b') (c <> c') (d <> d') e'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (T5 a b c d e) where
  hash (T5 a b c d e) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable1 (T5 a b c d) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c) => Hashable2 (T5 a b c) where
  liftHashWithSalt2 h1 h2 slt (T5 a b c d e) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c) `h1` d `h2` e

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (T5 a b c d e) where
  mempty = T5 mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e) => NFData (T5 a b c d e) where
  rnf (T5 a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (T5 a b c d e) where
  T5 a1 b1 c1 d1 e1 <> T5 a2 b2 c2 d2 e2 = T5 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
  stimes ii (T5 a b c d e) = T5 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e)

-- | @since 0.1.3
instance Bifunctor (T5 x y z) where
  bimap f g (T5 x y z a b) = T5 x y z (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T5 x y z) where
  bifoldMap f g (T5 _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T5 x y z) where
  bitraverse f g (T5 x y z a b) = T5 x y z <$> f a <*> g b
