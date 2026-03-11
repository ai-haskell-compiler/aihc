{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T8
  ( T8 (..),
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

data T8 a b c d e f g h
  = T8 a b c d e f g h
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T8 a b c d e f g)

-- | @since 0.1.3
deriving stock instance Functor (T8 a b c d e f g)

-- | @since 0.1.3
deriving stock instance Traversable (T8 a b c d e f g)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq1 (T8 a b c d e f g) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq2 (T8 a b c d e f) where
  liftEq2 e1 e2 (T8 a b c d e f g h) (T8 a' b' c' d' e' f' g' h') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && e1 g g' && e2 h h'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Applicative (T8 a b c d e f g) where
  pure h = T8 mempty mempty mempty mempty mempty mempty mempty h
  T8 a b c d e f g h <*> T8 a' b' c' d' e' f' g' h' =
    T8 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h h')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monad (T8 a b c d e f g) where
  return = pure
  T8 a b c d e f g h >>= i = case i h of
    T8 a' b' c' d' e' f' g' h' ->
      T8 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') h'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable (T8 a b c d e f g h) where
  hash (T8 a b c d e f g h) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable1 (T8 a b c d e f g) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable2 (T8 a b c d e f) where
  liftHashWithSalt2 h1 h2 slt (T8 a b c d e f g h) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f) `h1` g `h2` h

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monoid (T8 a b c d e f g h) where
  mempty = T8 mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h) => NFData (T8 a b c d e f g h) where
  rnf (T8 a b c d e f g h) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h) => Semigroup (T8 a b c d e f g h) where
  T8 a1 b1 c1 d1 e1 f1 g1 h1 <> T8 a2 b2 c2 d2 e2 f2 g2 h2 = T8 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2)
  stimes ii (T8 a b c d e f g h) = T8 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h)

-- | @since 0.1.3
instance Bifunctor (T8 x y z w t u) where
  bimap f g (T8 x y z w t u a b) = T8 x y z w t u (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T8 x y z w t u) where
  bifoldMap f g (T8 _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T8 x y z w t u) where
  bitraverse f g (T8 x y z w t u a b) = T8 x y z w t u <$> f a <*> g b
