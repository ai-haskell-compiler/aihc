{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T10
  ( T10 (..),
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

data T10 a b c d e f g h i j
  = T10 a b c d e f g h i j
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T10 a b c d e f g h i)

-- | @since 0.1.3
deriving stock instance Functor (T10 a b c d e f g h i)

-- | @since 0.1.3
deriving stock instance Traversable (T10 a b c d e f g h i)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq1 (T10 a b c d e f g h i) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq2 (T10 a b c d e f g h) where
  liftEq2 e1 e2 (T10 a b c d e f g h i j) (T10 a' b' c' d' e' f' g' h' i' j') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && e1 i i' && e2 j j'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Applicative (T10 a b c d e f g h i) where
  pure j = T10 mempty mempty mempty mempty mempty mempty mempty mempty mempty j
  T10 a b c d e f g h i j <*> T10 a' b' c' d' e' f' g' h' i' j' =
    T10 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j j')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Monad (T10 a b c d e f g h i) where
  return = pure
  T10 a b c d e f g h i j >>= k = case k j of
    T10 a' b' c' d' e' f' g' h' i' j' ->
      T10 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') j'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable (T10 a b c d e f g h i j) where
  hash (T10 a b c d e f g h i j) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable1 (T10 a b c d e f g h i) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable2 (T10 a b c d e f g h) where
  liftHashWithSalt2 h1 h2 slt (T10 a b c d e f g h i j) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h) `h1` i `h2` j

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Monoid (T10 a b c d e f g h i j) where
  mempty = T10 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j) => NFData (T10 a b c d e f g h i j) where
  rnf (T10 a b c d e f g h i j) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j) => Semigroup (T10 a b c d e f g h i j) where
  T10 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 <> T10 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 = T10 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2)
  stimes ii (T10 a b c d e f g h i j) = T10 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j)

-- | @since 0.1.3
instance Bifunctor (T10 x y z w t u v p) where
  bimap f g (T10 x y z w t u v p a b) = T10 x y z w t u v p (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T10 x y z w t u v p) where
  bifoldMap f g (T10 _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T10 x y z w t u v p) where
  bitraverse f g (T10 x y z w t u v p a b) = T10 x y z w t u v p <$> f a <*> g b
