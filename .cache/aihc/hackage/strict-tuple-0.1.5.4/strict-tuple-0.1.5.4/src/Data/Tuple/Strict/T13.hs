{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T13
  ( T13 (..),
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

data T13 a b c d e f g h i j k l m
  = T13 a b c d e f g h i j k l m
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T13 a b c d e f g h i j k l)

-- | @since 0.1.3
deriving stock instance Functor (T13 a b c d e f g h i j k l)

-- | @since 0.1.3
deriving stock instance Traversable (T13 a b c d e f g h i j k l)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq1 (T13 a b c d e f g h i j k l) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq2 (T13 a b c d e f g h i j k) where
  liftEq2 e1 e2 (T13 a b c d e f g h i j k l m) (T13 a' b' c' d' e' f' g' h' i' j' k' l' m') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && e1 l l' && e2 m m'

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) =>
  Applicative (T13 a b c d e f g h i j k l)
  where
  pure m = T13 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty m
  T13 a b c d e f g h i j k l m <*> T13 a' b' c' d' e' f' g' h' i' j' k' l' m' =
    T13 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m m')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) =>
  Monad (T13 a b c d e f g h i j k l)
  where
  return = pure
  T13 a b c d e f g h i j k l m >>= n = case n m of
    T13 a' b' c' d' e' f' g' h' i' j' k' l' m' ->
      T13 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') m'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m) => Hashable (T13 a b c d e f g h i j k l m) where
  hash (T13 a b c d e f g h i j k l m) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable1 (T13 a b c d e f g h i j k l) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable2 (T13 a b c d e f g h i j k) where
  liftHashWithSalt2 h1 h2 slt (T13 a b c d e f g h i j k l m) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k) `h1` l `h2` m

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) => Monoid (T13 a b c d e f g h i j k l m) where
  mempty = T13 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m) => NFData (T13 a b c d e f g h i j k l m) where
  rnf (T13 a b c d e f g h i j k l m) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l `seq` rnf m

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m) => Semigroup (T13 a b c d e f g h i j k l m) where
  T13 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 <> T13 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 = T13 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2)
  stimes ii (T13 a b c d e f g h i j k l m) = T13 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m)

-- | @since 0.1.3
instance Bifunctor (T13 x y z w t u v p q r s) where
  bimap f g (T13 x y z w t u v p q r s a b) = T13 x y z w t u v p q r s (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T13 x y z w t u v p q r s) where
  bifoldMap f g (T13 _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T13 x y z w t u v p q r s) where
  bitraverse f g (T13 x y z w t u v p q r s a b) = T13 x y z w t u v p q r s <$> f a <*> g b
