{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T14
  ( T14 (..),
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

data T14 a b c d e f g h i j k l m n
  = T14 a b c d e f g h i j k l m n
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T14 a b c d e f g h i j k l m)

-- | @since 0.1.3
deriving stock instance Functor (T14 a b c d e f g h i j k l m)

-- | @since 0.1.3
deriving stock instance Traversable (T14 a b c d e f g h i j k l m)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq1 (T14 a b c d e f g h i j k l m) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq2 (T14 a b c d e f g h i j k l) where
  liftEq2 e1 e2 (T14 a b c d e f g h i j k l m n) (T14 a' b' c' d' e' f' g' h' i' j' k' l' m' n') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && e1 m m' && e2 n n'

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) =>
  Applicative (T14 a b c d e f g h i j k l m)
  where
  pure n = T14 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty n
  T14 a b c d e f g h i j k l m n <*> T14 a' b' c' d' e' f' g' h' i' j' k' l' m' n' =
    T14 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n n')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m) =>
  Monad (T14 a b c d e f g h i j k l m)
  where
  return = pure
  T14 a b c d e f g h i j k l m n >>= o = case o n of
    T14 a' b' c' d' e' f' g' h' i' j' k' l' m' n' ->
      T14 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') n'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n) => Hashable (T14 a b c d e f g h i j k l m n) where
  hash (T14 a b c d e f g h i j k l m n) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m) => Hashable1 (T14 a b c d e f g h i j k l m) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable2 (T14 a b c d e f g h i j k l) where
  liftHashWithSalt2 h1 h2 slt (T14 a b c d e f g h i j k l m n) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l) `h1` m `h2` n

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n) => Monoid (T14 a b c d e f g h i j k l m n) where
  mempty = T14 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n) => NFData (T14 a b c d e f g h i j k l m n) where
  rnf (T14 a b c d e f g h i j k l m n) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l `seq` rnf m `seq` rnf n

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n) => Semigroup (T14 a b c d e f g h i j k l m n) where
  T14 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 <> T14 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 = T14 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2)
  stimes ii (T14 a b c d e f g h i j k l m n) = T14 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n)

-- | @since 0.1.3
instance Bifunctor (T14 x y z w t u v p q r s i) where
  bimap f g (T14 x y z w t u v p q r s i a b) = T14 x y z w t u v p q r s i (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T14 x y z w t u v p q r s i) where
  bifoldMap f g (T14 _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T14 x y z w t u v p q r s i) where
  bitraverse f g (T14 x y z w t u v p q r s i a b) = T14 x y z w t u v p q r s i <$> f a <*> g b
