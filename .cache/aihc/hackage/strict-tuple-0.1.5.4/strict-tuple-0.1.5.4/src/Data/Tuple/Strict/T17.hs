{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T17
  ( T17 (..),
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

data T17 a b c d e f g h i j k l m n o p q
  = T17 a b c d e f g h i j k l m n o p q
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.3
deriving stock instance Functor (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.3
deriving stock instance Traversable (T17 a b c d e f g h i j k l m n o p)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p) => Eq1 (T17 a b c d e f g h i j k l m n o p) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq2 (T17 a b c d e f g h i j k l m n o) where
  liftEq2 e1 e2 (T17 a b c d e f g h i j k l m n o p q) (T17 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' && n == n' && o == o' && e1 p p' && e2 q q'

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) =>
  Applicative (T17 a b c d e f g h i j k l m n o p)
  where
  pure q = T17 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty q
  T17 a b c d e f g h i j k l m n o p q <*> T17 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' =
    T17 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') (q q')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p) =>
  Monad (T17 a b c d e f g h i j k l m n o p)
  where
  return = pure
  T17 a b c d e f g h i j k l m n o p q >>= r = case r q of
    T17 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' ->
      T17 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p') q'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p, Hashable q) => Hashable (T17 a b c d e f g h i j k l m n o p q) where
  hash (T17 a b c d e f g h i j k l m n o p q) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o `hashWithSalt` p `hashWithSalt` q
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o, Hashable p) => Hashable1 (T17 a b c d e f g h i j k l m n o p) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o) => Hashable2 (T17 a b c d e f g h i j k l m n o) where
  liftHashWithSalt2 h1 h2 slt (T17 a b c d e f g h i j k l m n o p q) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l `hashWithSalt` m `hashWithSalt` n `hashWithSalt` o) `h1` p `h2` q

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l, Monoid m, Monoid n, Monoid o, Monoid p, Monoid q) => Monoid (T17 a b c d e f g h i j k l m n o p q) where
  mempty = T17 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l, NFData m, NFData n, NFData o, NFData p, NFData q) => NFData (T17 a b c d e f g h i j k l m n o p q) where
  rnf (T17 a b c d e f g h i j k l m n o p q) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l `seq` rnf m `seq` rnf n `seq` rnf o `seq` rnf p `seq` rnf q

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l, Semigroup m, Semigroup n, Semigroup o, Semigroup p, Semigroup q) => Semigroup (T17 a b c d e f g h i j k l m n o p q) where
  T17 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 <> T17 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 = T17 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2) (m1 <> m2) (n1 <> n2) (o1 <> o2) (p1 <> p2) (q1 <> q2)
  stimes ii (T17 a b c d e f g h i j k l m n o p q) = T17 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l) (stimes ii m) (stimes ii n) (stimes ii o) (stimes ii p) (stimes ii q)

-- | @since 0.1.3
instance Bifunctor (T17 x y z w t u v p q r s i j k l) where
  bimap f g (T17 x y z w t u v p q r s i j k l a b) = T17 x y z w t u v p q r s i j k l (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T17 x y z w t u v p q r s i j k l) where
  bifoldMap f g (T17 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T17 x y z w t u v p q r s i j k l) where
  bitraverse f g (T17 x y z w t u v p q r s i j k l a b) = T17 x y z w t u v p q r s i j k l <$> f a <*> g b
