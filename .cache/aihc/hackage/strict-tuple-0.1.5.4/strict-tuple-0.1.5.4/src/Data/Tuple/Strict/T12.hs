{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T12
  ( T12 (..),
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

data T12 a b c d e f g h i j k l
  = T12 a b c d e f g h i j k l
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T12 a b c d e f g h i j k)

-- | @since 0.1.3
deriving stock instance Functor (T12 a b c d e f g h i j k)

-- | @since 0.1.3
deriving stock instance Traversable (T12 a b c d e f g h i j k)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq1 (T12 a b c d e f g h i j k) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq2 (T12 a b c d e f g h i j) where
  liftEq2 e1 e2 (T12 a b c d e f g h i j k l) (T12 a' b' c' d' e' f' g' h' i' j' k' l') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && e1 k k' && e2 l l'

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) =>
  Applicative (T12 a b c d e f g h i j k)
  where
  pure l = T12 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty l
  T12 a b c d e f g h i j k l <*> T12 a' b' c' d' e' f' g' h' i' j' k' l' =
    T12 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') (l l')

-- | @since 0.1.3
instance
  (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) =>
  Monad (T12 a b c d e f g h i j k)
  where
  return = pure
  T12 a b c d e f g h i j k l >>= m = case m l of
    T12 a' b' c' d' e' f' g' h' i' j' k' l' ->
      T12 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k <> k') l'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l) => Hashable (T12 a b c d e f g h i j k l) where
  hash (T12 a b c d e f g h i j k l) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k `hashWithSalt` l
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable1 (T12 a b c d e f g h i j k) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable2 (T12 a b c d e f g h i j) where
  liftHashWithSalt2 h1 h2 slt (T12 a b c d e f g h i j k l) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j) `h1` k `h2` l

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k, Monoid l) => Monoid (T12 a b c d e f g h i j k l) where
  mempty = T12 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k, NFData l) => NFData (T12 a b c d e f g h i j k l) where
  rnf (T12 a b c d e f g h i j k l) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k, Semigroup l) => Semigroup (T12 a b c d e f g h i j k l) where
  T12 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 <> T12 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 = T12 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2) (l1 <> l2)
  stimes ii (T12 a b c d e f g h i j k l) = T12 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k) (stimes ii l)

-- | @since 0.1.3
instance Bifunctor (T12 x y z w t u v p q r) where
  bimap f g (T12 x y z w t u v p q r a b) = T12 x y z w t u v p q r (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T12 x y z w t u v p q r) where
  bifoldMap f g (T12 _ _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T12 x y z w t u v p q r) where
  bitraverse f g (T12 x y z w t u v p q r a b) = T12 x y z w t u v p q r <$> f a <*> g b
