{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T11
  ( T11 (..),
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

data T11 a b c d e f g h i j k
  = T11 a b c d e f g h i j k
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T11 a b c d e f g h i j)

-- | @since 0.1.3
deriving stock instance Functor (T11 a b c d e f g h i j)

-- | @since 0.1.3
deriving stock instance Traversable (T11 a b c d e f g h i j)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq1 (T11 a b c d e f g h i j) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq2 (T11 a b c d e f g h i) where
  liftEq2 e1 e2 (T11 a b c d e f g h i j k) (T11 a' b' c' d' e' f' g' h' i' j' k') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && e1 j j' && e2 k k'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Applicative (T11 a b c d e f g h i j) where
  pure k = T11 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty k
  T11 a b c d e f g h i j k <*> T11 a' b' c' d' e' f' g' h' i' j' k' =
    T11 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') (k k')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j) => Monad (T11 a b c d e f g h i j) where
  return = pure
  T11 a b c d e f g h i j k >>= l = case l k of
    T11 a' b' c' d' e' f' g' h' i' j' k' ->
      T11 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i <> i') (j <> j') k'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k) => Hashable (T11 a b c d e f g h i j k) where
  hash (T11 a b c d e f g h i j k) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i `hashWithSalt` j `hashWithSalt` k
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j) => Hashable1 (T11 a b c d e f g h i j) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable2 (T11 a b c d e f g h i) where
  liftHashWithSalt2 h1 h2 slt (T11 a b c d e f g h i j k) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i) `h1` j `h2` k

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i, Monoid j, Monoid k) => Monoid (T11 a b c d e f g h i j k) where
  mempty = T11 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i, NFData j, NFData k) => NFData (T11 a b c d e f g h i j k) where
  rnf (T11 a b c d e f g h i j k) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i, Semigroup j, Semigroup k) => Semigroup (T11 a b c d e f g h i j k) where
  T11 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 <> T11 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 = T11 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2) (k1 <> k2)
  stimes ii (T11 a b c d e f g h i j k) = T11 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i) (stimes ii j) (stimes ii k)

-- | @since 0.1.3
instance Bifunctor (T11 x y z w t u v p q) where
  bimap f g (T11 x y z w t u v p q a b) = T11 x y z w t u v p q (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T11 x y z w t u v p q) where
  bifoldMap f g (T11 _ _ _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T11 x y z w t u v p q) where
  bitraverse f g (T11 x y z w t u v p q a b) = T11 x y z w t u v p q <$> f a <*> g b
