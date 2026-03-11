{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T9
  ( T9 (..),
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

data T9 a b c d e f g h i
  = T9 a b c d e f g h i
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T9 a b c d e f g h)

-- | @since 0.1.3
deriving stock instance Functor (T9 a b c d e f g h)

-- | @since 0.1.3
deriving stock instance Traversable (T9 a b c d e f g h)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq1 (T9 a b c d e f g h) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq2 (T9 a b c d e f g) where
  liftEq2 e1 e2 (T9 a b c d e f g h i) (T9 a' b' c' d' e' f' g' h' i') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && e1 h h' && e2 i i'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Applicative (T9 a b c d e f g h) where
  pure i = T9 mempty mempty mempty mempty mempty mempty mempty mempty i
  T9 a b c d e f g h i <*> T9 a' b' c' d' e' f' g' h' i' =
    T9 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') (i i')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monad (T9 a b c d e f g h) where
  return = pure
  T9 a b c d e f g h i >>= j = case j i of
    T9 a' b' c' d' e' f' g' h' i' ->
      T9 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h') i'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i) => Hashable (T9 a b c d e f g h i) where
  hash (T9 a b c d e f g h i) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h `hashWithSalt` i
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h) => Hashable1 (T9 a b c d e f g h) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable2 (T9 a b c d e f g) where
  liftHashWithSalt2 h1 h2 slt (T9 a b c d e f g h i) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g) `h1` h `h2` i

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Monoid i) => Monoid (T9 a b c d e f g h i) where
  mempty = T9 mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g, NFData h, NFData i) => NFData (T9 a b c d e f g h i) where
  rnf (T9 a b c d e f g h i) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g, Semigroup h, Semigroup i) => Semigroup (T9 a b c d e f g h i) where
  T9 a1 b1 c1 d1 e1 f1 g1 h1 i1 <> T9 a2 b2 c2 d2 e2 f2 g2 h2 i2 = T9 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2)
  stimes ii (T9 a b c d e f g h i) = T9 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g) (stimes ii h) (stimes ii i)

-- | @since 0.1.3
instance Bifunctor (T9 x y z w t u v) where
  bimap f g (T9 x y z w t u v a b) = T9 x y z w t u v (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T9 x y z w t u v) where
  bifoldMap f g (T9 _ _ _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T9 x y z w t u v) where
  bitraverse f g (T9 x y z w t u v a b) = T9 x y z w t u v <$> f a <*> g b
