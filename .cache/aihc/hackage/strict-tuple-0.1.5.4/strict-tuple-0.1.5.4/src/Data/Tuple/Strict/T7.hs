{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T7
  ( T7 (..),
  )
where

import Control.DeepSeq (NFData, rnf)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2))
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

data T7 a b c d e f g
  = T7 a b c d e f g
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T7 a b c d e f)

-- | @since 0.1.3
deriving stock instance Functor (T7 a b c d e f)

-- | @since 0.1.3
deriving stock instance Traversable (T7 a b c d e f)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq1 (T7 a b c d e f) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq2 (T7 a b c d e) where
  liftEq2 e1 e2 (T7 a b c d e f g) (T7 a' b' c' d' e' f' g') =
    a == a' && b == b' && c == c' && d == d' && e == e' && e1 f f' && e2 g g'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Applicative (T7 a b c d e f) where
  pure g = T7 mempty mempty mempty mempty mempty mempty g
  T7 a b c d e f g <*> T7 a' b' c' d' e' f' g' =
    T7 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g g')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monad (T7 a b c d e f) where
  return = pure
  T7 a b c d e f g >>= h = case h g of
    T7 a' b' c' d' e' f' g' -> T7 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') g'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g) => Hashable (T7 a b c d e f g) where
  hash (T7 a b c d e f g) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable1 (T7 a b c d e f) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable2 (T7 a b c d e) where
  liftHashWithSalt2 h1 h2 slt (T7 a b c d e f g) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e) `h1` f `h2` g

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monoid (T7 a b c d e f g) where
  mempty = T7 mempty mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f, NFData g) => NFData (T7 a b c d e f g) where
  rnf (T7 a b c d e f g) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f, Semigroup g) => Semigroup (T7 a b c d e f g) where
  T7 a1 b1 c1 d1 e1 f1 g1 <> T7 a2 b2 c2 d2 e2 f2 g2 = T7 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)
  stimes ii (T7 a b c d e f g) = T7 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f) (stimes ii g)

-- | @since 0.1.3
instance Bifunctor (T7 x y z w t) where
  bimap f g (T7 x y z w t a b) = T7 x y z w t (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T7 x y z w t) where
  bifoldMap f g (T7 _ _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T7 x y z w t) where
  bitraverse f g (T7 x y z w t a b) = T7 x y z w t <$> f a <*> g b
