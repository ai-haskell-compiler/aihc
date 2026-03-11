{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T6
  ( T6 (..),
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

data T6 a b c d e f
  = T6 a b c d e f
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T6 a b c d e)

-- | @since 0.1.3
deriving stock instance Functor (T6 a b c d e)

-- | @since 0.1.3
deriving stock instance Traversable (T6 a b c d e)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq1 (T6 a b c d e) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance (Eq a, Eq b, Eq c, Eq d) => Eq2 (T6 a b c d) where
  liftEq2 e1 e2 (T6 a b c d e f) (T6 a' b' c' d' e' f') =
    a == a' && b == b' && c == c' && d == d' && e1 e e' && e2 f f'

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Applicative (T6 a b c d e) where
  pure f = T6 mempty mempty mempty mempty mempty f
  T6 a b c d e f <*> T6 a' b' c' d' e' f' =
    T6 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f f')

-- | @since 0.1.3
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monad (T6 a b c d e) where
  return = pure
  T6 a b c d e f >>= g = case g f of
    T6 a' b' c' d' e' f' -> T6 (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') f'

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable (T6 a b c d e f) where
  hash (T6 a b c d e f) = hash a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f
  hashWithSalt = hashWithSalt1

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable1 (T6 a b c d e) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable2 (T6 a b c d) where
  liftHashWithSalt2 h1 h2 slt (T6 a b c d e f) =
    (slt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d) `h1` e `h2` f

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monoid (T6 a b c d e f) where
  mempty = T6 mempty mempty mempty mempty mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b, NFData c, NFData d, NFData e, NFData f) => NFData (T6 a b c d e f) where
  rnf (T6 a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f) => Semigroup (T6 a b c d e f) where
  T6 a1 b1 c1 d1 e1 f1 <> T6 a2 b2 c2 d2 e2 f2 = T6 (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)
  stimes ii (T6 a b c d e f) = T6 (stimes ii a) (stimes ii b) (stimes ii c) (stimes ii d) (stimes ii e) (stimes ii f)

-- | @since 0.1.3
instance Bifunctor (T6 x y z w) where
  bimap f g (T6 x y z w a b) = T6 x y z w (f a) (g b)

-- | @since 0.1.3
instance Bifoldable (T6 x y z w) where
  bifoldMap f g (T6 _ _ _ _ a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable (T6 x y z w) where
  bitraverse f g (T6 x y z w a b) = T6 x y z w <$> f a <*> g b
