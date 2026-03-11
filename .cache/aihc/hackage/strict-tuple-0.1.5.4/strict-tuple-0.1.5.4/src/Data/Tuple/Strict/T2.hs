{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T2
  ( T2 (..),
    sfst,
    ssnd,
    scurry,
    suncurry,
    sswap,
  )
where

import Control.DeepSeq (NFData, rnf)
import Data.Biapplicative (Biapplicative (..))
import Data.Bifoldable
import Data.Bifunctor (Bifunctor (..))
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

data T2 a b
  = T2 a b
  deriving stock (Bounded, Eq, Ord, Read, Show, Generic)

-- | @since 0.1.3
deriving stock instance Foldable (T2 a)

-- | @since 0.1.3
deriving stock instance Functor (T2 a)

-- | @since 0.1.3
deriving stock instance Traversable (T2 a)

-- | @since 0.1.5
instance Eq a => Eq1 (T2 a) where
  liftEq = liftEq2 (==)

-- | @since 0.1.5
instance Eq2 T2 where
  liftEq2 e1 e2 (T2 a b) (T2 a' b') =
    e1 a a' && e2 b b'

-- | @since 0.1.3
instance Monoid a => Applicative (T2 a) where
  pure b = T2 mempty b
  T2 a f <*> T2 a' b = T2 (a <> a') (f b)

-- | @since 0.1.3
instance Monoid a => Monad (T2 a) where
  return = pure
  T2 a b >>= f = case f b of
    T2 a' b' -> T2 (a <> a') b'

instance (Hashable a, Hashable b) => Hashable (T2 a b) where
  hash (T2 a b) = hash a `hashWithSalt` b
  hashWithSalt = hashWithSalt1

instance Hashable a => Hashable1 (T2 a) where
  liftHashWithSalt = defaultLiftHashWithSalt

instance Hashable2 T2 where
  liftHashWithSalt2 h1 h2 slt (T2 a b) = slt `h1` a `h2` b

instance (Monoid a, Monoid b) => Monoid (T2 a b) where
  mempty = T2 mempty mempty

-- | @since 0.1.4
instance (NFData a, NFData b) => NFData (T2 a b) where
  rnf (T2 a b) = rnf a `seq` rnf b

instance (Semigroup a, Semigroup b) => Semigroup (T2 a b) where
  T2 a1 b1 <> T2 a2 b2 = T2 (a1 <> a2) (b1 <> b2)
  stimes ii (T2 a b) = T2 (stimes ii a) (stimes ii b)

-- | @since 0.1.3
instance Bifunctor T2 where
  bimap f g (T2 a b) = T2 (f a) (g b)

-- | @since 0.1.3
instance Biapplicative T2 where
  bipure = T2
  T2 f g <<*>> T2 a b = T2 (f a) (g b)

-- | @since 0.1.3
instance Bifoldable T2 where
  bifoldMap f g (T2 a b) = f a <> g b

-- | @since 0.1.3
instance Bitraversable T2 where
  bitraverse f g (T2 a b) = T2 <$> f a <*> g b

-- | A strict, 'T2'-based analog to 'fst'
--
-- @since 0.1.3
sfst :: T2 a b -> a
sfst (T2 a _) = a

-- | A strict, 'T2'-based analog to 'snd'
--
-- @since 0.1.3
ssnd :: T2 a b -> b
ssnd (T2 _ b) = b

-- | A strict, 'T2'-based analog to 'curry'
--
-- @since 0.1.3
scurry :: (T2 a b -> c) -> a -> b -> c
scurry f a b = f (T2 a b)

-- | A strict, 'T2'-based analog to 'uncurry'
--
-- @since 0.1.3
suncurry :: (a -> b -> c) -> T2 a b -> c
suncurry f (T2 a b) = f a b

-- | A strict, 'T2'-based analog to 'swap'
--
-- @since 0.1.3
sswap :: T2 a b -> T2 b a
sswap (T2 a b) = T2 b a
