{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

module Data.Tuple.Strict.T1
  ( T1 (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype T1 a
  = T1 a
  deriving stock (Bounded, Eq, Generic, Ord, Read, Show)
  deriving newtype (Hashable, Monoid, NFData, Semigroup)

-- | @since 0.1.3
deriving stock instance Foldable T1

-- | @since 0.1.3
deriving stock instance Functor T1

-- | @since 0.1.3
deriving stock instance Traversable T1

-- | @since 0.1.3
instance Applicative T1 where
  pure = T1
  T1 f <*> T1 a = T1 (f a)

-- | @since 0.1.3
instance Monad T1 where
  return = pure
  T1 a >>= f = f a
