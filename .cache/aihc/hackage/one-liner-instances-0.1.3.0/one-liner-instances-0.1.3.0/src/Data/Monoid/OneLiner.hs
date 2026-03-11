{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Monoid.OneLiner
-- Description : Derived methods for Semigroup and Monoid.
-- Copyright   : (c) Justin Le 2021
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Semigroup' and 'Monoid', using "Generics.OneLiner"
-- and "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Semigroup' (or
-- 'Monoid', depending on the function).
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Semigroup' and 'Monoid' instances, which can one day be used
-- with /DerivingVia/ syntax to derive instances automatically.
--

module Data.Monoid.OneLiner (
  -- * Newtype wrapper
    GMonoid(..)
  -- * Generics-derived methods
  -- ** Semigroup
  , gSemigroup
  -- ** Monoid
  , gMappend
  , gMempty
  ) where

import           Data.Coerce
import           Data.Data
import           GHC.Generics
import           Generics.OneLiner

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Semigroup', then @'GMonoid' a@ has a 'Semigroup' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Monoid', then @'GMonoid' a@ has a 'Monoid' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
newtype GMonoid a = GMonoid { getGMonoid :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADTRecord a
         , Constraints a Semigroup
         )
      => Semigroup (GMonoid a) where
    (<>) = coerce (gSemigroup @a)
    {-# INLINE (<>) #-}

instance ( ADTRecord a
         , Constraints a Semigroup
         , Constraints a Monoid
         )
      => Monoid (GMonoid a) where
    mappend = coerce (gMappend @a)
    {-# INLINE mappend #-}
    mempty  = coerce (gMempty @a)
    {-# INLINE mempty #-}


-- | Semigroup append ('<>') implemented by calling '<>' on the components.
gSemigroup
    :: forall a. (ADTRecord a, Constraints a Semigroup)
    => a -> a -> a
gSemigroup = binaryOp @Semigroup (<>)
{-# INLINE gSemigroup #-}

-- | Monoid append ('mappend') implemented by calling '<>' on the
-- components.
gMappend
    :: forall a. (ADTRecord a, Constraints a Monoid)
    => a -> a -> a
gMappend = binaryOp @Monoid mappend
{-# INLINE gMappend #-}

-- | Monoid identity ('mempty') implemented by using 'mempty' for all of
-- the components.
gMempty
    :: forall a. (ADTRecord a, Constraints a Monoid)
    => a
gMempty = nullaryOp @Monoid mempty
{-# INLINE gMempty #-}

