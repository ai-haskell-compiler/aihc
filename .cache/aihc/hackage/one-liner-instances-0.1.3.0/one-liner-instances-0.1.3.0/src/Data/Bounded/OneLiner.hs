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
-- Module      : Data.Bounded.OneLiner
-- Description : Derived methods for Semigroup.
-- Copyright   : (c) Justin Le 2021
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Bounded', using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') where every field is an
-- instance of 'Bounded'.
--
-- Also includes a newtype wrapper that imbues any such data type with an
-- instant 'Bounded' instance, which can one day be used with /DerivingVia/
-- syntax to derive instances automatically.
--

module Data.Bounded.OneLiner (
  -- * Newtype wrapper
    GBounded(..)
  -- * Generics-derived methods
  , gMinBound
  , gMaxBound
  ) where

import           Data.Coerce
import           Data.Data
import           GHC.Generics
import           Generics.OneLiner

-- | If @a@ is a data type whose fields are all instances of 'Bounded',
-- then @'GBounded' a@ has a 'Bounded' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
newtype GBounded a = GBounded { getGBounded :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADT a
         , Constraints a Bounded
         )
      => Bounded (GBounded a) where
    minBound = coerce (gMinBound @a)
    {-# INLINE minBound #-}
    maxBound = coerce (gMaxBound @a)
    {-# INLINE maxBound #-}

-- | 'minBound' implemented by using 'minBound' for all of the components
-- for the first constructor
gMinBound
    :: forall a. (ADT a, Constraints a Bounded)
    => a
gMinBound = case create @Bounded [minBound] of
              []  -> error "minBound: uninhabited"
              x:_ -> x

-- | 'maxBound' implemented by using 'maxBound' for all of the components
-- for the last constructor
gMaxBound
    :: forall a. (ADT a, Constraints a Bounded)
    => a
gMaxBound = case reverse (create @Bounded [maxBound]) of
              []  -> error "maxBound: uninhabited"
              x:_ -> x
