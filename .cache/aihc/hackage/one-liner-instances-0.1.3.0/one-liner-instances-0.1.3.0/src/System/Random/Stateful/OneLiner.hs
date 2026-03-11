{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : System.Random.Stateful.OneLiner
-- Description : Derived methods for Random.
-- Copyright   : (c) Justin Le 2021
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Uniform', using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Uniform'.
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Uniform' instances, which can one day be used with /DerivingVia/
-- syntax to derive instances automatically.
--
-- @since 0.1.3.0
module System.Random.Stateful.OneLiner (
  -- * Single constructor
  -- ** Newtype wrapper
    GUniform(..)
  -- ** Generics-derived methods
  , gUniformM
  , gUniformRM
  , gUniformListM
  , gRandomM
  , gRandomRM
  -- * Multiple constructor
  -- ** Newtype wrapper
  , GUniformSum(..)
  -- ** Generics-derived methods
  , gUniformSumM
  , gUniformRSumM
  , gUniformSumListM
  , gRandomSumM
  , gRandomRSumM
  ) where

import           Control.Monad
import           Data.Data
import           Data.Functor.Compose
import           Data.List.NonEmpty              (NonEmpty(..))
import           Data.Maybe
import           GHC.Generics
import           Generics.OneLiner
import           System.Random
import           System.Random.OneLiner
import           System.Random.OneLiner.Internal (Pair(..), dePair)
import           System.Random.Stateful
import qualified Data.List.NonEmpty              as NE

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Uniform', then @'GUniform' a@ has a 'Uniform' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
-- Only works with data types with single constructors.  If you need it to
-- work with types of multiple constructors, consider 'GUniformSum'.
newtype GUniform a = GUniform { getGUniform :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADTRecord a
         , Constraints a Uniform
         )
      => Uniform (GUniform a) where
    uniformM :: forall g m. StatefulGen g m => g -> m (GUniform a)
    uniformM g = GUniform <$> gUniformM g
    {-# INLINE uniformM #-}

instance ( ADTRecord a
         , Constraints a UniformRange
         )
      => UniformRange (GUniform a) where
    uniformRM :: forall g m. StatefulGen g m => (GUniform a, GUniform a) -> g -> m (GUniform a)
    uniformRM (GUniform l, GUniform r) g = GUniform <$> gUniformRM (l, r) g
    {-# INLINE uniformRM #-}

gUniformM
   :: forall a g m. (ADTRecord a, Constraints a Uniform, StatefulGen g m)
   => g
   -> m a
gUniformM g = createA' @Uniform (uniformM g)
{-# INLINE gUniformM #-}

gUniformRM
    :: forall a g m. (ADTRecord a, Constraints a UniformRange, StatefulGen g m)
    => (a, a) -> g -> m a
gUniformRM (l, u) g = dialgebra @UniformRange
    ((`uniformRM` g) . dePair)
    (Pair l u)
{-# INLINE gUniformRM #-}

-- | Uses the 'Random' instance instead of the 'Uniform' instance.
gRandomM
    :: forall a g m r. (ADTRecord a, Constraints a Random, RandomGenM g r m)
    => g -> m a
gRandomM = applyRandomGenM gRandom

-- | Uses the 'Random' instance instead of the 'Uniform' instance.
gRandomRM
    :: forall a g m r. (ADTRecord a, Constraints a Random, RandomGenM g r m)
    => (a, a) -> g -> m a
gRandomRM r = applyRandomGenM (gRandomR r)

gUniformListM
   :: forall a g m. (ADTRecord a, Constraints a Uniform, StatefulGen g m)
   => Int
   -> g
   -> m [a]
gUniformListM n g = replicateM n (gUniformM g)

-- | If @a@ is a data type whose fields are all instances of 'Uniform', then
-- @'GUniform' a@ has a 'Uniform' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
-- A version of 'GUniform' that works for data types with multiple
-- constructors.  If your type has only one constructor, it might be more
-- performant to use 'GUniform'.
--
-- Note that the "ranged" variants are partial: if given a range of items
-- made with different constructors, will be 'error'!
newtype GUniformSum a = GUniformSum { getGUniformSum :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADT a
         , Constraints a Uniform
         )
      => Uniform (GUniformSum a) where
    uniformM :: forall g m. StatefulGen g m => g -> m (GUniformSum a)
    uniformM g = GUniformSum <$> gUniformSumM g
    {-# INLINE uniformM #-}

-- | Note that this is partial: the range limits must have the same
-- constructors.
instance ( ADT a
         , Constraints a UniformRange
         )
      => UniformRange (GUniformSum a) where
    uniformRM :: forall g m. StatefulGen g m => (GUniformSum a, GUniformSum a) -> g -> m (GUniformSum a)
    uniformRM (GUniformSum l, GUniformSum u) g = GUniformSum <$> gUniformRSumM (l, u) g
    {-# INLINE uniformRM #-}

-- | 'randomR' implemented by sequencing 'randomR' between all components.
--
-- If given a range of items made with different constructors, will be
-- 'error'!
gUniformRSumM
    :: forall a g m. (ADT a, Constraints a UniformRange, StatefulGen g m)
    => (a, a) -> g -> m a
gUniformRSumM (l, u) g = fromMaybe (error badbad) . getCompose $
    zipWithA @UniformRange
      (\l' u' -> Compose (Just (uniformRM (l', u') g)))
      l u
  where
    badbad = "gUniformRSum: Constructors in range do not match."
{-# INLINE gUniformRSumM #-}

-- | 'random' implemented by selecting a random constructor and sequencing
-- 'random' for all components.
gUniformSumM
    :: forall a g m. (ADT a, Constraints a Uniform, StatefulGen g m)
    => g -> m a
gUniformSumM g = case options of
    Nothing   -> pure (error "gUniformSumM: Uninhabited type")
    Just opts -> join $ reservoir opts g
  where
    options = NE.nonEmpty . getCompose $ createA @Uniform @a $
        Compose [uniformM g]
{-# INLINE gUniformSumM #-}

-- | Uses the 'Random' instance instead of the 'Uniform' instance.
gRandomSumM
    :: forall a g m r. (ADT a, Constraints a Random, RandomGenM g r m)
    => g -> m a
gRandomSumM = applyRandomGenM gRandomSum

-- | Uses the 'Random' instance instead of the 'Uniform' instance.
gRandomRSumM
    :: forall a g m r. (ADT a, Constraints a Random, RandomGenM g r m)
    => (a, a) -> g -> m a
gRandomRSumM r = applyRandomGenM (gRandomRSum r)

gUniformSumListM
   :: forall a g m. (ADT a, Constraints a Uniform, StatefulGen g m)
   => Int
   -> g
   -> m [a]
gUniformSumListM n g = replicateM n (gUniformSumM g)

-- | Select a random item from a non-empty list in constant memory, using
-- only a single traversal, using reservoir sampling.
reservoir :: StatefulGen g m => NE.NonEmpty a -> g -> m a
reservoir (x :| xs) g = go 1 x xs
  where
    go _  y []     = pure y
    go !i y (z:zs) = do
      j <- uniformWord64R i g
      if j <= 0
        then go (i + 1) z zs
        else go (i + 1) y zs
{-# INLINE reservoir #-}

