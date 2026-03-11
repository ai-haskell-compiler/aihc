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
-- Module      : System.Random.OneLiner
-- Description : Derived methods for Random.
-- Copyright   : (c) Justin Le 2021
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Random', using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Random'.
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Random' instances, which can one day be used with /DerivingVia/
-- syntax to derive instances automatically.
--
-- @since 0.1.2.1

module System.Random.OneLiner (
  -- * Single constructor
  -- ** Newtype wrapper
    GRandom(..)
  -- ** Generics-derived methods
  , gRandomR
  , gRandom
  , gRandomRs
  , gRandoms
  , gRandomRIO
  , gRandomIO
  -- * Multiple constructor
  -- ** Newtype wrapper
  , GRandomSum(..)
  -- ** Generics-derived methods
  , gRandomRSum
  , gRandomSum
  , gRandomRSums
  , gRandomSums
  , gRandomRIOSum
  , gRandomIOSum
  ) where

import           Control.Monad
import           Data.Coerce
import           Data.Data
import           Data.Functor.Compose
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Maybe
import           GHC.Exts             (build)
import           GHC.Generics
import           Generics.OneLiner
import           System.Random
import           System.Random.OneLiner.Internal (Pair(..), dePair, State(..))
import qualified Data.List.NonEmpty   as NE

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Random', then @'GRandom' a@ has a 'Random' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
-- Only works with data types with single constructors.  If you need it to
-- work with types of multiple constructors, consider 'GRandomSum'.
--
-- @since 0.1.2.1
newtype GRandom a = GRandom { getGRandom :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADTRecord a
         , Constraints a Random
         )
      => Random (GRandom a) where
    randomR :: forall g. RandomGen g => (GRandom a, GRandom a) -> g -> (GRandom a, g)
    randomR = coerce (gRandomR @a @g)
    {-# INLINE randomR #-}
    random :: forall g. RandomGen g => g -> (GRandom a, g)
    random = coerce (gRandom @a @g)
    {-# INLINE random #-}
    randomRs :: forall g. RandomGen g => (GRandom a, GRandom a) -> g -> [GRandom a]
    randomRs = coerce (gRandomRs @a @g)
    {-# INLINE randomRs #-}
    randoms :: forall g. RandomGen g => g -> [GRandom a]
    randoms = coerce (gRandoms @a @g)
    {-# INLINE randoms #-}

-- | 'randomR' implemented by sequencing 'randomR' between all components
--
-- Requires the type to have only a single constructor.
--
-- @since 0.1.2.1
gRandomR
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> (a, g)
gRandomR (l, u) = runState $
    dialgebra @Random
      (State . randomR . dePair)
      (Pair l u)
{-# INLINE gRandomR #-}

-- | 'random' implemented by sequencing 'random' for all components.
--
-- Requires the type to have only a single constructor.
--
-- @since 0.1.2.1
gRandom
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => g -> (a, g)
gRandom = runState $ createA' @Random (State random)
{-# INLINE gRandom #-}

-- | 'randomRs' implemented by repeatedly calling 'gRandomR'.
--
-- @since 0.1.2.1
gRandomRs
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> [a]
gRandomRs ival g = build (\cons _nil -> buildRandoms cons (gRandomR ival) g)
{-# INLINE gRandomRs #-}

-- | 'randoms' implemented by repeatedly calling 'gRandom'.
--
-- @since 0.1.2.1
gRandoms
    :: forall a g. (ADTRecord a, Constraints a Random, RandomGen g)
    => g -> [a]
gRandoms g = build (\cons _nil -> buildRandoms cons gRandom g)
{-# INLINE gRandoms #-}

-- | 'randomRIO' implemented by calling 'gRandomR' on the global seed.
--
-- @since 0.1.2.1
gRandomRIO
    :: forall a. (ADTRecord a, Constraints a Random)
    => (a, a) -> IO a
gRandomRIO range = getStdRandom (gRandomR range)
{-# INLINE gRandomRIO #-}

-- | 'randomIO' implemented by calling 'gRandom' on the global seed.
--
-- @since 0.1.2.1
gRandomIO
    :: forall a. (ADTRecord a, Constraints a Random)
    => IO a
gRandomIO = getStdRandom gRandom
{-# INLINE gRandomIO #-}

-- | If @a@ is a data type whose fields are all instances of 'Random', then
-- @'GRandom' a@ has a 'Random' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
-- A version of 'GRandom' that works for data types with multiple
-- constructors.  If your type has only one constructor, it might be more
-- performant to use 'GRandom'.
--
-- Note that the "ranged" variants are partial: if given a range of items
-- made with different constructors, will be 'error'!
--
-- @since 0.1.2.1
newtype GRandomSum a = GRandomSum { getGRandomSum :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADT a
         , Constraints a Random
         )
      => Random (GRandomSum a) where
    randomR :: forall g. RandomGen g => (GRandomSum a, GRandomSum a) -> g -> (GRandomSum a, g)
    randomR = coerce (gRandomRSum @a @g)
    {-# INLINE randomR #-}
    random :: forall g. RandomGen g => g -> (GRandomSum a, g)
    random = coerce (gRandomSum @a @g)
    {-# INLINE random #-}
    randomRs :: forall g. RandomGen g => (GRandomSum a, GRandomSum a) -> g -> [GRandomSum a]
    randomRs = coerce (gRandomRSums @a @g)
    {-# INLINE randomRs #-}
    randoms :: forall g. RandomGen g => g -> [GRandomSum a]
    randoms = coerce (gRandomSums @a @g)
    {-# INLINE randoms #-}

-- | 'randomR' implemented by sequencing 'randomR' between all components.
--
-- If given a range of items made with different constructors, will be
-- 'error'!
--
-- @since 0.1.2.1
gRandomRSum
    :: forall a g. (ADT a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> (a, g)
gRandomRSum (l, u) = runState . fromMaybe (error badbad) . getCompose $
    zipWithA @Random (\l' u' -> Compose (Just (State (randomR (l', u')))))
      l u
  where
    badbad = "gRandomRSum: Constructors in range do not match."
{-# INLINE gRandomRSum #-}

-- | 'random' implemented by selecting a random constructor and sequencing
-- 'random' for all components.
--
-- @since 0.1.2.1
gRandomSum
    :: forall a g. (ADT a, Constraints a Random, RandomGen g)
    => g -> (a, g)
gRandomSum = case options of
    Nothing   -> (error "gRandomSum: Uninhabited type",)
    Just opts -> runState (join (reservoir opts))
  where
    options = NE.nonEmpty . getCompose $ createA @Random @a $
        Compose [State random]
{-# INLINE gRandomSum #-}

-- | 'randomRs' implemented by repeatedly calling 'gRandomRSum'.
--
-- If given a range of items made with different constructors, will be
-- 'error'!
--
-- @since 0.1.2.1
gRandomRSums
    :: forall a g. (ADT a, Constraints a Random, RandomGen g)
    => (a, a) -> g -> [a]
gRandomRSums ival g = build (\cons _nil -> buildRandoms cons (gRandomRSum ival) g)
{-# INLINE gRandomRSums #-}

-- | 'randoms' implemented by repeatedly calling 'gRandomSum'.
--
-- @since 0.1.2.1
gRandomSums
    :: forall a g. (ADT a, Constraints a Random, RandomGen g)
    => g -> [a]
gRandomSums g = build (\cons _nil -> buildRandoms cons gRandomSum g)
{-# INLINE gRandomSums #-}

-- | 'randomRIO' implemented by calling 'gRandomRSum' on the global seed.
--
-- If given a range of items made with different constructors, will be
-- 'error'!
--
-- @since 0.1.2.1
gRandomRIOSum
    :: forall a. (ADT a, Constraints a Random)
    => (a, a) -> IO a
gRandomRIOSum range = getStdRandom (gRandomRSum range)
{-# INLINE gRandomRIOSum #-}

-- | 'randomIO' implemented by calling 'gRandom' on the global seed.
gRandomIOSum
    :: forall a. (ADT a, Constraints a Random)
    => IO a
gRandomIOSum = getStdRandom gRandomSum
{-# INLINE gRandomIOSum #-}


buildRandoms :: RandomGen g
             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
             -> (g -> (a,g))     -- ^ E.g. 'random'
             -> g                -- ^ A 'RandomGen' instance
             -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = x `seq` (x `cons` go g') where (x,g') = rand g
{-# INLINE buildRandoms #-}

-- | Select a random item from a non-empty list in constant memory, using
-- only a single traversal, using reservoir sampling.
reservoir :: RandomGen g => NE.NonEmpty a -> State g a
reservoir (x :| xs) = go 2 x xs
  where
    go _  y []     = pure y
    go !i y (z:zs) = do
      j <- State $ randomR @Int (1, i)
      if j <= 1
        then go (i + 1) z zs
        else go (i + 1) y zs
{-# INLINE reservoir #-}
