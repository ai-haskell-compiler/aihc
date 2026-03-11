{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Instances
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Vector.Instances () where

import Prelude hiding ((++), drop, length)
import Control.Applicative
import Control.Monad
import Data.Semigroup
#ifdef MIN_VERSION_hashable
import Data.Hashable (Hashable(..))
import Data.Hashable.Lifted (Hashable1(..))
#endif
import Data.Key
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Plus
import Data.Pointed
import Data.Monoid (Monoid(..))
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as G
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle as Stream
import Data.Vector.Fusion.Bundle.Size
#else
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Fusion.Stream.Size
#endif
import Data.Vector (Vector,(++),drop,length,imap,ifoldr, ifoldl, izipWith,(!?),(//), generate)
import qualified Data.Vector as Vector
#if MIN_VERSION_vector(0,13,2)
import qualified Data.Vector.Strict as Vector.Strict
#endif
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Primitive as Primitive

type instance Key Vector = Int

instance Keyed Vector where
  mapWithKey = Vector.imap
  {-# INLINE mapWithKey #-}

instance Zip Vector where
  zipWith = Vector.zipWith
  {-# INLINE zipWith #-}

instance ZipWithKey Vector where
  zipWithKey = Vector.izipWith
  {-# INLINE zipWithKey #-}

instance Indexable Vector where
  index = (Vector.!)
  {-# INLINE index #-}

instance Lookup Vector where
  lookup = flip (!?)
  {-# INLINE lookup #-}

instance Adjustable Vector where
  adjust f n v = case v !? n of
    Just a ->  v // [(n, f a)]
    Nothing -> v
  {-# INLINE adjust #-}

  replace n a v = v // [(n,a)]
  {-# INLINE replace #-}

instance FoldableWithKey Vector where
  foldrWithKey = Vector.ifoldr
  {-# INLINE foldrWithKey #-}
  foldlWithKey = Vector.ifoldl
  {-# INLINE foldlWithKey #-}

instance Apply Vector where
  fs <.> as =
    G.unstream $ Stream.sized results (Exact n)
    where
      n = Vector.length fs * Vector.length as
      results = Stream.concatMap body $ G.stream fs
      body f = Stream.map f $ G.stream as
  {-# INLINE (<.>) #-}

instance Pointed Vector where
  point = Vector.singleton
  {-# INLINE point #-}

instance Bind Vector where
  v >>- f = Vector.concatMap f v
  {-# INLINE (>>-) #-}

instance Alt Vector where
  (<!>) = (++)
  {-# INLINE (<!>) #-}

instance Plus Vector where
  zero = Vector.empty
  {-# INLINE zero #-}

instance TraversableWithKey Vector where
  traverseWithKey f v
     = Vector.fromListN (Vector.length v) <$> traverseWithKey f (Vector.toList v)
  {-# INLINE traverseWithKey #-}

instance Extend Vector where
  duplicated v = generate (length v) (`drop` v)
  {-# INLINE duplicated #-}
  extended f v = generate (length v) (\n -> f (drop n v))
  {-# INLINE extended #-}

#if !(MIN_VERSION_vector(0,12,0))
instance Semigroup (Vector a) where
  (<>) = (++)
  {-# INLINE (<>) #-}

instance Unboxed.Unbox a => Semigroup (Unboxed.Vector a) where
  (<>) = (Unboxed.++)
  {-# INLINE (<>) #-}

instance Storable.Storable a => Semigroup (Storable.Vector a) where
  (<>) = (Storable.++)
  {-# INLINE (<>) #-}

instance Primitive.Prim a => Semigroup (Primitive.Vector a) where
  (<>) = (Primitive.++)
  {-# INLINE (<>) #-}
#endif

#ifdef MIN_VERSION_hashable
instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . Vector.toList
  {-# INLINE hashWithSalt #-}

#if MIN_VERSION_vector(0,13,2)
instance (Hashable a) => Hashable (Vector.Strict.Vector a) where
  hashWithSalt salt = hashWithSalt salt . Vector.Strict.toList
  {-# INLINE hashWithSalt #-}

instance Hashable1 Vector.Strict.Vector where
  liftHashWithSalt itemHashWithSalt salt = liftHashWithSalt itemHashWithSalt salt . Vector.Strict.toList
  {-# INLINE liftHashWithSalt #-}
#endif

instance (Unboxed.Unbox a, Hashable a) => Hashable (Unboxed.Vector a) where
  hashWithSalt salt = hashWithSalt salt . Unboxed.toList
  {-# INLINE hashWithSalt #-}

instance (Storable.Storable a, Hashable a) => Hashable (Storable.Vector a) where
  hashWithSalt salt = hashWithSalt salt . Storable.toList
  {-# INLINE hashWithSalt #-}

instance (Primitive.Prim a, Hashable a) => Hashable (Primitive.Vector a) where
  hashWithSalt salt = hashWithSalt salt . Primitive.toList
  {-# INLINE hashWithSalt #-}

instance Hashable1 Vector where
  liftHashWithSalt itemHashWithSalt salt = liftHashWithSalt itemHashWithSalt salt . Vector.toList
  {-# INLINE liftHashWithSalt #-}
#endif
