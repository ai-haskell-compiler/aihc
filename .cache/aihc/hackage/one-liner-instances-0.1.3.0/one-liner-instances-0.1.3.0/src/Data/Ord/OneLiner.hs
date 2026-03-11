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
-- Module      : Data.Ord.OneLiner
-- Description : Derived methods for Semigroup.
-- Copyright   : (c) Justin Le 2021
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for 'Eq' and 'Ord', using "Generics.OneLiner" and
-- "GHC.Generics".
--
-- Can be used for any types (deriving 'Generic') where every field is an
-- instance of 'Eq' (or 'Ord').
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Eq' and 'Ord' instances, which can one day be used with
-- /DerivingVia/ syntax to derive instances automatically.
--

module Data.Ord.OneLiner (
  -- * Newtype wrapper
    GOrd(..)
  -- * Generics-derived methods
  -- ** Eq
  , gEquals
  , gNotEquals
  -- ** Ord
  , gCompare
  , gLTE
  , gLT
  , gGTE
  , gGT
  , gMax
  , gMin
  ) where

import           Data.Coerce
import           Data.Data
import           Data.Monoid
import           GHC.Generics
import           Generics.OneLiner

-- | If @a@ is a data type whose fields are all instances of 'Eq', then
-- @'GOrd' a@ has a 'Eq' instance.
--
-- If @a@ is a data type whose fields are all instances of 'Ord', then
-- @'GOrd' a@ has a 'Ord' instance.
--
-- Will one day be able to be used with /DerivingVia/ syntax, to derive
-- instances automatically.
--
newtype GOrd a = GOrd { getGOrd :: a }
  deriving (Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance ( ADT a
         , Constraints a Eq
         )
      => Eq (GOrd a) where
    (==) = coerce (gEquals @a)
    {-# INLINE (==) #-}
    (/=) = coerce (gNotEquals @a)
    {-# INLINE (/=) #-}

instance ( ADT a
         , Constraints a Eq
         , Constraints a Ord
         )
      => Ord (GOrd a) where
    compare = coerce (gCompare @a)
    {-# INLINE compare #-}
    (<=)    = coerce (gLTE @a)
    {-# INLINE (<=) #-}
    (<)     = coerce (gLT @a)
    {-# INLINE (<) #-}
    (>=)    = coerce (gGTE @a)
    {-# INLINE (>=) #-}
    (>)     = coerce (gGT @a)
    {-# INLINE (>) #-}
    max     = coerce (gMax @a)
    {-# INLINE max #-}
    min     = coerce (gMin @a)
    {-# INLINE min #-}

-- | '==' implemented by using '==' between all of the
-- components, lexicographically.  First compares constructors.
gEquals
    :: forall a. (ADT a, Constraints a Eq)
    => a -> a -> Bool
gEquals x y = ctorIndex x == ctorIndex y
           && getAll (mzipWith @Eq (\x' -> All . (== x')) x y)
{-# INLINE gEquals #-}

-- | '/=' implemented by using '/=' between all of the
-- components, lexicographically.  First compares constructors.
gNotEquals
    :: forall a. (ADT a, Constraints a Eq)
    => a -> a -> Bool
gNotEquals x y = ctorIndex x /= ctorIndex y
              || getAny (mzipWith @Eq (\x' -> Any . (/= x')) x y)
{-# INLINE gNotEquals #-}

-- | 'compare' implemented by using 'compare' between all of the
-- components, lexicographically.  First compares constructors.
gCompare
    :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> Ordering
gCompare x y = compare (ctorIndex x) (ctorIndex y)
            <> mzipWith @Ord compare x y
{-# INLINE gCompare #-}

-- | '<=' implemented by using '<=' between all of the components.  First
-- compares constructors.
gLTE
    :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> Bool
gLTE x y = not $ gGT x y
{-# INLINE gLTE #-}

-- | '<' implemented by using '<' between all of the components.  First
-- compares constructors.
gLT :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> Bool
gLT x y = ctorIndex x < ctorIndex y
       || getAny (mzipWith @Ord (\x' -> Any . (x' <)) x y)
{-# INLINE gLT #-}

-- | '>=' implemented by using '>=' between all of the components.  First
-- compares constructors.
gGTE
    :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> Bool
gGTE x y = not $ gLT x y
{-# INLINE gGTE #-}

-- | '>' implemented by using '>' between all of the components.  First
-- compares constructors.
gGT :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> Bool
gGT x y = ctorIndex x > ctorIndex y
       || getAny (mzipWith @Ord (\x' -> Any . (x' >)) x y)
{-# INLINE gGT #-}

-- | 'max' implemented by using 'max' between all of the components.  First
-- compares constructors.  If two items are equal, returns the second.
gMax
    :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> a
gMax x y | gLTE x y  = y
         | otherwise = x
{-# INLINE gMax #-}

-- | 'min' implemented by using 'min' between all of the components.  First
-- compares constructors.  If two items are equal, returns the first.
gMin
    :: forall a. (ADT a, Constraints a Ord)
    => a -> a -> a
gMin x y | gLTE x y  = x
         | otherwise = y
{-# INLINE gMin #-}

