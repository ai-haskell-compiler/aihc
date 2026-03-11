{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.Instances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functor instances morphed functions
-- are restricted to products.
module Data.Functor.ProductIsomorphic.Instances (
  WrappedFunctor (..),
  WrappedAlter (..),
  ) where

import Data.Monoid (Monoid, mempty, (<>))
import Control.Applicative
  ((<$>), Applicative, pure, (<*>),
   Alternative, empty, (<|>),
   Const (..))

import Data.Functor.ProductIsomorphic.Class
  (ProductIsoFunctor(..), ProductIsoApplicative (..),
   ProductIsoAlternative (..), ProductIsoEmpty (..))


instance ProductIsoFunctor (Const a) where
  _ |$| Const a = Const a
  {-# INLINABLE (|$|) #-}

instance Monoid a => ProductIsoApplicative (Const a) where
  pureP _ = Const mempty
  {-# INLINABLE pureP #-}
  Const a |*| Const b = Const $ a <> b
  {-# INLINABLE (|*|) #-}

instance Monoid a => ProductIsoEmpty (Const a) () where
  pureE = pureP ()
  {-# INLINABLE pureE #-}
  peRight (Const a) = Const a
  {-# INLINABLE peRight #-}
  peLeft (Const a) = Const a
  {-# INLINABLE peLeft #-}


-- | Wrapped functor type to make instances of product-iso functors.
newtype WrappedFunctor f a = WrapFunctor { unwrapFunctor :: f a }

instance Functor f => ProductIsoFunctor (WrappedFunctor f) where
  f |$| fa = WrapFunctor $ f <$> unwrapFunctor fa
  {-# INLINABLE (|$|) #-}

instance Applicative f => ProductIsoApplicative (WrappedFunctor f) where
  pureP = WrapFunctor . pure
  {-# INLINABLE pureP #-}
  WrapFunctor ff |*| WrapFunctor fa = WrapFunctor $ ff <*> fa
  {-# INLINABLE (|*|) #-}

instance Alternative f => ProductIsoAlternative (WrappedFunctor f) where
  emptyP = WrapFunctor empty
  {-# INLINABLE emptyP #-}
  WrapFunctor fa1 ||| WrapFunctor fa2 = WrapFunctor $ fa1 <|> fa2
  {-# INLINABLE (|||) #-}

instance Applicative f => ProductIsoEmpty (WrappedFunctor f) () where
  pureE   = pureP ()
  {-# INLINABLE pureE #-}
  peRight = WrapFunctor . fmap fst . unwrapFunctor
  {-# INLINABLE peRight #-}
  peLeft  = WrapFunctor . fmap snd . unwrapFunctor
  {-# INLINABLE peLeft #-}


-- | Wrapped Const Alternative objects to make instances like Const functor.
newtype WrappedAlter f a b = WrapAlter { unWrapAlter :: Const (f a) b }

instance ProductIsoFunctor (WrappedAlter f a) where
  _ |$| WrapAlter (Const fa) = WrapAlter $ Const fa
  {-# INLINABLE (|$|) #-}

instance Alternative f => ProductIsoApplicative (WrappedAlter f a) where
  pureP _ = WrapAlter $ Const empty
  {-# INLINABLE pureP #-}
  WrapAlter (Const a) |*| WrapAlter (Const b) = WrapAlter $ Const $ a <|> b
  {-# INLINABLE (|*|) #-}

instance Alternative f => ProductIsoEmpty (WrappedAlter f a) () where
  pureE   = pureP ()
  {-# INLINABLE pureE #-}
  peRight = WrapAlter . fmap fst . unWrapAlter
  {-# INLINABLE peRight #-}
  peLeft  = WrapAlter . fmap snd . unWrapAlter
