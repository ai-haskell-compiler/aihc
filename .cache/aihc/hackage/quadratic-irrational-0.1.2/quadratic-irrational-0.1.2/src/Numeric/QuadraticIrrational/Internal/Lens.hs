{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Numeric.QuadraticIrrational.Internal.Lens
-- Description : A tiny implementation of some lens primitives
-- Copyright   : © 2014 Johan Kiviniemi
-- License     : MIT
-- Maintainer  : Johan Kiviniemi <devel@johan.kiviniemi.name>
-- Stability   : provisional
-- Portability : RankNTypes
--
-- A tiny implementation of some lens primitives. Please see
-- <http://hackage.haskell.org/package/lens> for proper documentation.

module Numeric.QuadraticIrrational.Internal.Lens
  ( Lens, Traversal, Lens', Traversal', Getting, Setting
  , view, over, set
  ) where
import Control.Applicative (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)

type Lens      s t a b = forall f. Functor     f => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Lens'      s a = forall f. Functor     f => (a -> f a) -> s -> f s
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

type Getting r s a   = (a -> Const r a)  -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

view :: Getting a s a -> s -> a
view l s = getConst (l Const s)
{-# INLINE view #-}

over :: Setting s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (f `seq` Identity . f) s)
{-# INLINE over #-}

set :: Setting s t a b -> b -> s -> t
set l b s = over l (const b) s
{-# INLINE set #-}
