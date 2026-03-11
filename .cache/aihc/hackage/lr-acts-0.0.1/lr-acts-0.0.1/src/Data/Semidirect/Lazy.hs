{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE InstanceSigs                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Semidirect.Lazy
-- Description : Lazy semidirect products
-- Copyright   : (c) Alice Rixte 2025
-- License     : BSD 3
-- Maintainer  : alice.rixte@u-bordeaux.fr
-- Stability   : unstable
-- Portability : non-portable (GHC extensions)
--
-- Semidirect products for left and right actions.
--
-- For a strict version, see @'Data.Semidirect.Strict'@.
--
-- [Usage :]
--
-- >>> import Data.Semigroup
-- >>> LSemidirect (Sum 1) (Product 2) <> LSemidirect (Sum (3 :: Int)) (Product (4 :: Int))
-- LSemidirect {lactee = Sum {getSum = 7}, lactor = Product {getProduct = 8}}
--
-- [Property checking :]
--
-- There is a @'Semigroup'@ instance for @'LSemidirect'@ (resp. @'RSemidirect'@)
-- only if there is a @'LActSgMorph'@ (resp. @'RActSgMorph'@) instance. For
-- example, @'Sum' Int@ acting on itself is not a semigroup action by morphism
-- and therefore the semidirect product is not associative :
--
-- >>> LSemidirect (Sum 1) (Sum 2) <> LSemidirect (Sum (3 :: Int)) (Sum (4 :: Int))
-- No instance for `LActDistrib (Sum Int) (Sum Int)'
--   arising from a use of `<>'
--
-----------------------------------------------------------------------------

module Data.Semidirect.Lazy
       ( LSemidirect (..)
       , lerase
       , lforget
       , lembedActee
       , lembedActor
       , lfromPair
        , RSemidirect (..)
        , rerase
        , rforget
        , rembedActee
        , rembedActor
        , rfromPair
       ) where

import Data.Bifunctor
import Data.Act

-- | A semi-direct product for a left action, where @s@ acts on @x@
--
data LSemidirect x s = LSemidirect
  { lactee :: x -- ^ The value being acted on
  , lactor :: s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance LActSgMorph x s
  => Semigroup (LSemidirect x s) where
  ~(LSemidirect x s) <> ~(LSemidirect x' s') =
    LSemidirect  (x <> (s <>$ x')) (s <> s')

instance LActMnMorph x s => Monoid (LSemidirect x s) where
  mempty = LSemidirect mempty mempty

instance Functor (LSemidirect x) where
  fmap f a = a {lactor = f (lactor a)}

instance Bifunctor LSemidirect where
  first f a = a {lactee = f (lactee a)}
  second = fmap

-- |  Erases the actee (i.e. replace it with @mempty@).
lerase :: Monoid x => LSemidirect x s -> LSemidirect x s
lerase a = a {lactee = mempty}

-- |  Forget the actor (i.e. replace it with @mempty@).
lforget :: Monoid s => LSemidirect x s -> LSemidirect x s
lforget a =a {lactor = mempty}

-- |  Make a semidirect pair whose actee is @mempty@.
lembedActor :: Monoid x => s -> LSemidirect x s
lembedActor s = LSemidirect mempty s

-- |  Make a semidirect pair whose actor is @mempty@.
lembedActee :: Monoid s => x -> LSemidirect x s
lembedActee x = LSemidirect x mempty

-- | Converts a pair into a semidirect product element.
lfromPair :: (x,s) -> LSemidirect x s
lfromPair (x,s) = LSemidirect x s


------------------------------------------------------------------------------

-- |  A semidirect product for a right action, where @s@ acts on @x@
--
data RSemidirect x s = RSemidirect
  { ractee :: x -- ^ The value being acted on
  , ractor :: s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance RActSgMorph x s
  => Semigroup (RSemidirect x s) where
  ~(RSemidirect x s) <> ~(RSemidirect x' s') =
    RSemidirect  (x <> (x' $<> s)) (s <> s')

instance RActMnMorph x s => Monoid (RSemidirect x s) where
  mempty = RSemidirect mempty mempty

instance Functor (RSemidirect x) where
  fmap f a = a {ractor = f (ractor a)}

instance Bifunctor RSemidirect where
  first f a = a {ractee = f (ractee a)}
  second = fmap

-- |  Erase the actee (i.e. replace it with @mempty@).
rerase :: Monoid x => RSemidirect x s -> RSemidirect x s
rerase a = a {ractee = mempty}

-- |  Forget the actor (i.e. replace it with @mempty@).
rforget :: Monoid s => RSemidirect x s -> RSemidirect x s
rforget a = a {ractor = mempty}

-- |  Make a semidirect pair whose actee is @mempty@.
rembedActor :: Monoid x => s -> RSemidirect x s
rembedActor s = RSemidirect mempty s

-- |  Make a semidirect pair whose actor element is @mempty@ .
rembedActee :: Monoid s => x -> RSemidirect x s
rembedActee x = RSemidirect x mempty

-- | Convert a pair into a semidirect product element
rfromPair :: (x,s) -> RSemidirect x s
rfromPair (x,s) = RSemidirect x s
