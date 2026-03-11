{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.Class
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functor interfaces which morphed functions
-- are restricted to products.
module Data.Functor.ProductIsomorphic.Class (
  -- * ProductIso classes
  ProductIsoFunctor (..),
  ProductIsoApplicative (..),
  ProductIsoAlternative (..),

  -- * Empty element
  ProductIsoEmpty (..),
  peRightR, peLeftR,

  --- (<|), (|>),
  ) where

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor)
import Data.Functor.ProductIsomorphic.TupleInstances ()

-- | Restricted functor on products.
class ProductIsoFunctor f where
  (|$|) :: ProductConstructor (a -> b) => (a -> b) -> f a -> f b

-- | Restricted applicative functor on products.
class ProductIsoFunctor f => ProductIsoApplicative f where
  pureP :: ProductConstructor a => a -> f a
  (|*|) :: f (a -> b) -> f a -> f b

-- | Restricted alternative on products.
class ProductIsoApplicative f => ProductIsoAlternative f where
  emptyP :: f a
  (|||)  :: f a -> f a -> f a

infixl 4 |$|, |*|
infixl 3 |||

-- | Empty element of product operator
class ProductIsoApplicative f => ProductIsoEmpty f e where
  pureE   :: f e
  peRight :: f (a, e) -> f a
  peLeft  :: f (e, a) -> f a

-- | peRight and peRightR should have isomorphic law.
-- @
--   peRight . peRightR == peRightR . peRight == id
-- @
peRightR :: ProductIsoEmpty f e
        => f a
        -> f (a, e)
peRightR p = (,) |$| p |*| pureE
{-# INLINABLE peRightR #-}

-- | peLeft and peLeftR should have isomorphic law.
-- @
--   peLeft . peLeftR == peLeftR . peLeft == id
-- @
peLeftR :: ProductIsoEmpty f e
       => f a
       -> f (e, a)
peLeftR p = (,) |$| pureE |*| p
{-# INLINABLE peLeftR #-}

{-
(<|) :: ProductIsoEmpty f e => f a -> f e -> f a
p <| e = peRight $ (,) |$| p |*| e
{-# INLINABLE (<|) #-}

(|>) :: ProductIsoEmpty f e => f e -> f a -> f a
e |> p = peLeft $ (,) |$| e |*| p
{-# INLINABLE (|>) #-}

infixl 4 <|, |>
 -}
