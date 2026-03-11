{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.CoState
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Transformation of state readers.
--
-- /TODO:/ define operations for this arrow.

module Control.Arrow.Transformer.CoState(
    CoStateArrow(CoStateArrow),
    ) where

import Control.Applicative
import Control.Arrow
import Control.Category
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Monoid

import Prelude hiding (id,(.))

newtype CoStateArrow s a b c = CoStateArrow (a (s -> b) (s -> c))

instance Category a => Category (CoStateArrow s a) where
    id = CoStateArrow id
    CoStateArrow f . CoStateArrow g = CoStateArrow (f . g)

instance Arrow a => Arrow (CoStateArrow s a) where
    arr f = CoStateArrow (arr (f .))
    first (CoStateArrow f) =
        CoStateArrow (arr unzipMap >>> first f >>> arr zipMap)

zipMap :: (s -> a, s -> b) -> (s -> (a,b))
zipMap h s = (fst h s, snd h s)

unzipMap :: (s -> (a,b)) -> (s -> a, s -> b)
unzipMap h = (fst . h, snd . h)

-- there is no transformer

-- promotions of standard classes

instance ArrowLoop a => ArrowLoop (CoStateArrow s a) where
    loop (CoStateArrow f) =
        CoStateArrow (loop (arr zipMap >>> f >>> arr unzipMap))

instance ArrowZero a => ArrowZero (CoStateArrow s a) where
    zeroArrow = CoStateArrow zeroArrow

instance ArrowPlus a => ArrowPlus (CoStateArrow s a) where
    CoStateArrow f <+> CoStateArrow g = CoStateArrow (f <+> g)

-- Other instances

instance Arrow a => Functor (CoStateArrow s a b) where
    fmap f g = g >>> arr f

instance Arrow a => Applicative (CoStateArrow s a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance ArrowPlus a => Alternative (CoStateArrow s a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance ArrowPlus a => Semigroup (CoStateArrow s a b c) where
    (<>) = (<+>)
#endif

instance ArrowPlus a => Monoid (CoStateArrow s a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif
