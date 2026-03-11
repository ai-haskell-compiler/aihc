{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Writer
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Arrow transformer that adds accumulation of output.

module Control.Arrow.Transformer.Writer(
    WriterArrow(WriterArrow),
    runWriter,
    ArrowAddWriter(..),
    ) where

import Control.Arrow.Internals
import Control.Arrow.Operations
import Control.Arrow.Transformer

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Monoid
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

import Prelude hiding (id,(.))

-- | An arrow type that augments an existing arrow with accumulating
-- output.  The 'ArrowWriter' class contains the relevant operations.

newtype WriterArrow w a b c = WriterArrow (a b (c, w))

-- | Encapsulation of a writer computation, providing the accumulated output.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> do
-- >        ...
-- >        (result, output) <- (|runWriter cmd|)

runWriter :: (Arrow a, Monoid w) => WriterArrow w a e b -> a e (b,w)
runWriter (WriterArrow f) = f

rstrength :: ((a, w), b) -> ((a, b), w)
rstrength ((a, w), b) = ((a, b), w)

unit :: Monoid w => a -> (a, w)
unit a = (a, mempty)

join :: Monoid w => ((a, w), w) -> (a, w)
join ((a, w2), w1) = (a, w1 `mappend` w2)

-- arrow transformer

instance (Arrow a, Monoid w) => ArrowTransformer (WriterArrow w) a where
    lift f = WriterArrow (f >>> arr unit)

-- liftings of standard classes

instance (Arrow a, Monoid w) => Category (WriterArrow w a) where
    id = WriterArrow (arr unit)
    WriterArrow f . WriterArrow g =
        WriterArrow (arr join . first f . g)

instance (Arrow a, Monoid w) => Arrow (WriterArrow w a) where
    arr f = WriterArrow (arr (unit . f))
    first (WriterArrow f) = WriterArrow (first f >>> arr rstrength)

instance (ArrowChoice a, Monoid w) => ArrowChoice (WriterArrow w a) where
    left (WriterArrow f) = WriterArrow (left f >>> arr lift_monoid)
      where
        lift_monoid (Left (x, w)) = (Left x, w)
        lift_monoid (Right y) = unit (Right y)

instance (ArrowApply a, Monoid w) => ArrowApply (WriterArrow w a) where
    app = WriterArrow (arr (\(WriterArrow f, x) -> (f, x)) >>> app)

instance (ArrowZero a, Monoid w) => ArrowZero (WriterArrow w a) where
    zeroArrow = WriterArrow zeroArrow

instance (ArrowPlus a, Monoid w) => ArrowPlus (WriterArrow w a) where
    WriterArrow f <+> WriterArrow g = WriterArrow (f <+> g)

instance (ArrowLoop a, Monoid w) => ArrowLoop (WriterArrow w a) where
    loop (WriterArrow f) = WriterArrow (loop (f >>> arr swapenv))
      where
        swapenv ~(~(x, y), w) = ((x, w), y)

-- Other instances

instance (Arrow a, Monoid w) => Functor (WriterArrow w a b) where
    fmap f g = g >>> arr f

instance (Arrow a, Monoid w) => Applicative (WriterArrow w a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance (ArrowPlus a, Monoid w) => Alternative (WriterArrow w a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance (ArrowPlus a, Monoid w) => Semigroup (WriterArrow w a b c) where
    (<>) = (<+>)
#endif

instance (ArrowPlus a, Monoid w) => Monoid (WriterArrow w a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

-- new instances

instance (Arrow a, Monoid w) => ArrowWriter w (WriterArrow w a) where
    write = WriterArrow (arr (\x -> ((), x)))
    newWriter (WriterArrow f) =
        WriterArrow (f >>> arr (\(x, w) -> ((x, w), w)))

instance (Arrow a, Monoid w) => ArrowAddWriter w (WriterArrow w a) a where
    liftWriter = lift
    elimWriter = runWriter

-- liftings of other classes

instance (ArrowCircuit a, Monoid w) => ArrowCircuit (WriterArrow w a) where
    delay x = lift (delay x)

instance (ArrowError ex a, Monoid w) => ArrowError ex (WriterArrow w a) where
    raise = lift raise
    handle (WriterArrow f) (WriterArrow h) = WriterArrow (handle f h)
    tryInUnless (WriterArrow f) (WriterArrow s) (WriterArrow h) =
        WriterArrow (tryInUnless f s' h)
      where
        s' = arr lstrength >>> first s >>> arr join
        lstrength (x, (y, w)) = ((x, y), w)
    newError (WriterArrow f) = WriterArrow (newError f >>> arr h)
      where
        h (Left ex) = unit (Left ex)
        h (Right (c, w)) = (Right c, w)

instance (ArrowReader r a, Monoid w) => ArrowReader r (WriterArrow w a) where
    readState = lift readState
    newReader (WriterArrow f) = WriterArrow (newReader f)

instance (ArrowState s a, Monoid w) => ArrowState s (WriterArrow w a) where
    fetch = lift fetch
    store = lift store

-- promotions of encapsulation operators

instance (ArrowAddError ex a a', Monoid w) =>
        ArrowAddError ex (WriterArrow w a) (WriterArrow w a') where
    liftError (WriterArrow f) = WriterArrow (liftError f)
    elimError (WriterArrow f) (WriterArrow h) = WriterArrow (elimError f h)

instance (ArrowAddReader r a a', Monoid w) =>
        ArrowAddReader r (WriterArrow w a) (WriterArrow w a') where
    liftReader (WriterArrow f) = WriterArrow (liftReader f)
    elimReader (WriterArrow f) = WriterArrow (elimReader f)

instance (ArrowAddState s a a', Monoid w) =>
        ArrowAddState s (WriterArrow w a) (WriterArrow w a') where
    liftState (WriterArrow f) = WriterArrow (liftState f)
    elimState (WriterArrow f) = WriterArrow (elimState f >>> arr rstrength)
