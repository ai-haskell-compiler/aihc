{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Static
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Arrow transformer adding static information.

module Control.Arrow.Transformer.Static(
    StaticArrow(StaticArrow), StaticMonadArrow, StaticArrowArrow,
    wrap, unwrap, wrapA, unwrapA, wrapM, unwrapM,
    ) where

import Control.Arrow.Internals
import Control.Arrow.Operations
import Control.Arrow.Transformer

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Monoid
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

import Prelude hiding (id,(.))

-- | An arrow type that augments the underlying arrow with static information.

newtype StaticArrow f a b c = StaticArrow (f (a b c))

instance (Arrow a, Applicative f) => ArrowTransformer (StaticArrow f) a where
    lift f = StaticArrow (pure f)

instance (Category a, Applicative f) => Category (StaticArrow f a) where
    id = StaticArrow (pure id)
    StaticArrow f . StaticArrow g = StaticArrow ((.) <$> f <*> g)

instance (Arrow a, Applicative f) => Arrow (StaticArrow f a) where
    arr f = StaticArrow (pure (arr f))
    first (StaticArrow f) = StaticArrow (first <$> f)

-- The following promotions follow directly from the arrow transformer.

instance (ArrowZero a, Applicative f) => ArrowZero (StaticArrow f a) where
    zeroArrow = lift zeroArrow

instance (ArrowCircuit a, Applicative f) => ArrowCircuit (StaticArrow f a) where
    delay x = lift (delay x)

instance (ArrowError ex a, Applicative f) => ArrowError ex (StaticArrow f a) where
    raise = lift raise
    handle (StaticArrow f) (StaticArrow h) =
        StaticArrow (handle <$> f <*> h)
    tryInUnless (StaticArrow f) (StaticArrow s) (StaticArrow h) =
        StaticArrow (tryInUnless <$> f <*> s <*> h)

instance (ArrowReader r a, Applicative f) => ArrowReader r (StaticArrow f a) where
    readState = lift readState
    newReader (StaticArrow f) = StaticArrow (newReader <$> f)

instance (ArrowState s a, Applicative f) => ArrowState s (StaticArrow f a) where
    fetch = lift fetch
    store = lift store

instance (ArrowWriter w a, Applicative f) => ArrowWriter w (StaticArrow f a) where
    write = lift write
    newWriter (StaticArrow f) = StaticArrow (newWriter <$> f)

-- Classes that are preserved.

instance (ArrowChoice a, Applicative f) => ArrowChoice (StaticArrow f a) where
    left (StaticArrow f) = StaticArrow (left <$> f)

-- ArrowApply is generally not preserved.

instance (ArrowLoop a, Applicative f) => ArrowLoop (StaticArrow f a) where
    loop (StaticArrow f) = StaticArrow (loop <$> f)

instance (ArrowPlus a, Applicative f) => ArrowPlus (StaticArrow f a) where
    StaticArrow f <+> StaticArrow g = StaticArrow ((<+>) <$> f <*> g)

-- Other instances

instance (Arrow a, Applicative f) => Functor (StaticArrow f a b) where
    fmap f g = g >>> arr f

instance (Arrow a, Applicative f) => Applicative (StaticArrow f a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance (ArrowPlus a, Applicative f) => Alternative (StaticArrow f a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance (ArrowPlus a, Applicative f) => Semigroup (StaticArrow f a b c) where
    (<>) = (<+>)
#endif

instance (ArrowPlus a, Applicative f) => Monoid (StaticArrow f a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

-- promotions

instance (ArrowAddStream a a', Applicative f) =>
        ArrowAddStream (StaticArrow f a) (StaticArrow f a') where
    liftStream (StaticArrow f) = StaticArrow (liftStream <$> f)
    elimStream (StaticArrow f) = StaticArrow (elimStream <$> f)

instance (ArrowAddState s a a', Applicative f) =>
        ArrowAddState s (StaticArrow f a) (StaticArrow f a') where
    liftState (StaticArrow f) = StaticArrow (liftState <$> f)
    elimState (StaticArrow f) = StaticArrow (elimState <$> f)

instance (ArrowAddReader r a a', Applicative f) =>
        ArrowAddReader r (StaticArrow f a) (StaticArrow f a') where
    liftReader (StaticArrow f) = StaticArrow (liftReader <$> f)
    elimReader (StaticArrow f) = StaticArrow (elimReader <$> f)

instance (ArrowAddWriter w a a', Applicative f) =>
        ArrowAddWriter w (StaticArrow f a) (StaticArrow f a') where
    liftWriter (StaticArrow f) = StaticArrow (liftWriter <$> f)
    elimWriter (StaticArrow f) = StaticArrow (elimWriter <$> f)

instance (ArrowAddError ex a a', Applicative f) =>
        ArrowAddError ex (StaticArrow f a) (StaticArrow f a') where
    liftError (StaticArrow f) = StaticArrow (liftError <$> f)
    elimError (StaticArrow f) (StaticArrow h) =
        StaticArrow (elimError <$> f <*> h)

wrap :: (Applicative f, Arrow a) => f (a b c) -> StaticArrow f a b c
wrap = StaticArrow

unwrap :: (Applicative f, Arrow a) => StaticArrow f a b c -> f (a b c)
unwrap (StaticArrow f) = f

-- | A special case.

type StaticArrowArrow a s = StaticArrow (WrappedArrow a s)

wrapA :: (Arrow a, Arrow a') => a s (a' b c) -> StaticArrowArrow a s a' b c
wrapA x = StaticArrow (WrapArrow x)

unwrapA :: (Arrow a, Arrow a') => StaticArrowArrow a s a' b c -> a s (a' b c)
unwrapA (StaticArrow (WrapArrow x)) = x

-- | A special case is monads applied to the whole arrow, in contrast to
-- 'Kleisli' arrows, in which the monad is applied to the output.

type StaticMonadArrow m = StaticArrow (WrappedMonad m)

wrapM :: (Monad m, Arrow a) => m (a b c) -> StaticMonadArrow m a b c
wrapM x = StaticArrow (WrapMonad x)

unwrapM :: (Monad m, Arrow a) => StaticMonadArrow m a b c -> m (a b c)
unwrapM (StaticArrow (WrapMonad x)) = x
