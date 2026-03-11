{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Reader
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Arrow transformer that adds a read-only state (i.e. an environment).

module Control.Arrow.Transformer.Reader(
    ReaderArrow(ReaderArrow),
    runReader,
    ArrowAddReader(..),
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

-- | An arrow type that augments an existing arrow with a read-only state
-- (or environment).  The 'ArrowReader' class contains the operations
-- on this state.

newtype ReaderArrow r a b c = ReaderArrow (a (b, r) c)

-- | Encapsulation of a state-reading computation, taking a value for the
-- state.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> ...
-- >        (|runReader cmd|) env

runReader :: Arrow a => ReaderArrow r a e b -> a (e,r) b
runReader (ReaderArrow f) = f

-- arrow transformer

instance Arrow a => ArrowTransformer (ReaderArrow r) a where
    lift f = ReaderArrow (arr fst >>> f)

-- liftings of standard classes

instance Arrow a => Category (ReaderArrow r a) where
    id = ReaderArrow (arr fst)
    ReaderArrow f . ReaderArrow g = ReaderArrow (f . first g . arr dupenv)
      where
        dupenv (a, r) = ((a, r), r)

instance Arrow a => Arrow (ReaderArrow r a) where
    arr f = ReaderArrow (arr (f . fst))
    first (ReaderArrow f) = ReaderArrow (arr swapsnd >>> first f)

swapsnd :: ((a, r), b) -> ((a, b), r)
swapsnd ~(~(a, r), b) = ((a, b), r)

instance ArrowChoice a => ArrowChoice (ReaderArrow r a) where
    left (ReaderArrow f) = ReaderArrow (arr dist' >>> left f)
      where
        dist' :: (Either b c, r) -> Either (b, r) c
        dist' (Left b, r) = Left (b, r)
        dist' (Right c, _) = Right c

instance ArrowApply a => ArrowApply (ReaderArrow r a) where
    app = ReaderArrow
        (arr (\((ReaderArrow f, a), r) -> (f, (a, r))) >>> app)

instance ArrowZero a => ArrowZero (ReaderArrow r a) where
    zeroArrow = lift zeroArrow

instance ArrowPlus a => ArrowPlus (ReaderArrow r a) where
    ReaderArrow f <+> ReaderArrow g = ReaderArrow (f <+> g)

instance ArrowLoop a => ArrowLoop (ReaderArrow r a) where
    loop (ReaderArrow f) = ReaderArrow (loop (arr swapsnd >>> f))

-- new instances

instance Arrow a => ArrowReader r (ReaderArrow r a) where
    readState = ReaderArrow (arr snd)
    newReader (ReaderArrow f) = ReaderArrow (arr fst >>> f)

instance Arrow a => ArrowAddReader r (ReaderArrow r a) a where
    liftReader = lift
    elimReader = runReader

-- liftings of other classes

instance ArrowCircuit a => ArrowCircuit (ReaderArrow r a) where
    delay x = lift (delay x)

instance ArrowError ex a => ArrowError ex (ReaderArrow r a) where
    raise = lift raise
    handle (ReaderArrow f) (ReaderArrow h) =
        ReaderArrow (handle f (arr swapsnd >>> h))
    tryInUnless (ReaderArrow f) (ReaderArrow s) (ReaderArrow h) =
        ReaderArrow (tryInUnless f (arr swapsnd >>> s) (arr swapsnd >>> h))
    newError (ReaderArrow f) = ReaderArrow (newError f)

instance ArrowState s a => ArrowState s (ReaderArrow r a) where
    fetch = lift fetch
    store = lift store

instance ArrowWriter s a => ArrowWriter s (ReaderArrow r a) where
    write = lift write
    newWriter (ReaderArrow f) = ReaderArrow (newWriter f)

-- Promotions of encapsulation operators.

instance ArrowAddError ex a a' =>
        ArrowAddError ex (ReaderArrow r a) (ReaderArrow r a') where
    liftError (ReaderArrow f) = ReaderArrow (liftError f)
    elimError (ReaderArrow f) (ReaderArrow h) =
        ReaderArrow (elimError f (arr swapsnd >>> h))

instance ArrowAddState s a a' =>
        ArrowAddState s (ReaderArrow r a) (ReaderArrow r a') where
    liftState (ReaderArrow f) = ReaderArrow (liftState f)
    elimState (ReaderArrow f) = ReaderArrow (arr swapsnd >>> elimState f)

-- instance ArrowAddReader r a a' =>
--         ArrowAddReader r (ReaderArrow r a) (ReaderArrow r a') where
--     elimReader (ReaderArrow f) = ReaderArrow (arr swapsnd >>> elimReader f)

instance ArrowAddWriter s a a' =>
        ArrowAddWriter s (ReaderArrow r a) (ReaderArrow r a') where
    liftWriter (ReaderArrow f) = ReaderArrow (liftWriter f)
    elimWriter (ReaderArrow f) = ReaderArrow (elimWriter f)

-- Other instances

instance Arrow a => Functor (ReaderArrow r a b) where
    fmap f g = g >>> arr f

instance Arrow a => Applicative (ReaderArrow r a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance ArrowPlus a => Alternative (ReaderArrow r a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance ArrowPlus a => Semigroup (ReaderArrow r a b c) where
    (<>) = (<+>)
#endif

instance ArrowPlus a => Monoid (ReaderArrow r a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif
