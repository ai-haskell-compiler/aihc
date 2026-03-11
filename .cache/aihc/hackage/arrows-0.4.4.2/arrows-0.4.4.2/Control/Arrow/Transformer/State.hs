{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.State
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- An arrow transformer that adds a modifiable state,
-- based of section 9 of /Generalising Monads to Arrows/, by John Hughes,
-- /Science of Computer Programming/ 37:67-111, May 2000.

module Control.Arrow.Transformer.State(
    StateArrow(StateArrow),
    runState,
    ArrowAddState(..),
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

-- | An arrow type that augments an existing arrow with a modifiable
-- state.  The 'ArrowState' class contains the operations on this state.

newtype StateArrow s a b c = StateArrow (a (b, s) (c, s))

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), z) = ((x, z), y)

instance Category a => Category (StateArrow s a) where
    id = StateArrow id
    StateArrow f . StateArrow g = StateArrow (f . g)

instance Arrow a => Arrow (StateArrow s a) where
    arr f = StateArrow (arr (\(x, s) -> (f x, s)))
    first (StateArrow f) =
        StateArrow (arr swapsnd >>> first f >>> arr swapsnd)

instance Arrow a => ArrowTransformer (StateArrow s) a where
    lift f = StateArrow (first f)

-- | Encapsulation of a state-using computation, exposing the initial
-- and final states.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> do
-- >        ...
-- >        (result, final_state) <- (|runState cmd|) init_state

runState :: Arrow a => StateArrow s a e b -> a (e,s) (b,s)
runState (StateArrow f) = f

-- operations

instance Arrow a => ArrowState s (StateArrow s a) where
    fetch = StateArrow (arr (\(_, s) -> (s, s)))
    store = StateArrow (arr (\(s, _) -> ((), s)))

instance Arrow a => ArrowAddState s (StateArrow s a) a where
    liftState = lift
    elimState = runState

-- The following promotions follow directly from the arrow transformer.

instance ArrowZero a => ArrowZero (StateArrow s a) where
    zeroArrow = StateArrow zeroArrow

instance ArrowCircuit a => ArrowCircuit (StateArrow s a) where
    delay x = lift (delay x)

instance ArrowError ex a => ArrowError ex (StateArrow s a) where
    raise = lift raise
    handle (StateArrow f) (StateArrow h) =
        StateArrow (handle f (arr swapsnd >>> h))
    tryInUnless (StateArrow f) (StateArrow s) (StateArrow h) =
        StateArrow (tryInUnless f (arr new_state >>> s) (arr swapsnd >>> h))
      where
        new_state ((b,_),(c,s')) = ((b,c),s')
    newError (StateArrow f) = StateArrow (newError f &&& arr snd >>> arr h)
      where
        h (Left ex, s) = (Left ex, s)
        h (Right (c, s'), _) = (Right c, s')

-- Note that in each case the error handler gets the original state.

instance ArrowReader r a => ArrowReader r (StateArrow s a) where
    readState = lift readState
    newReader (StateArrow f) = StateArrow (arr swapsnd >>> newReader f)

instance ArrowWriter w a => ArrowWriter w (StateArrow s a) where
    write = lift write
    newWriter (StateArrow f) = StateArrow (newWriter f >>> arr swapsnd)

-- liftings of standard classes

instance ArrowChoice a => ArrowChoice (StateArrow s a) where
    left (StateArrow f) = StateArrow (arr distr >>> left f >>> arr undistr)
      where
        distr (Left y, s) = Left (y, s)
        distr (Right z, s) = Right (z, s)
        undistr (Left (y, s)) = (Left y, s)
        undistr (Right (z, s)) = (Right z, s)

instance ArrowApply a => ArrowApply (StateArrow s a) where
    app = StateArrow (arr (\((StateArrow f, x), s) -> (f, (x, s))) >>> app)

instance ArrowLoop a => ArrowLoop (StateArrow s a) where
    loop (StateArrow f) =
        StateArrow (loop (arr swapsnd >>> f >>> arr swapsnd))

instance ArrowPlus a => ArrowPlus (StateArrow s a) where
    StateArrow f <+> StateArrow g = StateArrow (f <+> g)

-- Other instances

instance Arrow a => Functor (StateArrow s a b) where
    fmap f g = g >>> arr f

instance Arrow a => Applicative (StateArrow s a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance ArrowPlus a => Alternative (StateArrow s a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance ArrowPlus a => Semigroup (StateArrow s a b c) where
    (<>) = (<+>)
#endif

instance ArrowPlus a => Monoid (StateArrow s a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

-- promotions

instance ArrowAddReader r a a' =>
        ArrowAddReader r (StateArrow s a) (StateArrow s a') where
    liftReader (StateArrow f) = StateArrow (liftReader f)
    elimReader (StateArrow f) = StateArrow (arr swapsnd >>> elimReader f)

instance ArrowAddWriter w a a' =>
        ArrowAddWriter w (StateArrow s a) (StateArrow s a') where
    liftWriter (StateArrow f) = StateArrow (liftWriter f)
    elimWriter (StateArrow f) = StateArrow (elimWriter f >>> arr swapsnd)

instance ArrowAddError ex a a' =>
        ArrowAddError ex (StateArrow s a) (StateArrow s a') where
    liftError (StateArrow f) = StateArrow (liftError f)
    elimError (StateArrow f) (StateArrow h) =
        StateArrow (elimError f (arr swapsnd >>> h))
