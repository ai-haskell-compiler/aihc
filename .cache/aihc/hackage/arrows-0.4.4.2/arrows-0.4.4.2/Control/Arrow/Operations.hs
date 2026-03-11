-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Operations
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Subclasses of 'Arrow' providing additional operations.
--
-- The signatures are designed to be compatible with the proposed
-- notation for arrows, cf. <http://www.haskell.org/arrows/>.

module Control.Arrow.Operations (
    -- * Conventions
    -- $conventions

    -- * State transformers
    ArrowState(..),
    -- * State readers
    ArrowReader(..),
    -- * Monoid writers
    ArrowWriter(..),
    -- * Errors
    ArrowError(..),
    tryInUnlessDefault,
    -- * Synchronous circuits
    ArrowCircuit(..),
    ) where

import Control.Arrow
import Data.Monoid

-- $conventions
-- The arrow classes defined in this module have names like @Arrow@/Foo/,
-- and contain operations specific to such arrows.  Some of these include
-- a method @new@/Foo/, which maps computations to computations of the
-- same arrow type, but exposing some of the internals of the arrow.
--
-- Arrow transformers have names like /Bar/@Arrow@, and are
-- instances of appropriate arrow classes.  For each arrow
-- transformer, there is typically an encapsulation operator
-- @run@/Bar/ that removes that transformer from the outside of an
-- arrow type.  The 'Control.Arrow.Transformer.lift' method of the
-- 'Control.Arrow.Transformer.ArrowTransformer' class adds an arrow
-- transformer to the outside of an arrow type.
--
-- Typically a composite arrow type is built by applying a series of arrow
-- transformers to a base arrow (usually either a function arrow or a
-- 'Kleisli' arrow.  The 'Control.Arrow.Transformer.lift' method and the
-- @run@/Bar/ function operate only on the arrow transformer at the top
-- of this stack.  For more sophisticated manipulation of this stack of
-- arrow transformers, many arrow transformers provide an @ArrowAdd@/Bar/
-- class, with methods methods @lift@/Bar/ and @elim@/Bar/ to add and remove
-- the transformer anywhere in the stack.

-- | An arrow type that provides a read-only state (an environment).
-- If you also need to modify the state, use 'ArrowState'.

class Arrow a => ArrowReader r a | a -> r where
    -- | Obtain the current value of the state.
    readState :: a b r

    -- | Run a subcomputation in the same arrow, but with a different
    -- environment.  The environment of the outer computation is
    -- unaffected.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|newReader cmd|) env

    newReader :: a e b -> a (e,r) b

-- | An arrow type that provides a modifiable state,
-- based of section 9 of /Generalising Monads to Arrows/, by John Hughes,
-- /Science of Computer Programming/ 37:67-111, May 2000.

class Arrow a => ArrowState s a | a -> s where
    -- | Obtain the current value of the state.
    fetch :: a e s
    -- | Assign a new value to the state.
    store :: a s ()

-- | An arrow type that collects additional output (of some 'Monoid' type).

class (Monoid w, Arrow a) => ArrowWriter w a | a -> w where
    -- | Add a piece of additional output.
    write :: a w ()

    -- | Run a subcomputation in the same arrow, making its additional
    -- output accessible.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> do
    -- >        ...
    -- >        (value, output) <- (|newWriter cmd|)

    newWriter :: a e b -> a e (b,w)

-- | An arrow type that includes errors (or exceptions).
--
-- Minimal definition: 'raise' and 'tryInUnless'.
--
-- /TODO:/ the operations here are inconsistent with other arrow transformers.

class Arrow a => ArrowError ex a | a -> ex where
    -- | Raise an error.
    raise :: a ex b

    -- | Traditional exception construct.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        body `handle` \ex -> handler

    handle ::
        a e b           -- ^ computation that may raise errors
        -> a (e,ex) b   -- ^ computation to handle errors
        -> a e b
    handle f h = tryInUnless f (arr snd) h

    -- | Exception construct in the style of /Exceptional Syntax/,
    -- by Nick Benton and Andrew Kennedy, /JFP/ 11(4):395-410, July 2001.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|tryInUnless
    -- >            body
    -- >            (\res -> success)
    -- >            (\ex -> handler)
    -- >        |)
    tryInUnless ::
        a e b           -- ^ computation that may raise errors
        -> a (e,b) c    -- ^ computation to receive successful results
        -> a (e,ex) c   -- ^ computation to handle errors
        -> a e c

    -- | Handler that returns the error as a value.
    newError :: a e b -> a e (Either ex b)
    newError f = handle (f >>> arr Right) (arr (Left . snd))

-- | A suitable value for 'tryInUnless' when the arrow type belongs to
-- 'ArrowChoice'.  To use it, you must define either 'handle' or 'newError'.

tryInUnlessDefault :: (ArrowError ex a, ArrowChoice a) =>
    a e b           -- ^ computation that may raise errors
    -> a (e,b) c    -- ^ computation to receive successful results
    -> a (e,ex) c   -- ^ computation to handle errors
    -> a e c
tryInUnlessDefault f s h = arr id &&& newError f >>> arr dist >>> h ||| s
  where
    dist (e, Left ex) = Left (e, ex)
    dist (e, Right b) = Right (e, b)

-- tryInUnless (and thus handle) could be replaced by newError if:
-- 1. When ArrowChoice is available, tryInUnless and newError are equivalent.
-- 2. When tryInUnless is available, so is ArrowChoice.
--    (Counterexample: general CoKleisli)

-- | An arrow type that can be used to interpret synchronous circuits.

class ArrowLoop a => ArrowCircuit a where
    -- | A delay component.
    delay ::
        b        -- ^ the value to return initially.
        -> a b b -- ^ an arrow that propagates its input with a one-tick delay.
