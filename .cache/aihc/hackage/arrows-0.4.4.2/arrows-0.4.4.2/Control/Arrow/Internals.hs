-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Internals
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Manipulation of composite arrow types, beyond the basic lifting and
-- encapsulation provided with each arrow transformer.
--
-- The signatures are designed to be compatible with the proposed notation
-- for arrows, cf. <http://www.haskell.org/arrows/>.

-- #hide
module Control.Arrow.Internals (
    ArrowAddState(..),
    ArrowAddReader(..),
    ArrowAddWriter(..),
    ArrowAddError(..),
    ArrowAddStream(..),
    ) where

import Control.Arrow
import Control.Arrow.Operations
import Data.Stream

-- | Adding a 'Control.Arrow.Transformer.State.StateArrow' to an
-- arrow type, but not necessarily as the outer arrow transformer.
--
-- Typically a composite arrow type is built by applying a series
-- of arrow transformer to a base arrow (usually either a function
-- arrow or a 'Kleisli' arrow.  One can add a transformer to the
-- top of this stack using the 'Control.Arrow.Transformer.lift'
-- method of the 'Control.Arrow.Transformer.ArrowTransformer' class,
-- or remove a state transformer from the top of the stack using the
-- 'Control.Arrow.Transformer.State.runState' encapsulation operator.
-- The methods of this class add and remove state transformers anywhere
-- in the stack.  In the instance
--
-- >    instance Arrow a => ArrowAddState s (ArrowState s a) a
--
-- they are equivalent to 'Control.Arrow.Transformer.lift' and
-- 'Control.Arrow.Transformer.State.runState' respectively.
-- Instances are lifted through other transformers with
--
-- >    instance ArrowAddState s a a' =>
-- >        ArrowAddState s (FooArrow a) (FooArrow a')

class (ArrowState s a, Arrow a') => ArrowAddState s a a' | a -> a' where

    -- | Lift a computation from an arrow to one with an added state.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|liftState cmd|)

    liftState :: a' e b -> a e b

    -- | Elimination of a state transformer from a computation,
    -- exposing the initial and final states.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> do
    -- >        ...
    -- >        (result, final_state) <- (|elimState cmd|) init_state

    elimState :: a e b -> a' (e,s) (b,s)

-- | Adding a 'Control.Arrow.Transformer.Reader.ReaderArrow' to an
-- arrow type, but not necessarily as the outer arrow transformer.
--
-- Typically a composite arrow type is built by applying a series
-- of arrow transformer to a base arrow (usually either a function
-- arrow or a 'Kleisli' arrow.  One can add a transformer to the
-- top of this stack using the 'Control.Arrow.Transformer.lift'
-- method of the 'Control.Arrow.Transformer.ArrowTransformer' class,
-- or remove a state transformer from the top of the stack using the
-- 'Control.Arrow.Transformer.Reader.runReader' encapsulation operator.
-- The methods of this class add and remove state transformers anywhere
-- in the stack.  In the instance
--
-- >    instance Arrow a => ArrowAddReader r (ArrowReader r a) a
--
-- they are equivalent to 'Control.Arrow.Transformer.lift' and
-- 'Control.Arrow.Transformer.Reader.runReader' respectively.
-- Instances are lifted through other transformers with
--
-- >    instance ArrowAddReader r a a' =>
-- >        ArrowAddReader r (FooArrow a) (FooArrow a')

class (ArrowReader r a, Arrow a') => ArrowAddReader r a a' | a -> a' where

    -- | Lift a computation from an arrow to one with an added environment.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|liftReader cmd|)

    liftReader :: a' e b -> a e b

    -- | Elimination of a state reader from a computation,
    -- taking a value for the state.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|elimReader cmd|) env

    elimReader :: a e b -> a' (e,r) b

-- | Adding a 'Control.Arrow.Transformer.Writer.WriterArrow' to an
-- arrow type, but not necessarily as the outer arrow transformer.
--
-- Typically a composite arrow type is built by applying a series
-- of arrow transformer to a base arrow (usually either a function
-- arrow or a 'Kleisli' arrow.  One can add a transformer to the
-- top of this stack using the 'Control.Arrow.Transformer.lift'
-- method of the 'Control.Arrow.Transformer.ArrowTransformer' class,
-- or remove a state transformer from the top of the stack using the
-- 'Control.Arrow.Transformer.Writer.runWriter' encapsulation operator.
-- The methods of this class add and remove state transformers anywhere
-- in the stack.  In the instance
--
-- >    instance Arrow a => ArrowAddWriter w (ArrowWriter w a) a
--
-- they are equivalent to 'Control.Arrow.Transformer.lift' and
-- 'Control.Arrow.Transformer.Writer.runWriter' respectively.
-- Instances are lifted through other transformers with
--
-- >    instance ArrowAddWriter w a a' =>
-- >        ArrowAddWriter w (FooArrow a) (FooArrow a')

class (ArrowWriter w a, Arrow a') => ArrowAddWriter w a a' | a -> a' where

    -- | Lift a computation from an arrow to one with added output.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|liftWriter cmd|)

    liftWriter :: a' e b -> a e b

    -- | Elimination of an output writer from a computation,
    -- providing the accumulated output.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> do
    -- >        ...
    -- >        (result, output) <- (|elimWriter cmd|)

    elimWriter :: a e b -> a' e (b,w)

-- | Adding a 'Control.Arrow.Transformer.Error.ErrorArrow' to an
-- arrow type, but not necessarily as the outer arrow transformer.
--
-- Typically a composite arrow type is built by applying a series
-- of arrow transformer to a base arrow (usually either a function
-- arrow or a 'Kleisli' arrow.  One can add a transformer to the
-- top of this stack using the 'Control.Arrow.Transformer.lift'
-- method of the 'Control.Arrow.Transformer.ArrowTransformer' class,
-- or remove a state transformer from the top of the stack using the
-- 'Control.Arrow.Transformer.Error.runError' encapsulation operator.
-- The methods of this class add and remove state transformers anywhere
-- in the stack.  In the instance
--
-- >    instance Arrow a => ArrowAddError ex (ArrowError ex a) a
--
-- they are equivalent to 'Control.Arrow.Transformer.lift' and
-- 'Control.Arrow.Transformer.Error.runError' respectively.
-- Instances are lifted through other transformers with
--
-- >    instance ArrowAddError ex a a' =>
-- >        ArrowAddError ex (FooArrow a) (FooArrow a')
--
-- This could be combined with 'Control.Arrow.Transformer.Error.handle',
-- since the resulting arrow is always the arrow of the handler.
-- Separating them has the advantage of consistency with the other arrows,
-- and might give more helpful type error messages.

class (ArrowError ex a, Arrow a') => ArrowAddError ex a a' | a -> a' where

    -- | Lift a computation from an arrow to one with error handling.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|liftError cmd|)

    liftError :: a' e b -> a e b

    -- | Elimination of errors from a computation,
    -- by completely handling any errors.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        body `elimError` \ex -> handler

    elimError :: a e b -> a' (e,ex) b -> a' e b

-- | Adding a 'Control.Arrow.Transformer.Stream.StreamArrow' to an
-- arrow type, but not necessarily as the outer arrow transformer.
--
-- Typically a composite arrow type is built by applying a series
-- of arrow transformer to a base arrow (usually either a function
-- arrow or a 'Kleisli' arrow.  One can add a transformer to the
-- top of this stack using the 'Control.Arrow.Transformer.lift'
-- method of the 'Control.Arrow.Transformer.ArrowTransformer' class,
-- or remove a state transformer from the top of the stack using the
-- 'Control.Arrow.Transformer.Stream.runStream' encapsulation operator.
-- The methods of this class add and remove state transformers anywhere
-- in the stack.  In the instance
--
-- >    instance Arrow a => ArrowAddStream (ArrowStream a) a
--
-- they are equivalent to 'Control.Arrow.Transformer.lift' and
-- 'Control.Arrow.Transformer.Stream.runStream' respectively.
-- Instances are lifted through other transformers with
--
-- >    instance ArrowAddStream a a' =>
-- >        ArrowAddStream (FooArrow a) (FooArrow a')

class (ArrowCircuit a, Arrow a') => ArrowAddStream a a' | a -> a' where

    -- | Lift a computation from an arrow to a stream processing one.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|liftStream cmd|)

    liftStream :: a' e b -> a e b

    -- | Run a stream processor on a stream of inputs,
    -- obtaining a stream of outputs.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> do
    -- >        ...
    -- >        ys <- (|elimStream (\x -> ...)|) xs
    --
    -- Here @xs@ refers to the input stream and @x@ to individual
    -- elements of that stream.  @ys@ is bound to the output stream.

    elimStream :: a (e,b) c -> a' (e,Stream b) (Stream c)
