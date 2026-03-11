{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Stream
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Arrow transformer lifting an arrow to streams.

module Control.Arrow.Transformer.Stream(
    StreamArrow(StreamArrow),
    runStream,
    StreamMap,
    StreamMapST, runStreamST,
    ArrowAddStream(..),
    ) where

import Control.Arrow.Internals
import Control.Arrow.Operations
import Control.Arrow.Transformer

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.ST
import Data.Monoid
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

import Prelude hiding (id,(.))

-- | Arrows between streams.
--
-- /Note/: 'lift' is only a functor if '***' in the underlying arrow is.

newtype StreamArrow a b c = StreamArrow (a (Stream b) (Stream c))

instance Category a => Category (StreamArrow a) where
    id = StreamArrow id
    StreamArrow f . StreamArrow g = StreamArrow (f . g)

instance Arrow a => Arrow (StreamArrow a) where
    arr f = StreamArrow (arr (fmap f))
    first (StreamArrow f) =
        StreamArrow (arr Stream.unzip >>> first f >>> arr (uncurry Stream.zip))

genmap :: Arrow a => a b c -> a (Stream b) (Stream c)
genmap f =
    arr (\xs -> (Stream.head xs, Stream.tail xs)) >>>
        f *** genmap f >>> arr (uncurry (Stream.Cons))

-- Caution: genmap is only a functor if *** for the base arrow is.
-- (For Kleisli arrows, that would mean a commutative monad.)
-- The same goes for the equivalent lift: it can be used to lift arrows,
-- but won't preserve composition unless *** does.

instance Arrow a => ArrowTransformer (StreamArrow) a where
    lift f = StreamArrow (genmap f)

-- The following promotions follow directly from the arrow transformer.

instance ArrowZero a => ArrowZero (StreamArrow a) where
    zeroArrow = lift zeroArrow

instance ArrowState s a => ArrowState s (StreamArrow a) where
    fetch = lift fetch
    store = lift store

instance ArrowWriter w a => ArrowWriter w (StreamArrow a) where
    write = lift write
    newWriter (StreamArrow f) = StreamArrow (newWriter f >>> arr strength)
      where
        strength :: Functor w' => (w' a',b) -> w' (a',b)
        strength (v, y) = fmap (\x -> (x, y)) v

-- liftings of standard classes

instance Arrow a => ArrowChoice (StreamArrow a) where
    left (StreamArrow f) =
        StreamArrow ((arr getLeft >>> f) &&& arr id >>> arr replace)
      where
        getLeft (Cons (Left x) xs) = Cons x (getLeft xs)
        getLeft (Cons (Right _) xs) = getLeft xs
        replace (~(Cons x xs), Cons (Left _) ys) =
            Cons (Left x) (replace (xs, ys))
        replace (xs, Cons (Right y) ys) =
            Cons (Right y) (replace (xs, ys))

instance ArrowLoop a => ArrowLoop (StreamArrow a) where
    loop (StreamArrow f) =
        StreamArrow (loop (arr (uncurry Stream.zip) >>> f >>> arr Stream.unzip))

instance ArrowPlus a => ArrowPlus (StreamArrow a) where
    StreamArrow f <+> StreamArrow g = StreamArrow (f <+> g)

-- I don't know of any other useful promotions.
-- (elimWriter can be promoted, but doesn't seem useful.)

-- Circuits

instance ArrowLoop a => ArrowCircuit (StreamArrow a) where
    delay x = StreamArrow (arr (Cons x))

-- Other instances

instance Arrow a => Functor (StreamArrow a b) where
    fmap f g = g >>> arr f

instance Arrow a => Applicative (StreamArrow a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance ArrowPlus a => Alternative (StreamArrow a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance ArrowPlus a => Semigroup (StreamArrow a b c) where
    (<>) = (<+>)
#endif

instance ArrowPlus a => Monoid (StreamArrow a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

-- | Run a stream processor on a stream of inputs, obtaining a stream
-- of outputs.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> do
-- >        ...
-- >        ys <- (|runStream (\x -> ...)|) xs
--
-- Here @xs@ refers to the input stream and @x@ to individual
-- elements of that stream.  @ys@ is bound to the output stream.

runStream :: ArrowLoop a => StreamArrow a (e,b) c -> a (e,Stream b) (Stream c)
runStream (StreamArrow f) = arr (\(e, xs) -> fmap (\x -> (e, x)) xs) >>> f

instance ArrowLoop a => ArrowAddStream (StreamArrow a) a where
    liftStream = lift
    elimStream = runStream

-- | Mappings of streams
type StreamMap = StreamArrow (->)

-- | In-place state updates.
--
-- /Note/: this is an arrow type, and 'lift' can be used to promote arrows
-- from @'Kleisli' ('ST' s)@: the resulting arrow updates the state for
-- each stream element in turn, and as long as the final state in not
-- required all is well.  However, 'lift' does not preserve composition,
-- because this monad isn't commutative.  In particular, a composition
-- of 'lift's of state transformers will not work, as the second will
-- require the final state of the first.

type StreamMapST s = StreamArrow (Kleisli (ST s))

-- | Encapsulate a local state.

runStreamST :: (forall s. StreamMapST s e c) -> StreamMap e c
runStreamST cf = StreamArrow $ \ input ->
    runST (let StreamArrow (Kleisli f) = cf in f input)
