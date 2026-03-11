{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Automaton
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Simple Mealy-style automata.

module Control.Arrow.Transformer.Automaton(
    Automaton(Automaton), runAutomaton,
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
import Data.Stream

import Prelude hiding (id,(.))

-- | An arrow type comprising Mealy-style automata, each step of which is
-- is a computation in the original arrow type.

newtype Automaton a b c = Automaton (a b (c, Automaton a b c))

instance Arrow a => ArrowTransformer Automaton a where
    lift f = c
      where
        c = Automaton (f &&& arr (const c))

instance Arrow a => Category (Automaton a) where
    id = lift id
    Automaton f . Automaton g =
        Automaton (arr (\((z, cf), cg) -> (z, cf . cg)) . first f . g)

instance Arrow a => Arrow (Automaton a) where
    arr f = lift (arr f)
    first (Automaton f) =
        Automaton (first f >>>
            arr (\((x', c), y) -> ((x', y), first c)))
    second (Automaton f) =
        Automaton (second f >>>
            arr (\(x, (y', c)) -> ((x, y'), second c)))
    Automaton f1 *** Automaton f2 =
        Automaton ((f1 *** f2) >>>
            arr (\((x', c1), (y', c2)) -> ((x', y'), c1 *** c2)))
    Automaton f1 &&& Automaton f2 =
        Automaton ((f1 &&& f2) >>>
            arr (\((x1, c1), (x2, c2)) -> ((x1, x2), c1 &&& c2)))

instance ArrowChoice a => ArrowChoice (Automaton a) where
    left (Automaton f) = left_f
      where
        left_f = Automaton (left f >>> arr combine)
        combine (Left (y, cf)) = (Left y, left cf)
        combine (Right z) = (Right z, left_f)
    right (Automaton f) = right_f
      where
        right_f = Automaton (right f >>> arr combine)
        combine (Left z) = (Left z, right_f)
        combine (Right (y, cf)) = (Right y, right cf)
    Automaton f1 +++ Automaton f2 =
        Automaton ((f1 +++ f2) >>> arr combine)
      where
        combine (Left  (x, c)) = (Left x,  c +++ Automaton f2)
        combine (Right (x, c)) = (Right x, Automaton f1 +++ c)
    Automaton f1 ||| Automaton f2 =
        Automaton ((f1 +++ f2) >>> arr combine)
      where
        combine (Left  (x, c)) = (x, c ||| Automaton f2)
        combine (Right (x, c)) = (x, Automaton f1 ||| c)

instance ArrowZero a => ArrowZero (Automaton a) where
    zeroArrow = Automaton zeroArrow

instance ArrowPlus a => ArrowPlus (Automaton a) where
    Automaton f <+> Automaton g = Automaton (f <+> g)

-- Circuit combinators

instance ArrowLoop a => ArrowLoop (Automaton a) where
    loop (Automaton f) =
        Automaton (loop (f >>>
            arr (\((x, y), cf) -> ((x, loop cf), y))))

instance ArrowLoop a => ArrowCircuit (Automaton a) where
    delay x = Automaton (arr (\x' -> (x, delay x')))

-- Other instances

instance Arrow a => Functor (Automaton a b) where
    fmap f g = g >>> arr f

instance Arrow a => Applicative (Automaton a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance ArrowPlus a => Alternative (Automaton a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance ArrowPlus a => Semigroup (Automaton a b c) where
    (<>) = (<+>)
#endif

instance ArrowPlus a => Monoid (Automaton a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

--    runAutomaton (Automaton f) = proc (e, Cons x xs) -> do
--        (y, c) <- f <- (e, x)
--        ys <- runAutomaton c -<< (e, xs)
--        returnA -< Cons y ys

-- | Encapsulating an automaton by running it on a stream of inputs,
-- obtaining a stream of outputs.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> do
-- >        ...
-- >        ys <- (|runAutomaton (\x -> ...)|) xs
--
-- Here @xs@ refers to the input stream and @x@ to individual
-- elements of that stream.  @ys@ is bound to the output stream.

runAutomaton :: (ArrowLoop a, ArrowApply a) =>
    Automaton a (e,b) c -> a (e,Stream b) (Stream c)
runAutomaton (Automaton f) =
    arr (\(e, Cons x xs) -> ((e, x), (e, xs))) >>>
    first f >>>
    arr (\((y, c), (e, xs)) -> (y, (runAutomaton c, (e, xs)))) >>>
    second app >>>
    arr (uncurry Cons)

instance (ArrowLoop a, ArrowApply a) => ArrowAddStream (Automaton a) a where
    liftStream = lift
    elimStream = runAutomaton

-- other promotions

instance ArrowWriter w a => ArrowWriter w (Automaton a) where
    write = lift write
    newWriter (Automaton f) =
        Automaton (newWriter f >>>
            arr (\((c, f'), w) -> ((c, w), newWriter f')))

instance ArrowError r a => ArrowError r (Automaton a) where
    raise = lift raise
    tryInUnless f0@(Automaton f) s0@(Automaton s) h0@(Automaton h) =
        Automaton (tryInUnless f sA hA)
      where
        sA = arr (\(b,(c,f')) -> ((b,c),f')) >>> first s >>>
            arr (\((d,s'),f') -> (d, tryInUnless f' s' h0))
        hA = h >>> arr (\(d,h') -> (d, tryInUnless f0 s0 h'))
    newError (Automaton f) = Automaton (newError f >>> arr h)
      where
        h (Left ex) = (Left ex, newError (Automaton f))
        h (Right (c, f')) = (Right c, newError f')

instance ArrowReader r a => ArrowReader r (Automaton a) where
    readState = lift readState
    newReader (Automaton f) =
        Automaton (newReader f >>> second (arr newReader))

instance ArrowState s a => ArrowState s (Automaton a) where
    fetch = lift fetch
    store = lift store

-- encapsulations

instance ArrowAddWriter w a a' =>
        ArrowAddWriter w (Automaton a) (Automaton a') where
    liftWriter (Automaton f) =
        Automaton (liftWriter f >>>
            arr (\(c, f') -> (c, liftWriter f')))
    elimWriter (Automaton f) =
        Automaton (elimWriter f >>>
            arr (\((c, f'), w) -> ((c, w), elimWriter f')))

instance ArrowAddReader r a a' =>
        ArrowAddReader r (Automaton a) (Automaton a') where
    liftReader (Automaton f) =
        Automaton (liftReader f >>>
            arr (\(c, f') -> (c, liftReader f')))
    elimReader (Automaton f) =
        Automaton (elimReader f >>> second (arr elimReader))


instance ArrowAddState r a a' =>
        ArrowAddState r (Automaton a) (Automaton a') where
    liftState (Automaton f) =
        Automaton (liftState f >>>
            arr (\(c, f') -> (c, liftState f')))
    elimState (Automaton f) =
        Automaton (elimState f >>>
            arr (\((c, f'), s) -> ((c, s), elimState f')))
