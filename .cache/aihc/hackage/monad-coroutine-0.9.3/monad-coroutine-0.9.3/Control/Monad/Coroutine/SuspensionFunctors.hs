{- 
    Copyright 2010-2014 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | This module defines some common suspension functors for use with the "Control.Monad.Coroutine" module.
-- 

{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Control.Monad.Coroutine.SuspensionFunctors
   (
    -- * Suspension functors
    Yield(Yield), Await(Await), Request(Request),
    ReadRequest, ReadingResult(..), Reader, Reading(..),
    eitherFunctor,
    yield, await, request, requestRead,
    -- * Utility functions
    concatYields, concatAwaits,
    -- * WeaveSteppers for weaving pairs of coroutines
    weaveAwaitYield, weaveAwaitMaybeYield, weaveRequests,
    weaveReadWriteRequests, weaveNestedReadWriteRequests
   )
where

import Prelude hiding (foldl, foldr)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (Foldable, foldl, foldr)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Sum (Sum(InL, InR))
import Data.Monoid (Monoid, mempty)

import Control.Monad.Coroutine
import Control.Monad.Coroutine.Nested (eitherFunctor, NestWeaveStepper, pogoStickNested)

-- | The 'Yield' functor instance is equivalent to (,) but more descriptive. A coroutine with this suspension functor
-- provides a value with every suspension.
data Yield x y = Yield x y
instance Functor (Yield x) where
   fmap f (Yield x y) = Yield x (f y)

-- | The 'Await' functor instance is equivalent to (->) but more descriptive. A coroutine with this suspension functor
-- demands a value whenever it suspends, before it can resume its execution.
newtype Await x y = Await (x -> y)
instance Functor (Await x) where
   fmap f (Await g) = Await (f . g)

-- | The 'Request' functor instance combines a 'Yield' of a request with an 'Await' for a response.
data Request request response x = Request request (response -> x)
instance Functor (Request x f) where
   fmap f (Request x g) = Request x (f . g)

data Reading x py y = Final x y                     -- ^ Final result chunk with the unconsumed portion of the input
                    | Advance (Reader x py y) y py  -- ^ A part of the result with the reader of more input and the EOF
                    | Deferred (Reader x py y) y    -- ^ Reader of more input, plus the result if there isn't any.

data ReadingResult x py y = ResultPart py (Reader x py y)  -- ^ A part of the result with the reader of more input
                          | FinalResult y                  -- ^ Final result chunk

type Reader x py y = x -> Reading x py y

-- | Combines a 'Yield' of a 'Reader' with an 'Await' for a 'ReadingResult'.
data ReadRequest x z = forall a py y. ReadRequest (Reader x py y) y (ReadingResult x py y -> z)
instance Functor (ReadRequest x) where
   fmap f (ReadRequest r y g) = ReadRequest r y (f . g)

-- | Suspend the current coroutine yielding a value.
yield :: Monad m => x -> Coroutine (Yield x) m ()
yield x = suspend (Yield x (return ()))

-- | Suspend the current coroutine until a value is provided.
await :: Monad m => Coroutine (Await x) m x
await = suspend (Await return)

-- | Suspend yielding a request and awaiting the response.
request :: Monad m => x -> Coroutine (Request x y) m y
request x = suspend (Request x return)

-- | Suspend yielding a 'ReadRequest' and awaiting the 'ReadingResult'.
requestRead :: (Monad m, Monoid x) => Reader x py y -> Coroutine (ReadRequest x) m (ReadingResult x py y)
requestRead p = suspend (ReadRequest p eof return)
   where eof = case p mempty
               of Deferred _ r -> r
                  Advance _ r rp -> r
                  Final _ r -> r

-- | Converts a coroutine yielding collections of values into one yielding single values.
concatYields :: (Monad m, Foldable f) => Coroutine (Yield (f x)) m r -> Coroutine (Yield x) m r
concatYields c = Coroutine{resume= resume c >>= foldChunk}
   where foldChunk (Right r) = return (Right r)
         foldChunk (Left (Yield s c')) = foldr f (resume $ concatYields c') s
         f x rest = return (Left $ Yield x (Coroutine rest))

-- | Converts a coroutine awaiting single values into one awaiting collections of values.
concatAwaits :: (Monad m, Foldable f) => Coroutine (Await x) m r -> Coroutine (Await (f x)) m r
concatAwaits c = lift (resume c) >>= either concatenate return
   where concatenate s = do chunk <- await
                            concatAwaits (feedAll chunk (suspend s))
         feedAll :: (Foldable f, Monad m) => f x -> Coroutine (Await x) m r -> Coroutine (Await x) m r
         feedAll chunk c = foldl (flip feedCoroutine) c chunk
         feedCoroutine :: Monad m => x -> Coroutine (Await x) m r -> Coroutine (Await x) m r
         feedCoroutine x c = bounce (\(Await f)-> f x) c

-- | Weaves the suspensions of a 'Yield' and an 'Await' coroutine together into a plain 'Identity' coroutine. If the
-- 'Yield' coroutine terminates first, the 'Await' one is resumed using the argument default value.
weaveAwaitYield :: Monad m => x -> WeaveStepper (Await x) (Yield x) Identity m r1 r2 (r1, r2)
weaveAwaitYield _ weave (Left (Await f)) (Left (Yield x c)) = weave (f x) c
weaveAwaitYield x _ (Left (Await f)) (Right r2) = liftM (\r1-> (r1, r2)) $ mapSuspension proceed (f x)
   where proceed (Await f) = Identity (f x)
weaveAwaitYield _ _ (Right r1) (Left (Yield _ c)) = liftM ((,) r1) $ mapSuspension discardYield c
   where discardYield (Yield _ c) = Identity c
weaveAwaitYield _ _ (Right r1) (Right r2) = return (r1, r2)

-- | Like 'weaveAwaitYield', except the 'Await' coroutine expects 'Maybe'-wrapped values. After the 'Yield' coroutine
-- terminates, the 'Await' coroutine receives only 'Nothing'.
weaveAwaitMaybeYield :: Monad m => WeaveStepper (Await (Maybe x)) (Yield x) Identity m r1 r2 (r1, r2)
weaveAwaitMaybeYield weave (Left (Await f)) (Left (Yield x c)) = weave (f $ Just x) c
weaveAwaitMaybeYield _ (Left (Await f)) (Right r2) = liftM (\r1-> (r1, r2)) $ mapSuspension proceed (f Nothing)
   where proceed (Await f) = Identity (f Nothing)
weaveAwaitMaybeYield _ (Right r1) (Left (Yield _ c)) = liftM ((,) r1) $ mapSuspension discardYield c
   where discardYield (Yield _ c) = Identity c
weaveAwaitMaybeYield _ (Right r1) (Right r2) = return (r1, r2)

-- | Weaves two complementary 'Request' coroutine suspensions into a coroutine 'yield'ing both requests. If one
-- coroutine terminates before the other, the remaining coroutine is fed the appropriate  default value argument.
weaveRequests :: Monad m => x -> y -> WeaveStepper (Request x y) (Request y x) (Yield (x, y)) m r1 r2 (r1, r2)
weaveRequests _ _ weave (Left (Request x f)) (Left (Request y g)) = yield (x, y) >> weave (f y) (g x)
weaveRequests _ y weave (Left s1) (Right r2) = liftM (flip (,) r2) $ mapSuspension (defaultResponse y) (suspend s1)
   where defaultResponse a (Request b f) = Yield (b, a) (f a)
weaveRequests x _ weave (Right r1) (Left s2) = liftM ((,) r1) $ mapSuspension (defaultResponse x) (suspend s2)
   where defaultResponse a (Request b f) = Yield (a, b) (f a)
weaveRequests _ _ weave (Right r1) (Right r2) = return (r1, r2)

-- | The consumer coroutine requests input through 'ReadRequest' and gets 'ReadingResult' in response. The producer
-- coroutine receives the unconsumed portion of its last requested chunk as response.
weaveReadWriteRequests :: (Monad m, Monoid x) => WeaveStepper (ReadRequest x) (Request x x) Identity m r1 r2 (r1, r2)
weaveReadWriteRequests _ (Right r1) (Right r2) = return (r1, r2)
weaveReadWriteRequests _ (Left (ReadRequest p eof c)) (Right r2) =
   mapSuspension eofRequest $ liftM (\r1-> (r1, r2)) $ c $ FinalResult eof
   where eofRequest (ReadRequest _ eof c) = Identity (c $ FinalResult eof)
weaveReadWriteRequests _ (Right r1) (Left (Request chunk c)) =
   mapSuspension reflectRequest $ liftM ((,) r1) $ c chunk
   where reflectRequest (Request chunk c) = Identity (c chunk)
weaveReadWriteRequests weave (Left (ReadRequest p _ c1)) (Left (Request xs c2)) =
   case p xs
   of Final s r -> weave (c1 $ FinalResult r) (suspend $ Request s c2)
      Advance p' _ rp -> weave (c1 $ ResultPart rp p') (c2 mempty)
      Deferred p' eof -> weave (suspend $ ReadRequest p' eof c1) (c2 mempty)

-- | Like 'weaveReadWriteRequests' but for nested coroutines.
weaveNestedReadWriteRequests :: (Monad m, Functor s, Monoid x) =>
                                NestWeaveStepper s (ReadRequest x) (Request x x) m r1 r2 (r1, r2)
weaveNestedReadWriteRequests _ (Right r1) (Right r2) = return (r1, r2)
weaveNestedReadWriteRequests weave (Left (InL s)) cs2 =
   suspend $ fmap (flip weave (Coroutine $ return cs2)) s
weaveNestedReadWriteRequests weave cs1 (Left (InL s)) =
   suspend $ fmap (weave (Coroutine $ return cs1)) s
weaveNestedReadWriteRequests _ (Left (InR (ReadRequest p eof c))) (Right r2) =
   liftM (\r1-> (r1, r2)) $ pogoStickNested eofRequest $ c $ FinalResult eof
   where eofRequest (ReadRequest _ eof c) = c $ FinalResult eof
weaveNestedReadWriteRequests _ (Right r1) (Left (InR (Request chunk c))) =
   liftM ((,) r1) $ pogoStickNested reflectRequest $ c chunk
   where reflectRequest (Request chunk c) = c chunk
weaveNestedReadWriteRequests weave (Left (InR (ReadRequest p _ c1))) (Left (InR (Request xs c2))) =
   case p xs
   of Final s r -> weave (c1 $ FinalResult r) (suspend $ InR $ Request s c2)
      Advance p' _ rp -> weave (c1 $ ResultPart rp p') (c2 mempty)
      Deferred p' eof -> weave (suspend $ InR $ ReadRequest p' eof c1) (c2 mempty)
