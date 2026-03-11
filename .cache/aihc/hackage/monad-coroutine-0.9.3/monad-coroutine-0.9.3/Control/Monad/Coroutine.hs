{- 
    Copyright 2009-2014 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | This module defines the 'Coroutine' monad transformer.
-- 
-- A 'Coroutine' monadic computation can 'suspend' its execution at any time, returning control to its invoker. The
-- returned suspension value contains the coroutine's resumption wrapped in a 'Functor'. Here is an example of a
-- coroutine in the 'IO' monad that suspends computation using the functor 'Yield' from the
-- "Control.Monad.Coroutine.SuspensionFunctors" module:
-- 
-- @
-- producer :: Coroutine (Yield Int) IO String
-- producer = do yield 1
--               lift (putStrLn \"Produced one, next is four.\")
--               yield 4
--               return \"Finished\"
-- @
-- 
-- To continue the execution of a suspended 'Coroutine', extract it from the suspension functor and apply its 'resume'
-- method. The easiest way to run a coroutine to completion is by using the 'pogoStick' function, which keeps resuming
-- the coroutine in trampolined style until it completes. Here is one way to apply 'pogoStick' to the /producer/ example
-- above:
-- 
-- @
-- printProduce :: Show x => Coroutine (Yield x) IO r -> IO r
-- printProduce producer = pogoStick (\\(Yield x cont) -> lift (print x) >> cont) producer
-- @
-- 
-- Multiple concurrent coroutines can be run as well, and this module provides two different ways. To run two
-- interleaved computations, use a 'WeaveStepper' to 'weave' together steps of two different coroutines into a single
-- coroutine, which can then be executed by 'pogoStick'.
-- 
-- For various uses of trampoline-style coroutines, see
-- 
-- > Coroutine Pipelines - Mario Blažević, The Monad.Reader issue 19, pages 29-50
-- 
-- > Trampolined Style - Ganz, S. E. Friedman, D. P. Wand, M, ACM SIGPLAN NOTICES, 1999, VOL 34; NUMBER 9, pages 18-27
-- 
-- and
-- 
-- > The Essence of Multitasking - William L. Harrison, Proceedings of the 11th International Conference on Algebraic
-- > Methodology and Software Technology, volume 4019 of Lecture Notes in Computer Science, 2006

{-# LANGUAGE ScopedTypeVariables, Rank2Types, EmptyDataDecls #-}

module Control.Monad.Coroutine
   (
    -- * Coroutine definition
    Coroutine(Coroutine, resume), CoroutineStepResult, suspend,
    -- * Coroutine operations
    mapMonad, mapSuspension, mapFirstSuspension,
    -- * Running coroutines
    Naught, runCoroutine, bounce, pogoStick, pogoStickM, foldRun,
    -- * Weaving coroutines together
    PairBinder, sequentialBinder, parallelBinder, liftBinder,
    Weaver, WeaveStepper, weave, merge
   )
where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM, (<=<))
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either (partitionEithers)

import Control.Monad.Parallel (MonadParallel(..))

import Prelude hiding (fail)

-- | Suspending, resumable monadic computations.
newtype Coroutine s m r = Coroutine {
   -- | Run the next step of a `Coroutine` computation. The result of the step execution will be either a suspension or
   -- the final coroutine result.
   resume :: m (Either (s (Coroutine s m r)) r)
   }

type CoroutineStepResult s m r = Either (s (Coroutine s m r)) r

instance (Functor s, Functor m) => Functor (Coroutine s m) where
   fmap f t = Coroutine (fmap (apply f) (resume t))
      where apply fc (Right x) = Right (fc x)
            apply fc (Left s) = Left (fmap (fmap fc) s)

instance (Functor s, Functor m, Monad m) => Applicative (Coroutine s m) where
   pure x = Coroutine (return (Right x))
   (<*>) = ap
   t *> f = Coroutine (resume t >>= apply f)
      where apply fc (Right _) = resume fc
            apply fc (Left s) = return (Left (fmap (>> fc) s))

instance (Functor s, Monad m) => Monad (Coroutine s m) where
   return = pure
   t >>= f = Coroutine (resume t >>= apply f)
      where apply fc (Right x) = resume (fc x)
            apply fc (Left s) = return (Left (fmap (>>= fc) s))
   (>>) = (*>)

instance (Functor s, MonadFail m) => MonadFail (Coroutine s m) where
   fail msg = Coroutine (Right <$> fail msg)

instance (Functor s, MonadParallel m) => MonadParallel (Coroutine s m) where
   bindM2 = liftBinder bindM2

instance Functor s => MonadTrans (Coroutine s) where
   lift = Coroutine . liftM Right

instance (Functor s, MonadIO m) => MonadIO (Coroutine s m) where
   liftIO = lift . liftIO

-- | The 'Naught' functor instance doesn't contain anything and cannot be constructed. Used for building non-suspendable
-- coroutines.
data Naught x
instance Functor Naught where
   fmap _ _ = undefined

-- | Suspend the current 'Coroutine'.
suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Left s))
{-# INLINE suspend #-}

-- | Change the base monad of a 'Coroutine'.
mapMonad :: forall s m m' x. (Functor s, Monad m, Monad m') =>
            (forall y. m y -> m' y) -> Coroutine s m x -> Coroutine s m' x
mapMonad f cort = Coroutine {resume= liftM map' (f $ resume cort)}
   where map' (Right r) = Right r
         map' (Left s) = Left (fmap (mapMonad f) s)

-- | Change the suspension functor of a 'Coroutine'.
mapSuspension :: (Functor s, Monad m) => (forall y. s y -> s' y) -> Coroutine s m x -> Coroutine s' m x
mapSuspension f cort = Coroutine {resume= liftM map' (resume cort)}
   where map' (Right r) = Right r
         map' (Left s) = Left (f $ fmap (mapSuspension f) s)
{-# INLINE mapSuspension #-}

-- | Modify the first upcoming suspension of a 'Coroutine'.
mapFirstSuspension :: forall s m x. (Functor s, Monad m) =>
                      (forall y. s y -> s y) -> Coroutine s m x -> Coroutine s m x
mapFirstSuspension f cort = Coroutine {resume= liftM map' (resume cort)}
   where map' (Right r) = Right r
         map' (Left s) = Left (f s)

-- | Convert a non-suspending 'Coroutine' to the base monad.
runCoroutine :: Monad m => Coroutine Naught m x -> m x
runCoroutine = pogoStick (error "runCoroutine can run only a non-suspending coroutine!")

-- | Runs a single step of a suspendable 'Coroutine', using a function that extracts the coroutine resumption from its
-- suspension functor.
bounce :: (Monad m, Functor s) => (s (Coroutine s m x) -> Coroutine s m x) -> Coroutine s m x -> Coroutine s m x
bounce spring c = lift (resume c) >>= either spring return

-- | Runs a suspendable 'Coroutine' to its completion.
pogoStick :: Monad m => (s (Coroutine s m x) -> Coroutine s m x) -> Coroutine s m x -> m x
pogoStick spring = loop
   where loop c = resume c >>= either (loop . spring) return

-- | Runs a suspendable 'Coroutine' to its completion with a monadic action.
pogoStickM :: Monad m => (s (Coroutine s m x) -> m (Coroutine s m x)) -> Coroutine s m x -> m x
pogoStickM spring = loop
   where loop c = resume c >>= either (loop <=< spring) return

-- | Runs a suspendable coroutine much like 'pogoStick', but allows the resumption function to thread an arbitrary
-- state as well.
foldRun :: Monad m => (a -> s (Coroutine s m x) -> (a, Coroutine s m x)) -> a -> Coroutine s m x -> m (a, x)
foldRun f a c = resume c
                >>= \s-> case s 
                         of Right result -> return (a, result)
                            Left c' -> uncurry (foldRun f) (f a c')

-- | Type of functions that can bind two monadic values together, used to combine two coroutines' step results. The two
-- functions provided here are 'sequentialBinder' and 'parallelBinder'.
type PairBinder m = forall x y r. (x -> y -> m r) -> m x -> m y -> m r

-- | A 'PairBinder' that runs the two steps sequentially before combining their results.
sequentialBinder :: Monad m => PairBinder m
sequentialBinder f mx my = do {x <- mx; y <- my; f x y}

-- | A 'PairBinder' that runs the two steps in parallel.
parallelBinder :: MonadParallel m => PairBinder m
parallelBinder = bindM2

-- | Lifting a 'PairBinder' onto a 'Coroutine' monad transformer.
liftBinder :: forall s m. (Functor s, Monad m) => PairBinder m -> PairBinder (Coroutine s m)
liftBinder binder f t1 t2 = Coroutine (binder combine (resume t1) (resume t2)) where
   combine (Right x) (Right y) = resume (f x y)
   combine (Left s) (Right y) = return $ Left (fmap (flip f y =<<) s)
   combine (Right x) (Left s) = return $ Left (fmap (f x =<<) s)
   combine (Left s1) (Left s2) = return $ Left (fmap (liftBinder binder f $ suspend s1) s2)

-- | Type of functions that can weave two coroutines into a single coroutine.
type Weaver s1 s2 s3 m x y z = Coroutine s1 m x -> Coroutine s2 m y -> Coroutine s3 m z

-- | Type of functions capable of combining two coroutines' 'CoroutineStepResult' values into a third one. Module
-- "Monad.Coroutine.SuspensionFunctors" contains several 'WeaveStepper' examples.
type WeaveStepper s1 s2 s3 m x y z =
   Weaver s1 s2 s3 m x y z -> CoroutineStepResult s1 m x -> CoroutineStepResult s2 m y -> Coroutine s3 m z

-- | Weaves two coroutines into one, given a 'PairBinder' to run the next step of each coroutine and a 'WeaveStepper' to
-- combine the results of the steps.
weave :: forall s1 s2 s3 m x y z. (Monad m, Functor s1, Functor s2, Functor s3) =>
         PairBinder m -> WeaveStepper s1 s2 s3 m x y z -> Weaver s1 s2 s3 m x y z
weave runPair weaveStep c1 c2 = zipC c1 c2 where
   zipC c1 c2 = Coroutine{resume= runPair (\c1' c2'-> resume $ weaveStep zipC c1' c2') (resume c1) (resume c2)}

-- | Weaves a list of coroutines with the same suspension functor type into a single coroutine. The coroutines suspend
-- and resume in lockstep.
merge :: forall s m x. (Monad m, Functor s) =>
         (forall y. [m y] -> m [y]) -> (forall y. [s y] -> s [y])
      -> [Coroutine s m x] -> Coroutine s m [x]
merge sequence1 sequence2 corts = Coroutine{resume= liftM step $ sequence1 (map resume corts)} where
   step :: [CoroutineStepResult s m x] -> CoroutineStepResult s m [x]
   step list = case partitionEithers list
               of ([], ends) -> Right ends
                  (suspensions, ends) -> Left $ fmap (merge sequence1 sequence2 . (map return ends ++)) $
                                         sequence2 suspensions
