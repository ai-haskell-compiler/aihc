{- 
    Copyright 2010-2012 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | The "Control.Monad.Coroutine" tests.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (sequence)
import Control.Exception (assert)
import Control.Monad (liftM, mapM, when)
import Control.Parallel (pseq)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Functor.Sum (Sum(InL, InR))
import Data.List (find)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Coroutine.Nested
import Control.Monad.Parallel (MonadParallel, bindM2, liftM2, sequence)

import Criterion.Main

factors n = maybe [n] (\k-> (k : factors (n `div` k))) (find (\k-> n `mod` k == 0) [2 .. n - 1])

fib x 0 | x >= 0 = 1
fib _ 1 = 1
fib x n = fib x (n - 2) + fib x (n - 1)

factorFibs :: MonadParallel m => [Int] -> m Integer
factorFibs nums = pogoStick runIdentity $
                  weave bindM2 weaveSteps
                     (mapM_ (yieldApply (fib 0)) nums)
                     (factorize 0)
   where factorize :: MonadParallel m => Integer -> Coroutine (Await (Maybe Integer)) m Integer
         factorize sum = await
                         >>= maybe
                                (return sum)
                                (\n-> factorize (sum + n {-product (factors n)-}))
         weaveSteps _ (Left s) (Right r) = liftM (const r) $ mapSuspension unYield (suspend s)
            where unYield (Yield _ c) = Identity c
         weaveSteps _ (Right _) (Left s) = mapSuspension unAwait (suspend s)
            where unAwait (Await f) = Identity (f Nothing)
         weaveSteps weave (Left (Yield x c1)) (Left (Await c2)) = weave c1 (c2 (Just x))
         weaveSteps _ (Right _) (Right r) = return r

twoFibs :: forall m. MonadParallel m => [Int] -> m Integer
twoFibs nums = pogoStick resume (weave bindM2 stepper (fibs 1) (fibs 2))
               >>= \(x, y)-> return (x + y)
   where resume :: Yield Integer c -> c
         resume (Yield n c) = c
         stepper :: WeaveStepper (Yield Integer) (Yield Integer) (Yield Integer) m Integer Integer (Integer, Integer)
         stepper _ (Right n1) (Right n2) = return (n1, n2)
         stepper weave (Left (Yield n1 c1)) (Left (Yield n2 c2)) =
            assert (n1 == n2) (yield n1 >> weave c1 c2)
         fibs ix = mapM_ (yieldApply (fib ix)) nums >> applyM (fib ix) (last nums)

twoFibsSeesaw :: MonadParallel m => [Int] -> m Integer
twoFibsSeesaw nums = pogoStick (\(Yield _ c)-> c) $
                     weave bindM2 weaveYields (fibs 1) (fibs 2)
   where weaveYields weave (Left (Yield left c1)) (Left (Yield right c2)) =
            assert (left == right) $ yield left >> weave c1 c2
         weaveYields _ (Right r1) (Right r2) = assert (r1 == r2) $ return r1
         fibs ix = mapM_ (yieldApply (fib ix)) nums >> applyM (fib ix) (last nums)

fibs :: MonadParallel m => Int -> [Int] -> m Integer
fibs coroutineCount nums = liftM sum $
                           pogoStick
                              resume
                              (merge sequence appendYields $ replicateIx coroutineCount fibs)
   where resume :: Yield [Integer] (Coroutine (Yield [Integer]) m [Integer]) -> Coroutine (Yield [Integer]) m [Integer]
         resume (Yield (x:xs) c) = assert (all (==x) xs) c
         fibs ix = mapM_ (yieldApply ((:[]) . fib ix)) nums >> applyM (fib ix) (last nums)
         appendYields :: [Yield [s] x] -> Yield [s] [x]
         appendYields yields = uncurry Yield $ foldr (\(Yield s x) (ss, xs)-> (s ++ ss, x:xs)) ([], []) yields

yieldApply f n = let result = f n in result `pseq` yield result
applyM f n = let result = f n in result `pseq` return result

replicateIx :: Int -> (Int -> x) -> [x]
replicateIx n f = map f [1..n]

nested :: (Monad m, Functor p) =>
          Int -> (Integer -> Coroutine p m ()) -> Coroutine (Sum p (Yield Integer)) m ()
nested level suspendParent = do mapSuspension InR (yield 1)
                                liftAncestor (suspendParent 2)
                                when (level > 0) (pogoStickNested cont $
                                                  nested (pred level) (liftAncestor . suspendParent))
   where cont (Yield x c) = c

runNested size = liftM fst $ foldRun add 0 (nested size yield)
   where add s (InL (Yield n c)) = (s + n, c)
         add s (InR (Yield n c)) = (s + 10 * n, c)


main = defaultMain ([bgroup "Identity" [bench name (nf (runIdentity . task name) size) | (name, size) <- tasks],
                     bgroup "Maybe" [bench name (nf (fromJust . task name) size) | (name, size) <- tasks],
                     bgroup "List" [bench name (nf (head . task name) size) | (name, size) <- tasks],
                     bgroup "IO" [bench name (whnfIO $ task name size) | (name, size) <- tasks]])

tasks = [("fib-factor", 32), ("2fibs", 30), ("2fibsSeesaw", 30), ("nested", 250),
         ("1*fibs", 33), ("2*fibs", 33), ("3*fibs", 33), ("4*fibs", 33)]

task :: MonadParallel m => String -> Int -> m Integer
task taskName size = 
   case taskName 
   of "fib-factor" -> factorFibs [1 .. size]
      "2fibs" -> twoFibs [1 .. size]
      "2fibsSeesaw" -> twoFibsSeesaw [1 .. size]
      "nested" -> runNested size
      coroutineCount : "*fibs" -> fibs (read [coroutineCount]) [1 .. size]
      _ -> error "Bad task."
