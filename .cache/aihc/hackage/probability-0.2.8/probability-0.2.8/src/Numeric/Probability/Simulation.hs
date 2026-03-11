-- | Simulation
module Numeric.Probability.Simulation where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Random       as Rnd
import qualified Numeric.Probability.Trace        as Trace

import System.Random (Random, )

import qualified Numeric.Probability.Monad as MonadExt


{-

Naming convention:

 * @*@   takes @n :: Int@ and a generator and iterates the generator n times

 * @.@   produces a single result

 * @..@  produces a trace

 * @~@   takes @k :: Int@ [and @n :: Int@] and a generator and simulates
         the [n-fold repetition of the] generator k times


There are the following functions:

 * @n *.  t@   iterates t and produces a distribution

 * @n *.. t@   iterates t and produces a trace

 * @k     ~.  t@   simulates t and produces a distribution

 * @(k,n) ~*. t@   simulates the n-fold repetition of t and produces a distribution

 * @(k,n) ~.. t@   simulates the n-fold repetition of t and produces a trace


Iteration captures three iteration strategies:
iter builds an n-fold composition of a (randomized) transition
while and until implement conditional repetitions

The class Iterate allows the overloading of iteration for different
kinds of generators, namely transitions and Rnd.change changes:

 *  @Trans   a = a -> Dist a    ==>   c = Dist@

 *  @RChange a = a -> Rnd.T a   ==>   c = Rnd.T = IO@

-}


{- |
Simulation means to repeat a Rnd.change change many times and
to accumulate all results into a distribution. Therefore,
simulation can be regarded as an approximation of distributions
through randomization.

The Sim class allows the overloading of simulation for different
kinds of generators, namely transitions and Rnd.change changes:

  * @Trans   a = a -> Dist a   ==>   c = Dist@

  * @RChange a = a -> Rnd.T a  ==>   c = Rnd.T = IO@
-}
class C c where
  -- | returns the final randomized transition
  (~.)  :: (Fractional prob, Ord prob, Random prob, Ord a) =>
              Int       -> (a -> c a) -> Rnd.Transition prob a
  -- | returns the whole trace for a k-fold simulation
  (~..) :: (Fractional prob, Ord prob, Random prob, Ord a) =>
              (Int,Int) -> (a -> c a) -> Trace.RExpand prob a
  -- | returns the whole trace for a single simulation
  (~*.) :: (Fractional prob, Ord prob, Random prob, Ord a) =>
              (Int,Int) -> (a -> c a) -> Rnd.Transition prob a

infix 6 ~. , ~..

infix 8 ~*.


-- simulation for transitions
--
instance (Num prob, Ord prob, Random prob) => C (Dist.T prob) where
  (~.)  x = (~.)  x . Rnd.change
  (~..) x = (~..) x . Rnd.change
  (~*.) x = (~*.) x . Rnd.change


-- simulation for Rnd.change changes
--
instance C Rnd.T where
  (~.)     n  t = Rnd.dist . replicate n . t
  (~..) (k,n) t = Trace.merge . replicate k . MonadExt.walk n t
  (~*.) (k,n) t = k ~. MonadExt.iterate n t
