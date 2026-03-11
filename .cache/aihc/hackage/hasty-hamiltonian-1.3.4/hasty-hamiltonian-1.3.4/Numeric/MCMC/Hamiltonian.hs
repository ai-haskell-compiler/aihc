{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Numeric.MCMC.Hamiltonian
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- This implementation performs Hamiltonian Monte Carlo using an identity mass
-- matrix.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `slice` transition can be used for more flexible purposes, such as
-- working with samples in memory.
--
-- See <http://arxiv.org/pdf/1206.1901.pdf Neal, 2012> for the definitive
-- reference of the algorithm.

module Numeric.MCMC.Hamiltonian (
    mcmc
  , chain
  , hamiltonian

  -- * Re-exported
  , Target(..)
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Lens hiding (index)
import Control.Monad (replicateM)
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.Primitive (PrimState, PrimMonad)
import Control.Monad.Trans.State.Strict hiding (state)
import qualified Data.Foldable as Foldable (sum)
import Data.Maybe (fromMaybe)
import Data.Sampling.Types
import Data.Traversable (for)
import Pipes hiding (for, next)
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability (Prob, Gen)
import qualified System.Random.MWC.Probability as MWC

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> withSystemRandom . asGenIO $ mcmc 10000 0.05 20 [0, 0] target
mcmc
  :: ( MonadIO m, PrimMonad m
     , Num (IxValue (t Double)), Show (t Double), Traversable t
     , FunctorWithIndex (Index (t Double)) t, Ixed (t Double)
     , IxValue (t Double) ~ Double)
  => Int
  -> Double
  -> Int
  -> t Double
  -> Target (t Double)
  -> Gen (PrimState m)
  -> m ()
mcmc n step leaps chainPosition chainTarget gen = runEffect $
        drive step leaps Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ (liftIO . print)
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing

-- | Trace 'n' iterations of a Markov chain and collect the results in a list.
--
-- >>> results <- withSystemRandom . asGenIO $ chain 1000 0.05 20 [0, 0] target
chain
  :: (PrimMonad m, Traversable f
     , FunctorWithIndex (Index (f Double)) f, Ixed (f Double)
     , IxValue (f Double) ~ Double)
  => Int
  -> Double
  -> Int
  -> f Double
  -> Target (f Double)
  -> Gen (PrimState m)
  -> m [Chain (f Double) b]
chain n step leaps position target gen = runEffect $
        drive step leaps origin gen
    >-> collect n
  where
    origin = Chain {
        chainScore    = lTarget target position
      , chainTunables = Nothing
      , chainTarget   = target
      , chainPosition = position
      }

    collect :: Monad m => Int -> Consumer a m [a]
    collect size = lowerCodensity $
      replicateM size (lift Pipes.await)

-- Drive a Markov chain.
drive
  :: (Num (IxValue (t Double)), Traversable t
     , FunctorWithIndex (Index (t Double)) t, Ixed (t Double)
     , PrimMonad m, IxValue (t Double) ~ Double)
  => Double
  -> Int
  -> Chain (t Double) b
  -> Gen (PrimState m)
  -> Producer (Chain (t Double) b) m c
drive step leaps = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (hamiltonian step leaps) state) prng)
    yield next
    loop next prng

-- | A Hamiltonian transition operator.
hamiltonian
  :: (Num (IxValue (t Double)), Traversable t
     , FunctorWithIndex (Index (t Double)) t, Ixed (t Double), PrimMonad m
     , IxValue (t Double) ~ Double)
  => Double -> Int -> Transition m (Chain (t Double) b)
hamiltonian e l = do
  Chain {..} <- get
  r0 <- lift (for chainPosition (const MWC.standardNormal))
  zc <- lift (MWC.uniform :: PrimMonad m => Prob m Double)
  let (q, r) = leapfrogIntegrator chainTarget e l (chainPosition, r0)
      perturbed      = nextState chainTarget (chainPosition, q) (r0, r) zc
      perturbedScore = lTarget chainTarget perturbed
  put (Chain chainTarget perturbedScore perturbed chainTunables)

-- Calculate the next state of the chain.
nextState
  :: (Foldable s, Foldable t, FunctorWithIndex (Index (t Double)) t
     , FunctorWithIndex (Index (s Double)) s, Ixed (s Double)
     , Ixed (t Double), IxValue (t Double) ~ Double
     , IxValue (s Double) ~ Double)
  => Target b
  -> (b, b)
  -> (s Double, t Double)
  -> Double
  -> b
nextState target position momentum z
    | z < pAccept = snd position
    | otherwise   = fst position
  where
    pAccept = acceptProb target position momentum

-- Calculate the acceptance probability of a proposed moved.
acceptProb
  :: (Foldable t, Foldable s, FunctorWithIndex (Index (t Double)) t
     , FunctorWithIndex (Index (s Double)) s, Ixed (t Double)
     , Ixed (s Double), IxValue (t Double) ~ Double
     , IxValue (s Double) ~ Double)
  => Target a
  -> (a, a)
  -> (s Double, t Double)
  -> Double
acceptProb target (q0, q1) (r0, r1) = exp . min 0 $
  auxilliaryTarget target (q1, r1) - auxilliaryTarget target (q0, r0)

-- A momentum-augmented target.
auxilliaryTarget
  :: (Foldable t, FunctorWithIndex (Index (t Double)) t
     , Ixed (t Double), IxValue (t Double) ~ Double)
  => Target a
  -> (a, t Double)
  -> Double
auxilliaryTarget target (t, r) = f t - 0.5 * innerProduct r r where
  f = lTarget target

innerProduct
  :: (Num (IxValue s), Foldable t, FunctorWithIndex (Index s) t, Ixed s)
  => t (IxValue s) -> s -> IxValue s
innerProduct xs ys = Foldable.sum $ gzipWith (*) xs ys

-- A container-generic zipwith.
gzipWith
  :: (FunctorWithIndex (Index s) f, Ixed s)
  => (a -> IxValue s -> b) -> f a -> s -> f b
gzipWith f xs ys = imap (\j x -> f x (fromMaybe err (ys ^? ix j))) xs where
  err = error "gzipWith: invalid index"

-- The leapfrog or Stormer-Verlet integrator.
leapfrogIntegrator
  :: (Num (IxValue (f Double))
     , FunctorWithIndex (Index (f Double)) t
     , FunctorWithIndex (Index (t Double)) f
     , Ixed (f Double), Ixed (t Double)
     , IxValue (f Double) ~ Double
     , IxValue (t Double) ~ Double)
  => Target (f Double)
  -> Double
  -> Int
  -> (f Double, t (IxValue (f Double)))
  -> (f Double, t (IxValue (f Double)))
leapfrogIntegrator target e l (q0, r0) = go q0 r0 l where
  go q r 0 = (q, r)
  go q r n = go q1 r1 (pred n) where
    (q1, r1) = leapfrog target e (q, r)

-- A single leapfrog step.
leapfrog
  :: (Num (IxValue (f Double))
     , FunctorWithIndex (Index (f Double)) t
     , FunctorWithIndex (Index (t Double)) f
     , Ixed (t Double), Ixed (f Double)
     , IxValue (f Double) ~ Double, IxValue (t Double) ~ Double)
  => Target (f Double)
  -> Double
  -> (f Double, t (IxValue (f Double)))
  -> (f Double, t (IxValue (f Double)))
leapfrog target e (q, r) = (qf, rf) where
  rm = adjustMomentum target e (q, r)
  qf = adjustPosition e (rm, q)
  rf = adjustMomentum target e (qf, rm)

adjustMomentum
  :: (Functor f, Num (IxValue (f Double))
     , FunctorWithIndex (Index (f Double)) t, Ixed (f Double))
  => Target (f Double)
  -> Double
  -> (f Double, t (IxValue (f Double)))
  -> t (IxValue (f Double))
adjustMomentum target e (q, r) = r .+ ((0.5 * e) .* g q) where
  g   = fromMaybe err (glTarget target)
  err = error "adjustMomentum: no gradient provided"

adjustPosition
  :: (Functor f, Num (IxValue (f Double))
     , FunctorWithIndex (Index (f Double)) t, Ixed (f Double))
  => Double
  -> (f Double, t (IxValue (f Double)))
  -> t (IxValue (f Double))
adjustPosition e (r, q) = q .+ (e .* r)

-- Scalar-vector product.
(.*) :: (Num a, Functor f) => a -> f a -> f a
z .* xs = fmap (* z) xs

-- Vector addition.
(.+)
  :: (Num (IxValue t), FunctorWithIndex (Index t) f, Ixed t)
  => f (IxValue t)
  -> t
  -> f (IxValue t)
(.+) = gzipWith (+)

