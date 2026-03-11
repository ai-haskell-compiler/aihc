{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Numeric.MCMC.Slice
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- This implementation performs slice sampling by first finding a bracket about
-- a mode (using a simple doubling heuristic), and then doing rejection
-- sampling along it.  The result is a reliable and computationally inexpensive
-- sampling routine.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `slice` transition can be used for more flexible purposes, such as
-- working with samples in memory.
--
-- See <http://people.ee.duke.edu/~lcarin/slice.pdf Neal, 2003> for the
-- definitive reference of the algorithm.

module Numeric.MCMC.Slice (
    mcmc
  , chain
  , slice

  -- * Re-exported
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Monad (replicateM)
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.Trans.State.Strict (put, get, execStateT)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Lens hiding (index)
import Data.Maybe (fromMaybe)
import Data.Sampling.Types
import Pipes hiding (next)
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability (Prob, Gen, Variate)
import qualified System.Random.MWC.Probability as MWC

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> let rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >>> withSystemRandom . asGenIO $ mcmc 3 1 [0, 0] rosenbrock
-- -3.854097694213343e-2,0.16688601288358407
-- -9.310661272172682e-2,0.2562387977415508
-- -0.48500122500661846,0.46245400501919076
mcmc
  :: (MonadIO m, PrimMonad m,
     Show (t a), FoldableWithIndex (Index (t a)) t, Ixed (t a),
     Num (IxValue (t a)), Variate (IxValue (t a)))
  => Int
  -> IxValue (t a)
  -> t a
  -> (t a -> Double)
  -> Gen (PrimState m)
  -> m ()
mcmc n radial chainPosition target gen = runEffect $
        drive radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ (liftIO . print)
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing
    chainTarget   = Target target Nothing

-- | Trace 'n' iterations of a Markov chain and collect them in a list.
--
-- >>> results <- withSystemRandom . asGenIO $ mcmc 3 1 [0, 0] rosenbrock
chain
  :: (PrimMonad m, FoldableWithIndex (Index (f a)) f, Ixed (f a)
     , Variate (IxValue (f a)), Num (IxValue (f a)))
  => Int
  -> IxValue (f a)
  -> f a
  -> (f a -> Double)
  -> Gen (PrimState m)
  -> m [Chain (f a) b]
chain n radial position target gen = runEffect $
        drive radial origin gen
    >-> collect n
  where
    ctarget = Target target Nothing

    origin = Chain {
        chainScore    = lTarget ctarget position
      , chainTunables = Nothing
      , chainTarget   = ctarget
      , chainPosition = position
      }

    collect :: Monad m => Int -> Consumer a m [a]
    collect size = lowerCodensity $
      replicateM size (lift Pipes.await)

-- A Markov chain driven by the slice transition operator.
drive
  :: (PrimMonad m, FoldableWithIndex (Index (t a)) t, Ixed (t a),
     Num (IxValue (t a)), Variate (IxValue (t a)))
  => IxValue (t a)
  -> Chain (t a) b
  -> Gen (PrimState m)
  -> Producer (Chain (t a) b) m c
drive radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (slice radial) state) prng)
    yield next
    loop next prng

-- | A slice sampling transition operator.
slice
  :: (PrimMonad m, FoldableWithIndex (Index (t a)) t, Ixed (t a),
      Num (IxValue (t a)), Variate (IxValue (t a)))
  => IxValue (t a)
  -> Transition m (Chain (t a) b)
slice step = do
  Chain _ _ position _ <- get
  ifor_ position $ \index _ -> do
    Chain {..} <- get
    let bounds = (0, exp (lTarget chainTarget chainPosition))
    height    <- lift (fmap log (MWC.uniformR bounds))

    let bracket =
          findBracket (lTarget chainTarget) index step height chainPosition

    perturbed <- lift $
      rejection (lTarget chainTarget) index bracket height chainPosition

    let perturbedScore = lTarget chainTarget perturbed
    put (Chain chainTarget perturbedScore perturbed chainTunables)

-- Find a bracket by expanding its bounds through powers of 2.
findBracket
  :: (Ord a, Ixed s, Num (IxValue s))
  => (s -> a)
  -> Index s
  -> IxValue s
  -> a
  -> s
  -> (IxValue s, IxValue s)
findBracket target index step height xs = go step xs xs where
  err = error "findBracket: invalid index -- please report this as a bug!"
  go !e !bl !br
    | target bl < height && target br < height =
        let l = fromMaybe err (bl ^? ix index)
            r = fromMaybe err (br ^? ix index)
        in  (l, r)
    | target bl < height && target br >= height =
        let br0 = expandBracketRight index step br
        in  go (2 * e) bl br0
    | target bl >= height && target br < height =
        let bl0 = expandBracketLeft index step bl
        in  go (2 * e) bl0 br
    | otherwise =
        let bl0 = expandBracketLeft index step bl
            br0 = expandBracketRight index step br
        in  go (2 * e) bl0 br0

expandBracketLeft
  :: (Ixed s, Num (IxValue s))
  => Index s
  -> IxValue s
  -> s
  -> s
expandBracketLeft = expandBracketBy (-)

expandBracketRight
  :: (Ixed s, Num (IxValue s))
  => Index s
  -> IxValue s
  -> s
  -> s
expandBracketRight = expandBracketBy (+)

expandBracketBy
  :: Ixed s
  => (IxValue s -> t -> IxValue s)
  -> Index s
  -> t
  -> s
  -> s
expandBracketBy f index step xs = xs & ix index %~ (`f` step )

-- Perform rejection sampling within the supplied bracket.
rejection
  :: (Ord a, PrimMonad m, Ixed b, Variate (IxValue b))
  => (b -> a)
  -> Index b
  -> (IxValue b, IxValue b)
  -> a
  -> b
  -> Prob m b
rejection target dimension bracket height = go where
  go zs = do
    u <- MWC.uniformR bracket
    let  updated = zs & ix dimension .~ u
    if   target updated < height
    then go updated
    else return updated

