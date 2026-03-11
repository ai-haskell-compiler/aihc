{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Data.Sampling.Types
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- Common types for implementing Markov Chain Monte Carlo (MCMC) algorithms.
--
-- 'Target' is a product type intended to hold a log-target density function and
-- potentially its gradient.
--
-- The 'Chain' type represents a kind of annotated parameter space.
-- Technically all that's required here is the type of the parameter space
-- itself (held here in 'chainPosition') but in practice some additional
-- information is typically useful.  Additionally there is 'chainScore' for
-- holding the most recent score of the chain, as well as the target itself for
-- implementing things like annealing.  The `chainTunables` field can be used
-- to hold arbitrary data.
--
-- One should avoid exploiting these features to do something nasty (like, say,
-- invalidating the Markov property).
--
-- The 'Transition' type permits probabilistic transitions over some state
-- space by way of the underlying 'Prob' monad.

module Data.Sampling.Types (
    Transition
  , Chain(..)
  , Target(..)
  ) where

import Control.Monad.Trans.State.Strict (StateT)
import System.Random.MWC.Probability (Prob)

-- | A generic transition operator.
--
--   Has access to randomness via the underlying 'Prob' monad.
type Transition m a = StateT a (Prob m) ()

-- | The @Chain@ type specifies the state of a Markov chain at any given
--   iteration.
data Chain a b = Chain {
    chainTarget   :: Target a
  , chainScore    :: !Double
  , chainPosition :: a
  , chainTunables :: Maybe b
  }

instance Show a => Show (Chain a b) where
  show Chain {..} = filter (`notElem` "fromList []") (show chainPosition)

-- | A @Target@ consists of a function from parameter space to the reals, as
--   well as possibly a gradient.
--
--   Most implementations assume a /log/-target, so records are named
--   accordingly.
data Target a = Target {
    lTarget  :: a -> Double
  , glTarget :: Maybe (a -> a)
  }

