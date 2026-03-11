-- |
-- Module      :  ELynx.Simulate.MarkovProcess
-- Description :  Markov process helpers
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 24 09:02:25 2019.
module ELynx.Simulate.MarkovProcess
  ( ProbMatrix,
    State,
    probMatrix,
    jump,
  )
where

import ELynx.MarkovProcess.RateMatrix
import Numeric.LinearAlgebra
import System.Random.MWC.Distributions
import System.Random.Stateful

-- | A probability matrix, P_ij(t) = Pr (X_t = j | X_0 = i).
type ProbMatrix = Matrix R

-- | Make type signatures a little clearer.
type State = Int

-- | The important matrix that gives the probabilities to move from one state to
-- another in a specific time (branch length).
probMatrix :: RateMatrix -> Double -> ProbMatrix
probMatrix q t
  | n /= m = error "probMatrix: Matrix is not square."
  | t == 0 = ident n
  | t < 0 = error "probMatrix: Time is negative."
  | otherwise = expm $ scale t q
  where
    n = rows q
    m = cols q

-- | Move from a given state to a new one according to a transition probability
-- matrix .
--
-- This function is the bottleneck of the simulator and takes up most of the
-- computation time.
jump :: (StatefulGen g m) => State -> ProbMatrix -> g -> m State
jump i p = categorical (p ! i)

-- XXX: Maybe for later, use condensed tables.
--
-- Write storable instance, compilation is really slow otherwise. instance
-- Storable (Int, R) where sizeOf (x, y) = sizeOf x + sizeOf y
--
-- Do not generate table for each jump.
--
-- jump :: StatefulGen g m => State -> ProbMatrix -> g -> m State
-- jump i p = genFromTable table
--   where
--     ws = toList $ p ! i
--     vsAndWs = fromList [ (v, w) | (v, w) <- zip [(0 :: Int) ..] ws
--                                 , w > 0 ]
--     table = tableFromProbabilities vsAndWs

-- -- | Perform N jumps from a given state and according to a transition
-- -- probability matrix transformed to a list of generators. This implementation
-- -- uses 'foldM' and I am not sure how to access or store the actual chain. This
-- -- could be done by an equivalent of 'scanl' for general monads, which I was
-- -- unable to find. This function is neat, but will most likely not be needed.
-- -- However, it is instructive and is left in place.
-- jumpN :: (MonadRandom m) => State -> [Generator State] -> Int -> m State
-- jumpN s p n = foldM jump s (replicate n p)
