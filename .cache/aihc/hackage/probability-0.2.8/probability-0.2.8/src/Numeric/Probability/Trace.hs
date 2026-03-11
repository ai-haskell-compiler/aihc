-- | Tracing
module Numeric.Probability.Trace where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition   as Trans
import qualified Numeric.Probability.Random       as Rnd

import Data.List (transpose)


-- * traces of distributions

type Trace a  = [a]
type Walk a   = a -> Trace a

type Space prob a  = Trace (Dist.T prob a)
type Expand prob a = a -> Space prob a



-- for ListUtils
-- | walk is a bounded version of the predefined function iterate
walk :: Int -> Trans.Change a -> Walk a
walk n f = take n . iterate f



-- * traces of random experiments

type RTrace a  = Rnd.T (Trace a)
type RWalk a   = a -> RTrace a

type RSpace prob a  = Rnd.T (Space prob a)
type RExpand prob a = a -> RSpace prob a


{- |
'merge' converts a list of 'RTrace's
into a list of randomized distributions, i.e., an 'RSpace',
by creating a randomized distribution for each list position across all traces
-}
merge :: (Fractional prob, Ord a) =>
   [RTrace a] -> RSpace prob a
merge =
   fmap (zipListWith (Dist.norm . Dist.uniform)) . sequence


-- for ListUtils
zipListWith :: ([a] -> b) -> [[a]] -> [b]
zipListWith f = map f . transpose
