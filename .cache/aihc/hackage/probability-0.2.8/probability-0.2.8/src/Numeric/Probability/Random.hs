-- | Randomized values
module Numeric.Probability.Random where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition   as Trans

import qualified System.Random as Random
import System.Random (Random, )

import Control.Applicative (Applicative(..))
import Control.Monad.Trans.State (State, state, evalState, runState, )

import qualified System.IO as IO
import Prelude hiding (print)


-- *  random generator

-- | Random values
newtype T a = Cons {decons :: State Random.StdGen a}

instance Monad T where
   return x = Cons (return x)
   Cons x >>= y =
      Cons (decons . y =<< x)

instance Functor T where
   fmap f = Cons . fmap f . decons

instance Applicative T where
   pure x = Cons (pure x)
   fm <*> m = Cons (decons fm <*> decons m)

randomR :: Random.Random a => (a, a) -> T a
randomR rng =
   Cons (state (Random.randomR rng))

{- |
Run random action in 'IO' monad.
-}
run :: T a -> IO a
run = Random.getStdRandom . runState . decons

{- |
Run random action without 'IO' using a seed.
-}
runSeed :: Random.StdGen -> T a -> a
runSeed g r = evalState (decons r) g


print :: Show a => T a -> IO ()
print r  =  IO.print =<< run r

-- instance Show (IO a) where
--   show _ = ""

pick :: (Num prob, Ord prob, Random prob) =>
   Dist.T prob a -> T a
pick d = return . Dist.selectP d =<< randomR (0,1)


-- *  random distribution

-- | Randomized distribution
type Distribution prob a = T (Dist.T prob a)

above, below :: (Num prob, Ord prob, Ord a) =>
   prob -> Distribution prob a -> Distribution prob (Dist.Select a)
above p = fmap (Dist.above p)
below p = fmap (Dist.below p)

{- |
'dist' converts a list of randomly generated values into
a distribution by taking equal weights for all values.
Thus @dist (replicate n rnd)@ simulates @rnd@ @n@ times
and returns an estimation of the distribution represented by @rnd@.
-}
dist :: (Fractional prob, Ord a) => [T a] -> Distribution prob a
dist = fmap (Dist.norm . Dist.uniform) . sequence


-- * Randomized changes

-- | random change
type Change a = a -> T a

change :: (Num prob, Ord prob, Random prob) =>
   Trans.T prob a -> Change a
change t = pick . t


-- * Randomized transitions

-- | random transition
type Transition prob a = a -> Distribution prob a

type ApproxDist a = T [a]




{-
for quickCheck

LAWS

  const . pick = random . const

-}
