-- | Helpful functions that should have been included in System.Random.

module System.Random.XRayCustom
  ( withRandomGenIORef
  , replicateRandom
  ) where

import Prelude

import Data.IORef
import System.Random

-- | Like 'getStdRandom', except use a given 'IORef StdGen' instead of the
-- global 'theStdGen'.
withRandomGenIORef :: (RandomGen g) => IORef g -> (g -> (a, g)) -> IO a
withRandomGenIORef ioRef f = atomicModifyIORef' ioRef (swap . f)
  where swap (x, y) = (y, x)

-- | Runs the supplied function a specified number of times, each time using
-- the new 'RandomGen' value.
replicateRandom :: (RandomGen g) => Int -> g -> (g -> (a, g)) -> ([a], g)
replicateRandom n gen f = replicateRandom' n ([], gen)
 where
  replicateRandom' n' (xs, gen')
    | n' <= 0
    = (xs, gen')
    | otherwise
    = let ~(x, gen'') = f gen' in replicateRandom' (n' - 1) (x : xs, gen'')
