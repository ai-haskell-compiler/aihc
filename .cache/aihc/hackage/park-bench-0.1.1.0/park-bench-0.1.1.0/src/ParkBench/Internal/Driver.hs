module ParkBench.Internal.Driver
  ( BenchmarkConfig (..),
    benchmark,
    LiveBenchmark,
    sampleLiveBenchmark,
    LiveBenchmarks,
    makeLiveBenchmarks,
    stepLiveBenchmarks,
  )
where

import Data.Foldable (toList)
import Data.IORef
import ParkBench.Internal.Array1 (Array1)
import qualified ParkBench.Internal.Array1 as Array1
import ParkBench.Internal.Benchable (Benchable)
import qualified ParkBench.Internal.Benchable as Benchable
import ParkBench.Internal.Prelude
import ParkBench.Internal.RtsStats (RtsStats)
import ParkBench.Internal.Statistics

data BenchmarkConfig = BenchmarkConfig
  { runlen :: !Rational
  }

data LiveBenchmark a = LiveBenchmark
  { _liveBenchmarkSample :: !(IO (Estimate a)),
    _liveBenchmarkPull :: !(Pull a)
  }

sampleLiveBenchmark :: LiveBenchmark a -> IO (Estimate a)
sampleLiveBenchmark =
  _liveBenchmarkSample
{-# INLINE sampleLiveBenchmark #-}

benchmark :: forall a. Roll a => BenchmarkConfig -> Benchable (Timed a) -> IO (LiveBenchmark a)
benchmark BenchmarkConfig {runlen} benchable = do
  e0 <- initialEstimate <$> Benchable.run benchable 1
  estimateRef <- newIORef e0
  let go :: Estimate a -> Pull a
      go oldEstimate =
        Pull (elapsed oldEstimate) do
          let newIters = itersInNanoseconds oldEstimate runlen
          newTime <- Benchable.run benchable newIters
          let !newEstimate = updateEstimate newIters newTime oldEstimate
          writeIORef estimateRef newEstimate
          pure (go newEstimate)
  pure
    LiveBenchmark
      { _liveBenchmarkSample = readIORef estimateRef,
        _liveBenchmarkPull = go e0
      }
{-# SPECIALIZE benchmark :: BenchmarkConfig -> Benchable (Timed RtsStats) -> IO (LiveBenchmark RtsStats) #-}

-- Given this latest estimate, how many iters could we run in the given number of nanoseconds?
itersInNanoseconds :: Estimate a -> Rational -> Word64
itersInNanoseconds Estimate {mean, samples} nanos =
  max 1 (min samples (floor (nanos / nanoseconds mean)))

data Pull a = Pull
  { -- amount of time this pull has gotten
    pullElapsed :: {-# UNPACK #-} !Rational,
    pullNext :: !(IO (Pull a))
  }

isMoreUrgentThan :: Pull a -> Pull a -> Bool
isMoreUrgentThan p0 p1 =
  pullElapsed p0 < pullElapsed p1

-- | A @LiveBenchmarks@ represents the suspended state of a collection of 1+ benchmarks.
data LiveBenchmarks a
  = -- Most benchmark runs are probably only comparing 1-3 things, so we optimize those cases.
    P1 !(Pull a)
  | P2 !(Pull a) !(Pull a)
  | P3 !(Pull a) !(Pull a) !(Pull a)
  | -- invariant: 4+ elements
    Pn_ ![Pull a]

pattern Pn :: Pull a -> [Pull a] -> LiveBenchmarks a
pattern Pn p ps <- Pn_ (p : ps)

{-# COMPLETE P1, P2, P3, Pn #-}

-- | Construct a 'LiveBenchmarks' from a non-empty array of 'LiveBenchmark'.
makeLiveBenchmarks :: Array1 (LiveBenchmark a) -> LiveBenchmarks a
makeLiveBenchmarks (fmap _liveBenchmarkPull -> xs)
  | n == 1 = P1 (Array1.get 0 xs)
  | n == 2 = P2 (Array1.get 0 xs) (Array1.get 1 xs)
  | n == 3 = P3 (Array1.get 0 xs) (Array1.get 1 xs) (Array1.get 2 xs)
  | otherwise = Pn_ (toList xs)
  where
    n = length xs

-- | Step forward a 'LiveBenchmarks', which blocks until the benchmark that has heretofore accumulated the smallest
-- amount of runtime finishes one more run.
--
-- Returns the 'LiveBenchmarks' to use next time, which reflects the latest benchmark run that just completed.
stepLiveBenchmarks :: LiveBenchmarks a -> IO (LiveBenchmarks a)
stepLiveBenchmarks = \case
  P1 p0 -> do
    x0 <- pullNext p0
    pure (P1 x0)
  P2 p0 x1 -> do
    x0 <- pullNext p0
    pure
      if x1 `isMoreUrgentThan` x0
        then P2 x1 x0
        else P2 x0 x1
  P3 p0 x1 x2 -> do
    x0 <- pullNext p0
    pure
      if x1 `isMoreUrgentThan` x0
        then
          if x2 `isMoreUrgentThan` x0
            then P3 x1 x2 x0
            else P3 x1 x0 x2
        else P3 x0 x1 x2
  Pn p0 xs -> do
    x0 <- pullNext p0
    pure (Pn_ (insertPull x0 xs))

insertPull :: Pull a -> [Pull a] -> [Pull a]
insertPull x0 = \case
  [] -> [x0]
  x1 : xs ->
    if x0 `isMoreUrgentThan` x1
      then x0 : x1 : xs
      else x1 : insertPull x0 xs
