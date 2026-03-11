module ParkBench
  ( Benchmark,
    benchmark,
    function,
    action,
    file,
  )
where

import Control.Concurrent (threadDelay)
import Data.Function ((&))
import qualified Data.List.NonEmpty as List1
import Data.Maybe (catMaybes)
import qualified Data.Text as Text (pack)
import ParkBench.Internal.Array1 (Array1)
import qualified ParkBench.Internal.Array1 as Array1
import ParkBench.Internal.Benchable (Benchable)
import qualified ParkBench.Internal.Benchable as Benchable
import ParkBench.Internal.Config (Config (Config))
import qualified ParkBench.Internal.Config as Config
import qualified ParkBench.Internal.Driver as Driver
import qualified ParkBench.Internal.Measure as Measure
import ParkBench.Internal.Named (Named (Named))
import ParkBench.Internal.Prelude
import ParkBench.Internal.Pretty (renderTable)
import ParkBench.Internal.Render (estimatesToTable)
import ParkBench.Internal.RtsStats (RtsStats)
import ParkBench.Internal.Statistics (Estimate)
import ParkBench.Internal.Terminal (renderToTerminal, withTerminal)

-- A "benchmark blueprint" sum type, that captures the basic shape of a benchmark, which is either "in memory" (i.e.
-- defined with 'function' or 'action', to be run live), or read from a file (and thus static).
data B a b
  = MemoryBenchmark a
  | FileBenchmark b

------------------------------------------------------------------------------------------------------------------------

-- Here we walk through the livecycle of a benchmark. It starts out "unresolved", because we (may) have a file paths
-- from a user from which we want to parse some previously-saved estimate(s).
--
-- If anything goes wrong (file doesn't exist, data doesn't parse, etc) we just map that to Nothing and keep going.

-- | A single benchmark.
newtype Benchmark
  = Benchmark UnresolvedBenchmarks

type UnresolvedBenchmarks =
  B (Named (Benchable ())) FilePath

type ResolvedBenchmarks =
  B (Named (Benchable ())) (Array1 (Named (Estimate RtsStats)))

type ResolvedBenchmark =
  B (Benchable ()) (Estimate RtsStats)

type ActivatedBenchmark =
  B (Driver.LiveBenchmark RtsStats) (Estimate RtsStats)

resolveBenchmarks :: UnresolvedBenchmarks -> IO (Maybe ResolvedBenchmarks)
resolveBenchmarks = \case
  MemoryBenchmark benchable -> pure (Just (MemoryBenchmark benchable))
  -- FIXME try reading file
  FileBenchmark _path -> pure Nothing

-- Next, we slightly munge the data by flattening each file (each containing 1+ benchmarks) into a flat list. Each
-- element in this flattened list corresponds to one benchmark (either "live" / to-be-run, or "dead" / parsed-from-a-
-- file), so each has a name, so we can pull the Named type wrapper out of the type.

flattenResolvedBenchmarks :: ResolvedBenchmarks -> [Named ResolvedBenchmark]
flattenResolvedBenchmarks = \case
  MemoryBenchmark benchable -> [MemoryBenchmark <$> benchable]
  FileBenchmark estimates -> fmap FileBenchmark <$> Array1.toList estimates

-- And finally, we "activate" a benchmark, which means (in the case of a "live" / to-be-run benchmark) just turning our
-- 'Benchable ()' into a 'LiveBenchmark RtsStats'. This makes it ready to be tugged on and updated.
--
-- Activating a "dead" / parsed-from-a-file benchmark does nothing.

activateBenchmark :: Driver.BenchmarkConfig -> ResolvedBenchmark -> IO ActivatedBenchmark
activateBenchmark config = \case
  MemoryBenchmark benchable ->
    MemoryBenchmark <$> Driver.benchmark config (Benchable.mapIO Measure.measure benchable)
  FileBenchmark estimate -> pure (FileBenchmark estimate)

-- Whew! Benchmark lifecycle walkthrough over.

------------------------------------------------------------------------------------------------------------------------

-- | Run a collection of benchmarks.
benchmark ::
  -- | ⠀
  [Benchmark] ->
  IO void
benchmark (coerce @[Benchmark] @[UnresolvedBenchmarks] -> benchmarks0) = do
  benchmarks <- catMaybes <$> traverse resolveBenchmarks benchmarks0
  case List1.nonEmpty benchmarks of
    Nothing -> forever (threadDelay maxBound)
    Just resolvedBenchmarks0 ->
      resolvedBenchmarks0
        & foldMap flattenResolvedBenchmarks
        & Array1.unsafeFromList
        & benchmark_

benchmark_ :: Array1 (Named ResolvedBenchmark) -> IO void
benchmark_ resolvedBenchmarks = do
  config <- Config.getFromEnv
  withRenderEstimatesToTerminal \renderEstimatesToTerminal -> do
    activatedBenchmarks <- traverse (traverse (activateBenchmark (makeBenchmarkConfig config))) resolvedBenchmarks
    let maybeLiveBenchmarks =
          activatedBenchmarks
            & Array1.mapMaybe \case
              Named _name (MemoryBenchmark liveBenchmark) -> Just liveBenchmark
              Named _name (FileBenchmark _) -> Nothing
    let maybeFileBenchmarks =
          activatedBenchmarks
            & Array1.mapMaybe \case
              Named _name (MemoryBenchmark _) -> Nothing
              Named name (FileBenchmark estimate) -> Just (Named name estimate)
    case (maybeLiveBenchmarks, maybeFileBenchmarks) of
      (Just liveBenchmarks0, _) ->
        loopForever (Driver.makeLiveBenchmarks liveBenchmarks0) \liveBenchmarks -> do
          estimates <-
            activatedBenchmarks
              & traverse
                ( traverse \case
                    MemoryBenchmark liveBenchmark -> Driver.sampleLiveBenchmark liveBenchmark
                    FileBenchmark estimate -> pure estimate
                )
          renderEstimatesToTerminal estimates
          Driver.stepLiveBenchmarks liveBenchmarks
      (Nothing, Just estimates) -> do
        renderEstimatesToTerminal estimates
        forever (threadDelay maxBound)
      (Nothing, Nothing) -> undefined -- impossible: no live or file benchmarks?

makeBenchmarkConfig :: Config -> Driver.BenchmarkConfig
makeBenchmarkConfig Config {runlen} =
  Driver.BenchmarkConfig {runlen}

withRenderEstimatesToTerminal :: ((Array1 (Named (Estimate RtsStats)) -> IO ()) -> IO a) -> IO a
withRenderEstimatesToTerminal f =
  withTerminal \terminal ->
    f (renderToTerminal terminal . renderTable . estimatesToTable)

loopForever :: forall a void. a -> (a -> IO a) -> IO void
loopForever x0 once =
  let loop :: a -> IO void
      loop x = do
        y <- once x
        loop y
   in loop x0

-- | Benchmark a function. The result is evaluated to weak head normal form.
function ::
  -- | ⠀
  String ->
  -- | ⠀
  (a -> b) ->
  -- | ⠀
  a ->
  Benchmark
function name f x =
  Benchmark (MemoryBenchmark (Named (Text.pack name) (Benchable.function f x)))

-- | Benchmark an @IO@ action. The result is evaluated to weak head normal form.
action ::
  -- | ⠀
  String ->
  -- | ⠀
  IO a ->
  Benchmark
action name x =
  Benchmark (MemoryBenchmark (Named (Text.pack name) (Benchable.action x)))

file ::
  -- | ⠀
  FilePath ->
  Benchmark
file path =
  Benchmark (FileBenchmark path)
