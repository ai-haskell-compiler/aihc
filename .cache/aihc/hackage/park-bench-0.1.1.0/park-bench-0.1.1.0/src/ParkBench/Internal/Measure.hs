module ParkBench.Internal.Measure
  ( measure,
  )
where

import qualified GHC.Stats as GHC
import ParkBench.Internal.Prelude
import ParkBench.Internal.RtsStats (RtsStats (RtsStats))
import ParkBench.Internal.Statistics (Timed (..))
import System.Mem (performGC)

-- | Measure the time/memory usage of an IO action.
measure :: IO a -> IO (Timed RtsStats)
measure action = do
  performGC
  s0 <- GHC.getRTSStats
  _ <- action
  s1 <- GHC.getRTSStats
  -- Perform a major GC to update a few stats that are only accurate after a major GC.
  -- But the latest GC before collecting `s1` might have happened to be major, so check that first.
  s2 <-
    if GHC.gcdetails_gen (GHC.gc s0) == GHC.gcdetails_gen (GHC.gc s1)
      then pure s1
      else do
        performGC
        GHC.getRTSStats

  let elapsed_ns :: Rational
      elapsed_ns =
        toRational (GHC.elapsed_ns s1 - GHC.elapsed_ns s0)

  pure
    Timed
      { nanoseconds = elapsed_ns,
        value =
          RtsStats
            (toRational (GHC.allocated_bytes s1 - GHC.allocated_bytes s0))
            (toRational (GHC.copied_bytes s1 - GHC.copied_bytes s0))
            (toRational (GHC.cpu_ns s1 - GHC.cpu_ns s0))
            (toRational (GHC.cumulative_live_bytes s2 - GHC.cumulative_live_bytes s0))
            (toRational (GHC.cumulative_par_balanced_copied_bytes s1 - GHC.cumulative_par_balanced_copied_bytes s0))
            elapsed_ns
            (toRational (GHC.gc_cpu_ns s1 - GHC.gc_cpu_ns s0))
            (toRational (GHC.gc_elapsed_ns s1 - GHC.gc_elapsed_ns s0))
            (toRational (GHC.gcs s1 - GHC.gcs s0))
            (toRational (GHC.major_gcs s1 - GHC.major_gcs s0))
            (toRational (max (GHC.max_compact_bytes s2) (GHC.max_compact_bytes s0)))
            (toRational (max (GHC.max_large_objects_bytes s2) (GHC.max_large_objects_bytes s0)))
            (toRational (max (GHC.max_live_bytes s2) (GHC.max_live_bytes s0)))
            (toRational (max (GHC.max_mem_in_use_bytes s1) (GHC.max_mem_in_use_bytes s2)))
            (toRational (max (GHC.max_slop_bytes s2) (GHC.max_slop_bytes s0)))
            (toRational (GHC.mutator_cpu_ns s1 - GHC.mutator_cpu_ns s0))
            (toRational (GHC.mutator_elapsed_ns s1 - GHC.mutator_elapsed_ns s0))
            (toRational (GHC.par_copied_bytes s1 - GHC.par_copied_bytes s0))
      }
