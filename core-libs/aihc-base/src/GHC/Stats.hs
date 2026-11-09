-- | Runtime statistics.
--
-- The aihc runtime does not collect these statistics yet.
-- 'getRTSStatsEnabled' returns 'False' and 'getRTSStats' returns zero values.
module GHC.Stats
  ( RTSStats (..),
    GCDetails (..),
    RtsTime,
    getRTSStats,
    getRTSStatsEnabled,
  )
where

import Data.Int (Int64)
import Data.Word (Word32, Word64)
import Prelude

-- | Time values from the RTS, in nanoseconds.
type RtsTime = Int64

-- | Statistics about the runtime system.
data RTSStats = RTSStats
  { gcs :: Word32,
    major_gcs :: Word32,
    allocated_bytes :: Word64,
    max_live_bytes :: Word64,
    max_large_objects_bytes :: Word64,
    max_compact_bytes :: Word64,
    max_slop_bytes :: Word64,
    max_mem_in_use_bytes :: Word64,
    cumulative_live_bytes :: Word64,
    copied_bytes :: Word64,
    par_copied_bytes :: Word64,
    cumulative_par_max_copied_bytes :: Word64,
    cumulative_par_balanced_copied_bytes :: Word64,
    init_cpu_ns :: RtsTime,
    init_elapsed_ns :: RtsTime,
    mutator_cpu_ns :: RtsTime,
    mutator_elapsed_ns :: RtsTime,
    gc_cpu_ns :: RtsTime,
    gc_elapsed_ns :: RtsTime,
    cpu_ns :: RtsTime,
    elapsed_ns :: RtsTime,
    nonmoving_gc_sync_cpu_ns :: RtsTime,
    nonmoving_gc_sync_elapsed_ns :: RtsTime,
    nonmoving_gc_sync_max_elapsed_ns :: RtsTime,
    nonmoving_gc_cpu_ns :: RtsTime,
    nonmoving_gc_elapsed_ns :: RtsTime,
    nonmoving_gc_max_elapsed_ns :: RtsTime,
    gc :: GCDetails
  }
  deriving (Read, Show)

-- | Statistics about one garbage collection.
data GCDetails = GCDetails
  { gcdetails_gen :: Word32,
    gcdetails_threads :: Word32,
    gcdetails_allocated_bytes :: Word64,
    gcdetails_live_bytes :: Word64,
    gcdetails_large_objects_bytes :: Word64,
    gcdetails_compact_bytes :: Word64,
    gcdetails_slop_bytes :: Word64,
    gcdetails_mem_in_use_bytes :: Word64,
    gcdetails_copied_bytes :: Word64,
    gcdetails_par_max_copied_bytes :: Word64,
    gcdetails_par_balanced_copied_bytes :: Word64,
    gcdetails_block_fragmentation_bytes :: Word64,
    gcdetails_sync_elapsed_ns :: RtsTime,
    gcdetails_cpu_ns :: RtsTime,
    gcdetails_elapsed_ns :: RtsTime,
    gcdetails_nonmoving_gc_sync_cpu_ns :: RtsTime,
    gcdetails_nonmoving_gc_sync_elapsed_ns :: RtsTime
  }
  deriving (Read, Show)

-- | Return 'True' if the RTS collects statistics.
-- The aihc runtime does not collect them, thus this is always 'False'.
getRTSStatsEnabled :: IO Bool
getRTSStatsEnabled = return False

-- | Return the current runtime statistics.
-- The aihc runtime does not collect them, thus all values are zero.
getRTSStats :: IO RTSStats
getRTSStats = return emptyRTSStats

emptyRTSStats :: RTSStats
emptyRTSStats =
  RTSStats
    { gcs = 0,
      major_gcs = 0,
      allocated_bytes = 0,
      max_live_bytes = 0,
      max_large_objects_bytes = 0,
      max_compact_bytes = 0,
      max_slop_bytes = 0,
      max_mem_in_use_bytes = 0,
      cumulative_live_bytes = 0,
      copied_bytes = 0,
      par_copied_bytes = 0,
      cumulative_par_max_copied_bytes = 0,
      cumulative_par_balanced_copied_bytes = 0,
      init_cpu_ns = 0,
      init_elapsed_ns = 0,
      mutator_cpu_ns = 0,
      mutator_elapsed_ns = 0,
      gc_cpu_ns = 0,
      gc_elapsed_ns = 0,
      cpu_ns = 0,
      elapsed_ns = 0,
      nonmoving_gc_sync_cpu_ns = 0,
      nonmoving_gc_sync_elapsed_ns = 0,
      nonmoving_gc_sync_max_elapsed_ns = 0,
      nonmoving_gc_cpu_ns = 0,
      nonmoving_gc_elapsed_ns = 0,
      nonmoving_gc_max_elapsed_ns = 0,
      gc = emptyGCDetails
    }

emptyGCDetails :: GCDetails
emptyGCDetails =
  GCDetails
    { gcdetails_gen = 0,
      gcdetails_threads = 0,
      gcdetails_allocated_bytes = 0,
      gcdetails_live_bytes = 0,
      gcdetails_large_objects_bytes = 0,
      gcdetails_compact_bytes = 0,
      gcdetails_slop_bytes = 0,
      gcdetails_mem_in_use_bytes = 0,
      gcdetails_copied_bytes = 0,
      gcdetails_par_max_copied_bytes = 0,
      gcdetails_par_balanced_copied_bytes = 0,
      gcdetails_block_fragmentation_bytes = 0,
      gcdetails_sync_elapsed_ns = 0,
      gcdetails_cpu_ns = 0,
      gcdetails_elapsed_ns = 0,
      gcdetails_nonmoving_gc_sync_cpu_ns = 0,
      gcdetails_nonmoving_gc_sync_elapsed_ns = 0
    }
