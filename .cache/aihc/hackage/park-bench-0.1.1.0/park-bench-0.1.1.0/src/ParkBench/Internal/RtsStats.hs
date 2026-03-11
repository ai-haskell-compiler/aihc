-- | RTS stats type.
module ParkBench.Internal.RtsStats
  ( RtsStats (RtsStats),
    allocated_bytes,
    allocated_bytes_per_second,
    average_live_data,
    elapsed_ns,
    cumulative_live_bytes,
    cumulative_par_balanced_copied_bytes,
    copied_bytes,
    max_mem_in_use_bytes,
    major_gcs,
    gcs,
    par_copied_bytes,
    cpu_ns,
    gc_average_ns,
    gc_cpu_ns,
    gc_cpu_percent,
    gc_elapsed_ns,
    max_slop_bytes,
    gc_wall_percent,
    max_compact_bytes,
    max_large_objects_bytes,
    max_live_bytes,
    max_normal_objects_bytes,
    minor_gcs,
    mut_cpu_percent,
    mut_wall_percent,
    mutator_cpu_ns,
    mutator_elapsed_ns,
    work_balance,
  )
where

import ParkBench.Internal.Prelude
import ParkBench.Internal.Statistics

-- | RTS stats type.
--
-- This type is intentionally not a record, because it's kind of large, and generated record accessors cause quadratic
-- time to compile.
--
-- The hand-written accessors do, too, but we don't need the generated setters at all, so there are some savings by
-- writing it all out by hand.
--
-- TODO nonmoving_gc_*
data RtsStats
  = RtsStats
      {-# UNPACK #-} !Rational -- allocated_bytes
      {-# UNPACK #-} !Rational -- copied_bytes
      {-# UNPACK #-} !Rational -- cpu_ns
      {-# UNPACK #-} !Rational -- cumulative_live_bytes, requires major gc
      {-# UNPACK #-} !Rational -- cumulative_par_balanced_copied_bytes
      {-# UNPACK #-} !Rational -- elapsed_ns
      {-# UNPACK #-} !Rational -- gc_cpu_ns
      {-# UNPACK #-} !Rational -- gc_elapsed_ns
      {-# UNPACK #-} !Rational -- gcs
      {-# UNPACK #-} !Rational -- major_gcs
      {-# UNPACK #-} !Rational -- max_compact_bytes, requires major gc
      {-# UNPACK #-} !Rational -- max_large_objects_bytes, requires major gc
      {-# UNPACK #-} !Rational -- max_live_bytes, requires major gc
      {-# UNPACK #-} !Rational -- max_mem_in_use_bytes
      {-# UNPACK #-} !Rational -- max_slop_bytes, requires major gc
      {-# UNPACK #-} !Rational -- mutator_cpu_ns
      {-# UNPACK #-} !Rational -- mutator_elapsed_ns
      {-# UNPACK #-} !Rational -- par_copied_bytes

instance Roll RtsStats where
  roll
    step
    ( RtsStats
        allocated_bytes0
        copied_bytes0
        cpu_ns0
        cumulative_live_bytes0
        cumulative_par_balanced_copied_bytes0
        elapsed_ns0
        gc_cpu_ns0
        gc_elapsed_ns0
        gcs0
        major_gcs0
        max_compact_bytes0
        max_large_objects_bytes0
        max_live_bytes0
        max_mem_in_use_bytes0
        max_slop_bytes0
        mutator_cpu_ns0
        mutator_elapsed_ns0
        par_copied_bytes0
      )
    ( RtsStats
        allocated_bytes1
        copied_bytes1
        cpu_ns1
        cumulative_live_bytes1
        cumulative_par_balanced_copied_bytes1
        elapsed_ns1
        gc_cpu_ns1
        gc_elapsed_ns1
        gcs1
        major_gcs1
        max_compact_bytes1
        max_large_objects_bytes1
        max_live_bytes1
        max_mem_in_use_bytes1
        max_slop_bytes1
        mutator_cpu_ns1
        mutator_elapsed_ns1
        par_copied_bytes1
      ) =
      RtsStats
        (step allocated_bytes0 allocated_bytes1)
        (step copied_bytes0 copied_bytes1)
        (step cpu_ns0 cpu_ns1)
        (step cumulative_live_bytes0 cumulative_live_bytes1)
        (step cumulative_par_balanced_copied_bytes0 cumulative_par_balanced_copied_bytes1)
        (step elapsed_ns0 elapsed_ns1)
        (step gc_cpu_ns0 gc_cpu_ns1)
        (step gc_elapsed_ns0 gc_elapsed_ns1)
        (step gcs0 gcs1)
        (step major_gcs0 major_gcs1)
        (max max_compact_bytes0 max_compact_bytes1)
        (max max_large_objects_bytes0 max_large_objects_bytes1)
        (max max_live_bytes0 max_live_bytes1)
        (max max_mem_in_use_bytes0 max_mem_in_use_bytes1)
        (max max_slop_bytes0 max_slop_bytes1)
        (step mutator_cpu_ns0 mutator_cpu_ns1)
        (step mutator_elapsed_ns0 mutator_elapsed_ns1)
        (step par_copied_bytes0 par_copied_bytes1)

allocated_bytes :: RtsStats -> Rational
allocated_bytes (RtsStats n _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = n

allocated_bytes_per_second :: RtsStats -> Rational
allocated_bytes_per_second s =
  allocated_bytes s `divide` (elapsed_ns s / 1_000_000_000)

average_live_data :: RtsStats -> Rational
average_live_data s =
  -- If the thing didn't even perform 1 major GC, we still performed one ourselves to get accurate an accurate
  -- cumulative_live_bytes count. So, use 1 as the denominator in this case, too.
  cumulative_live_bytes s / max 1 (major_gcs s)

copied_bytes :: RtsStats -> Rational
copied_bytes (RtsStats _ n _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = n

cpu_ns :: RtsStats -> Rational
cpu_ns (RtsStats _ _ n _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = n

cumulative_live_bytes :: RtsStats -> Rational
cumulative_live_bytes (RtsStats _ _ _ n _ _ _ _ _ _ _ _ _ _ _ _ _ _) = n

cumulative_par_balanced_copied_bytes :: RtsStats -> Rational
cumulative_par_balanced_copied_bytes (RtsStats _ _ _ _ n _ _ _ _ _ _ _ _ _ _ _ _ _) = n

elapsed_ns :: RtsStats -> Rational
elapsed_ns (RtsStats _ _ _ _ _ n _ _ _ _ _ _ _ _ _ _ _ _) = n

gc_average_ns :: RtsStats -> Rational
gc_average_ns s =
  gc_elapsed_ns s `divide` gcs s

gc_cpu_ns :: RtsStats -> Rational
gc_cpu_ns (RtsStats _ _ _ _ _ _ n _ _ _ _ _ _ _ _ _ _ _) = n

gc_cpu_percent :: RtsStats -> Rational
gc_cpu_percent s =
  gc_cpu_ns s `divide` cpu_ns s

gc_elapsed_ns :: RtsStats -> Rational
gc_elapsed_ns (RtsStats _ _ _ _ _ _ _ n _ _ _ _ _ _ _ _ _ _) =
  n

gc_wall_percent :: RtsStats -> Rational
gc_wall_percent s =
  gc_elapsed_ns s `divide` elapsed_ns s

gcs :: RtsStats -> Rational
gcs (RtsStats _ _ _ _ _ _ _ _ n _ _ _ _ _ _ _ _ _) =
  n

major_gcs :: RtsStats -> Rational
major_gcs (RtsStats _ _ _ _ _ _ _ _ _ n _ _ _ _ _ _ _ _) =
  n

max_compact_bytes :: RtsStats -> Rational
max_compact_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ n _ _ _ _ _ _ _) =
  n

max_large_objects_bytes :: RtsStats -> Rational
max_large_objects_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ _ n _ _ _ _ _ _) =
  n

max_live_bytes :: RtsStats -> Rational
max_live_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ n _ _ _ _ _) =
  n

max_mem_in_use_bytes :: RtsStats -> Rational
max_mem_in_use_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ _ n _ _ _ _) =
  n

max_normal_objects_bytes :: RtsStats -> Rational
max_normal_objects_bytes s =
  max_live_bytes s - max_compact_bytes s - max_large_objects_bytes s

max_slop_bytes :: RtsStats -> Rational
max_slop_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ _ _ n _ _ _) =
  n

minor_gcs :: RtsStats -> Rational
minor_gcs s =
  gcs s - major_gcs s

mut_cpu_percent :: RtsStats -> Rational
mut_cpu_percent s =
  mutator_cpu_ns s `divide` cpu_ns s

mut_wall_percent :: RtsStats -> Rational
mut_wall_percent s =
  mutator_elapsed_ns s `divide` elapsed_ns s

mutator_cpu_ns :: RtsStats -> Rational
mutator_cpu_ns (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ n _ _) =
  n

mutator_elapsed_ns :: RtsStats -> Rational
mutator_elapsed_ns (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ n _) =
  n

par_copied_bytes :: RtsStats -> Rational
par_copied_bytes (RtsStats _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ n) =
  n

work_balance :: RtsStats -> Maybe Rational
work_balance s =
  cumulative_par_balanced_copied_bytes s `divide'` par_copied_bytes s
