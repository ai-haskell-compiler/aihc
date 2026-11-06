-- | AIHC does not profile, so this module re-exports the variant for
-- programs without profiling.
module GHC.Exts.Heap.FFIClosures (module Reexport) where

import GHC.Exts.Heap.FFIClosures_ProfilingDisabled as Reexport
