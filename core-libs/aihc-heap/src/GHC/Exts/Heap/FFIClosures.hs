module GHC.Exts.Heap.FFIClosures (module Reexport) where

-- AIHC has no profiled GHC closures, so the module re-exports the variant for
-- builds without profiling, as ghc-heap does for them.
import GHC.Exts.Heap.FFIClosures_ProfilingDisabled as Reexport
