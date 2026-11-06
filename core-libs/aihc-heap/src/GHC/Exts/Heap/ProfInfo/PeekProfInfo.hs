module GHC.Exts.Heap.ProfInfo.PeekProfInfo (module Reexport) where

-- AIHC has no profiled GHC closures, so the module re-exports the variant for
-- builds without profiling, as ghc-heap does for them.
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled as Reexport
