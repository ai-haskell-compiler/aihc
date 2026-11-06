-- | AIHC does not profile, so this module re-exports the variant for
-- programs without profiling.
module GHC.Exts.Heap.ProfInfo.PeekProfInfo (module Reexport) where

import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled as Reexport
