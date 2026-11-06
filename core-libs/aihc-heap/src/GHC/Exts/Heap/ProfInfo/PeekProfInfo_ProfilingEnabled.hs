module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled
  ( peekStgTSOProfInfo,
    peekTopCCS,
  )
where

import Foreign (Ptr)
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Unsupported
import Prelude

-- | AIHC has no profiled closures to read.
peekStgTSOProfInfo :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo _ _ = unsupported "peekStgTSOProfInfo (profiling enabled)"

-- | AIHC has no profiled closures to read.
peekTopCCS :: Ptr a -> IO (Maybe CostCentreStack)
peekTopCCS _ = unsupported "peekTopCCS (profiling enabled)"
