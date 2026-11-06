module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled
  ( peekStgTSOProfInfo,
    peekTopCCS,
  )
where

import Foreign (Ptr)
import GHC.Exts.Heap.ProfInfo.Types
import Prelude

-- | A program without profiling has no profile record, so the result is
-- always 'Nothing'.
peekStgTSOProfInfo :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo _ _ = pure Nothing

-- | A program without profiling has no cost-centre stack, so the result is
-- always 'Nothing'.
peekTopCCS :: Ptr a -> IO (Maybe CostCentreStack)
peekTopCCS _ = pure Nothing
