module GHC.Exts.Heap.Utils
  ( dataConNames,
  )
where

import Foreign (Ptr)
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.Unsupported
import Prelude

-- | The package, the module, and the name of the data constructor of an info
-- table. AIHC info tables do not have the GHC layout.
dataConNames :: Ptr StgInfoTable -> IO (String, String, String)
dataConNames _ = unsupported "dataConNames"
