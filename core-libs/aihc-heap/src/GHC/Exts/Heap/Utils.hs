module GHC.Exts.Heap.Utils
  ( dataConNames,
  )
where

import Foreign
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.Unsupported
import Prelude

-- | Read the package, module, and name of a data constructor from its info
-- table. AIHC info tables do not have the layout of GHC info tables.
dataConNames :: Ptr StgInfoTable -> IO (String, String, String)
dataConNames _ = unsupported "dataConNames"
