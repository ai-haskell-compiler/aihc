module GHC.Exts.Heap.InfoTable
  ( module GHC.Exts.Heap.InfoTable.Types,
    itblSize,
    peekItbl,
    pokeItbl,
  )
where

import Foreign (Ptr)
import GHC.Exts.Heap.InfoTable.Types
import GHC.Exts.Heap.Unsupported
import Prelude

-- | Read an info table. AIHC info tables do not have the GHC layout.
peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl _ = unsupported "peekItbl"

-- | Write an info table. AIHC info tables do not have the GHC layout.
pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl _ _ = unsupported "pokeItbl"

-- | The size in bytes of an info table. AIHC info tables do not have the GHC
-- layout.
itblSize :: Int
itblSize = unsupported "itblSize"
