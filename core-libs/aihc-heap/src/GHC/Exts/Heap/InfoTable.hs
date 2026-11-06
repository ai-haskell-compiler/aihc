module GHC.Exts.Heap.InfoTable
  ( module GHC.Exts.Heap.InfoTable.Types,
    itblSize,
    peekItbl,
    pokeItbl,
  )
where

import Foreign
import GHC.Exts.Heap.InfoTable.Types
import GHC.Exts.Heap.Unsupported
import Prelude

-- | AIHC info tables do not have the layout of GHC info tables.
peekItbl :: Ptr StgInfoTable -> IO StgInfoTable
peekItbl _ = unsupported "peekItbl"

-- | AIHC info tables do not have the layout of GHC info tables.
pokeItbl :: Ptr StgInfoTable -> StgInfoTable -> IO ()
pokeItbl _ _ = unsupported "pokeItbl"

-- | Size in bytes of a standard info table.
itblSize :: Int
itblSize = unsupported "itblSize"
