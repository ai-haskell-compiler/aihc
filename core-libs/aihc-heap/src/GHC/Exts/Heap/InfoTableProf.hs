-- | AIHC does not profile, so this module has the same functions as
-- "GHC.Exts.Heap.InfoTable".
module GHC.Exts.Heap.InfoTableProf
  ( module GHC.Exts.Heap.InfoTable.Types,
    itblSize,
    peekItbl,
    pokeItbl,
  )
where

import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.InfoTable.Types
