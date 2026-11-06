-- | The kinds of heap object in the GHC runtime system.
module GHC.Exts.Heap.ClosureTypes
  ( ClosureType (..),
    closureTypeHeaderSize,
  )
where

import GHC.Internal.ClosureTypes (ClosureType (..))
import Prelude

-- | The number of header words of an object of this kind. AIHC objects are
-- not profiled, so a thunk-like object has two header words and every other
-- object has one.
closureTypeHeaderSize :: ClosureType -> Int
closureTypeHeaderSize kind
  | kind >= THUNK && kind <= THUNK_0_2 = 2
  | kind `elem` [THUNK_SELECTOR, AP, AP_STACK] = 2
  | otherwise = 1
