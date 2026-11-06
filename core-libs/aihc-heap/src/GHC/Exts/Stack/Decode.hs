module GHC.Exts.Stack.Decode
  ( decodeStack,
  )
where

import GHC.Exts.Heap.Closures (StgStackClosure)
import GHC.Exts.Heap.Unsupported
import GHC.Stack.CloneStack (StackSnapshot)
import Prelude

-- | Decode a 'StackSnapshot' to a 'StgStackClosure'. AIHC stacks do not have
-- the layout of GHC stacks, so the function raises an exception.
decodeStack :: StackSnapshot -> IO StgStackClosure
decodeStack _ = unsupported "decodeStack"
