module GHC.Exts.Stack
  ( decodeStack,
    stackFrameSize,
  )
where

import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.Unsupported
import GHC.Exts.Stack.Decode
import Prelude

-- | The size of a stack frame in words. AIHC stack frames do not have the GHC
-- layout.
stackFrameSize :: StackFrame -> Int
stackFrameSize _ = unsupported "stackFrameSize"
