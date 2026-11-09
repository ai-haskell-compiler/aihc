-- | Memory-related system operations.
--
-- The aihc runtime does not let a program start a garbage collection yet.
-- Thus, the GC operations do nothing.
-- The allocation counter and the allocation limit are also not available.
module System.Mem
  ( performGC,
    performMajorGC,
    performBlockingMajorGC,
    performMinorGC,
    setAllocationCounter,
    getAllocationCounter,
    enableAllocationLimit,
    disableAllocationLimit,
  )
where

import Data.Int (Int64)
import Prelude

-- | Request a major garbage collection.
-- On aihc, this operation does nothing.
performGC :: IO ()
performGC = return ()

-- | Request a major garbage collection.
-- On aihc, this operation does nothing.
performMajorGC :: IO ()
performMajorGC = return ()

-- | Request a major garbage collection and wait for it to complete.
-- On aihc, this operation does nothing.
performBlockingMajorGC :: IO ()
performBlockingMajorGC = return ()

-- | Request a minor garbage collection.
-- On aihc, this operation does nothing.
performMinorGC :: IO ()
performMinorGC = return ()

-- | Set the allocation counter of the current thread.
-- On aihc, this operation does nothing.
setAllocationCounter :: Int64 -> IO ()
setAllocationCounter _ = return ()

-- | Return the allocation counter of the current thread.
-- On aihc, the value is always 0.
getAllocationCounter :: IO Int64
getAllocationCounter = return 0

-- | Enable the allocation limit of the current thread.
-- On aihc, this operation does nothing.
enableAllocationLimit :: IO ()
enableAllocationLimit = return ()

-- | Disable the allocation limit of the current thread.
-- On aihc, this operation does nothing.
disableAllocationLimit :: IO ()
disableAllocationLimit = return ()
