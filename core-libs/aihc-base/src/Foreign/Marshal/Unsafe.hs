-- | The unsafe entry point of the marshalling functions.
module Foreign.Marshal.Unsafe
  ( unsafeLocalState,
  )
where

import GHC.IO (IO, unsafeDupablePerformIO)

-- | Run an action that only allocates and reads local memory, and return its
-- result as a pure value.
unsafeLocalState :: IO a -> a
unsafeLocalState = unsafeDupablePerformIO
