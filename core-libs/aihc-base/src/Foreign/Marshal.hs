module Foreign.Marshal
  ( module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Array,
    module Foreign.Marshal.Error,
    module Foreign.Marshal.Pool,
    module Foreign.Marshal.Utils,
    unsafeLocalState,
  )
where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils
import GHC.IO.Unsafe (unsafePerformIO)
import Prelude (IO)

unsafeLocalState :: IO a -> a
unsafeLocalState = unsafePerformIO
