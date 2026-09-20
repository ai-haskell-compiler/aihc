module System.CPUTime.Clock (clock) where

import Foreign.C.Types (CClock)
import GHC.Internal.IO.Types (ioError, unsupportedOperation)
import System.IO.Error (ioeSetLocation)
import Prelude (IO)

-- | WASI has no process CPU clock.
clock :: IO CClock
clock = ioError (ioeSetLocation unsupportedOperation "getCPUTime")
