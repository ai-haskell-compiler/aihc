{-# LANGUAGE CPP #-}
{-|
Utility functions to inspect RTS flags
-}
module Debug.Trace.Flags
  ( userTracingEnabled
  ) where
import Foreign.C.Types
import Foreign.Marshal.Utils
#if !MIN_VERSION_base(4, 10, 0)
import Data.Word
#endif

-- | Check if user tracing is enabled in event logging. Currently GHC RTS
-- doesn't modify the flag after the eventlog framework is initialized so making
-- this a constant value makes sense.
userTracingEnabled :: Bool
userTracingEnabled = toBool c_userTracingEnabled
{-# NOINLINE userTracingEnabled #-}

#if MIN_VERSION_base(4, 10, 0)
type CBOOL = CBool
#else
type CBOOL = Word8
#endif

foreign import ccall "userTracingEnabled" c_userTracingEnabled :: CBOOL
