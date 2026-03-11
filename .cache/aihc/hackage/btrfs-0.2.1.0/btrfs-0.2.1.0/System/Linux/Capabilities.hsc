module System.Linux.Capabilities
    ( hasSysAdminCap
    ) where

import Foreign
import Foreign.C
import System.Posix (getProcessID)

#include <sys/capability.h>

-- It's probably a bad idea to use the capget syscall instead of the
-- functions from libcap, but this way we avoid the dependecy on libcap.

hasSysAdminCap :: IO Bool
hasSysAdminCap =
    flip testBit (#const CAP_SYS_ADMIN) <$> getCapabilities

getCapabilities :: IO Word64
getCapabilities =
    allocaBytes (#size struct __user_cap_header_struct) $ \hdrp ->
    allocaBytes (2 * (#size struct __user_cap_data_struct)) $ \datap -> do
        pid <- getProcessID
        (#poke struct __user_cap_header_struct, version) hdrp ((#const _LINUX_CAPABILITY_VERSION_2) :: Word32)
        (#poke struct __user_cap_header_struct, pid) hdrp (fromIntegral pid :: Word32)
        throwErrnoIfMinus1_ "capget" $ c_capget hdrp datap
        effective0 <- (#peek struct __user_cap_data_struct, effective) datap :: IO Word32
        let datap1 = datap `plusPtr` (#size struct __user_cap_data_struct)
        effective1 <- (#peek struct __user_cap_data_struct, effective) datap1 :: IO Word32
        return (fromIntegral effective1 `unsafeShiftL` 32 .|. fromIntegral effective0)

data CapUserHeader
data CapUserData

foreign import ccall unsafe "sys/capability.h capget"
    c_capget :: Ptr CapUserHeader -> Ptr CapUserData -> IO CInt
