{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | The POSIX calls and constants "System.Posix.Internals" offers, as WASI
-- provides them.
--
-- WASI is a capability-based interface rather than a POSIX kernel, and its C
-- library leaves out what it cannot express: it has no signals at all, so
-- including its @signal.h@ is itself an error without an emulation flag, and
-- it has no file mode of its own, so @umask@ and @mkfifo@ are absent. Those
-- five calls raise an unsupported-operation error here, which is how GHC
-- handles the same gaps on the platforms that have them. Everything else is
-- the call WASI does provide, reached through its header exactly as on a
-- POSIX platform.
module System.Posix.Internals.Syscalls
  ( -- * File system calls
    c_access,
    c_chmod,
    c_fstat,
    c_ftruncate,
    c_link,
    c_lseek,
    c_mkfifo,
    c_stat,
    c_umask,
    c_unlink,
    c_utime,

    -- * Descriptor control
    c_fcntl_read,
    c_fcntl_write,
    c_fcntl_lock,

    -- * Signal masks
    c_sigaddset,
    c_sigemptyset,
    c_sigprocmask,
    sizeof_sigset_t,

    -- * The @open(2)@ flags
    o_RDONLY,
    o_WRONLY,
    o_RDWR,
    o_APPEND,
    o_CREAT,
    o_EXCL,
    o_TRUNC,
    o_NOCTTY,
    o_NONBLOCK,
  )
where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CLong (..))
import Foreign.Ptr (Ptr)
import GHC.Int (Int)
import GHC.Internal.IO.Types (ioError, unsupportedOperation)
import System.IO.Error (ioeSetLocation)
import System.Posix.Internals.Repr (sizeofSigsetT)
import System.Posix.Internals.Types (CFLock, CSigset, CStat, CUtimbuf)
import System.Posix.Types (CMode (..), COff (..))
import Prelude (IO, String)

-- | The error a call WASI does not have raises.
unsupported :: String -> IO a
unsupported name = ioError (ioeSetLocation unsupportedOperation name)

foreign import capi unsafe "unistd.h access"
  c_access :: CString -> CInt -> IO CInt

foreign import capi unsafe "sys/stat.h chmod"
  c_chmod :: CString -> CMode -> IO CInt

foreign import capi unsafe "sys/stat.h fstat"
  c_fstat :: CInt -> Ptr CStat -> IO CInt

foreign import capi unsafe "unistd.h ftruncate"
  c_ftruncate :: CInt -> COff -> IO CInt

foreign import capi unsafe "unistd.h link"
  c_link :: CString -> CString -> IO CInt

foreign import capi unsafe "unistd.h lseek"
  c_lseek :: CInt -> COff -> CInt -> IO COff

-- | WASI has no FIFOs.
c_mkfifo :: CString -> CMode -> IO CInt
c_mkfifo _ _ = unsupported "mkfifo"

foreign import capi unsafe "sys/stat.h stat"
  c_stat :: CString -> Ptr CStat -> IO CInt

-- | WASI has no process-wide file creation mask.
c_umask :: CMode -> IO CMode
c_umask _ = unsupported "umask"

foreign import capi unsafe "unistd.h unlink"
  c_unlink :: CString -> IO CInt

foreign import capi unsafe "utime.h utime"
  c_utime :: CString -> Ptr CUtimbuf -> IO CInt

-- | @fcntl@ with no third argument, as @F_GETFL@ and @F_GETFD@ take none.
foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl_read :: CInt -> CInt -> IO CInt

-- | @fcntl@ with an integer third argument, as @F_SETFL@ and @F_SETFD@ take.
foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt

-- | @fcntl@ with a lock third argument, as the @F_@/@GETLK@ family takes.
foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl_lock :: CInt -> CInt -> Ptr CFLock -> IO CInt

-- | WASI has no signals.
c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
c_sigaddset _ _ = unsupported "sigaddset"

-- | WASI has no signals.
c_sigemptyset :: Ptr CSigset -> IO CInt
c_sigemptyset _ = unsupported "sigemptyset"

-- | WASI has no signals.
c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
c_sigprocmask _ _ _ = unsupported "sigprocmask"

-- | The size of a @sigset_t@, for the caller that allocates one.
sizeof_sigset_t :: Int
sizeof_sigset_t = sizeofSigsetT

foreign import capi unsafe "fcntl.h value O_RDONLY" o_RDONLY :: CInt

foreign import capi unsafe "fcntl.h value O_WRONLY" o_WRONLY :: CInt

foreign import capi unsafe "fcntl.h value O_RDWR" o_RDWR :: CInt

foreign import capi unsafe "fcntl.h value O_APPEND" o_APPEND :: CInt

foreign import capi unsafe "fcntl.h value O_CREAT" o_CREAT :: CInt

foreign import capi unsafe "fcntl.h value O_EXCL" o_EXCL :: CInt

foreign import capi unsafe "fcntl.h value O_TRUNC" o_TRUNC :: CInt

foreign import capi unsafe "fcntl.h value O_NOCTTY" o_NOCTTY :: CInt

foreign import capi unsafe "fcntl.h value O_NONBLOCK" o_NONBLOCK :: CInt
