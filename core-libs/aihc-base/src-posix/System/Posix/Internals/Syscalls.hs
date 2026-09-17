{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | The POSIX calls and constants "System.Posix.Internals" offers, as a
-- POSIX platform provides them.
--
-- Almost every entity here is reached with @capi@ rather than @ccall@: a C
-- library is free to make one of these a macro that redirects to a
-- differently named symbol, which @stat@ and @lseek@ are on a glibc system
-- with large file support and @fcntl@ is on several, and it is free to make
-- one a variadic function, which @fcntl@ is everywhere. A @capi@ import is
-- compiled against the platform's own header, so whatever the header says
-- the entity is, the call is the one the header describes.
--
-- The @open(2)@ flags are @capi@ values for the same reason: they are macros
-- rather than symbols, and their numbers differ between platforms.
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
import System.Posix.Internals.Repr (sizeofSigsetT)
import System.Posix.Internals.Types (CFLock, CSigset, CStat, CUtimbuf)
import System.Posix.Types (CMode (..), COff (..))
import Prelude (IO)

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

foreign import capi unsafe "sys/stat.h mkfifo"
  c_mkfifo :: CString -> CMode -> IO CInt

foreign import capi unsafe "sys/stat.h stat"
  c_stat :: CString -> Ptr CStat -> IO CInt

foreign import capi unsafe "sys/stat.h umask"
  c_umask :: CMode -> IO CMode

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

-- @sigaddset@ and @sigemptyset@ are the two entities here that @capi@ cannot
-- reach: Apple's @signal.h@ defines each as a macro that dereferences its
-- argument, and a generated wrapper passes the set as a @void *@, which has
-- nothing to dereference. POSIX requires both to exist as functions as well,
-- so these name those functions directly.
foreign import ccall unsafe "sigaddset"
  c_sigaddset :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "sigemptyset"
  c_sigemptyset :: Ptr CSigset -> IO CInt

foreign import capi unsafe "signal.h sigprocmask"
  c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt

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
