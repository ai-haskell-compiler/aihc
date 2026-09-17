-- | The C types a POSIX call takes a pointer to.
--
-- Each is an empty data type: nothing here reads one of these structures, it
-- only points at one, and its layout is the platform's business. A caller
-- that does read one, @unix@ for instance, knows the offsets from the
-- platform's headers and needs no more from this library than a type to name
-- the pointer with.
--
-- They live apart from "System.Posix.Internals" so that the platform module
-- of calls can name them without importing the module that re-exports it.
module System.Posix.Internals.Types
  ( CFLock,
    CSigset,
    CStat,
    CTermios,
    CUtimbuf,
    CUtsname,
    FD,
    CFilePath,
  )
where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt)

-- | The @struct flock@ an advisory lock is described by.
data {-# CTYPE "struct flock" #-} CFLock

-- | The @sigset_t@ a set of signals is held in.
data {-# CTYPE "sigset_t" #-} CSigset

-- | The @struct stat@ a file's metadata is read into.
data {-# CTYPE "struct stat" #-} CStat

-- | The @struct termios@ a terminal's settings are held in.
data {-# CTYPE "struct termios" #-} CTermios

-- | The @struct utimbuf@ the times of a file are set from.
data {-# CTYPE "struct utimbuf" #-} CUtimbuf

-- | The @struct utsname@ the system's name is read into.
data {-# CTYPE "struct utsname" #-} CUtsname

-- | A file descriptor: the C @int@ that names an open file.
type FD = CInt

-- | The string a POSIX call takes a file path as. POSIX paths are bytes, so
-- this is a byte string rather than the wide string Windows would want.
type CFilePath = CString
