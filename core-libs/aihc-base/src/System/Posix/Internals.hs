{-# LANGUAGE CApiFFI #-}

{- HLINT ignore "Use camelCase" -}

-- | The POSIX layer the boot libraries are built on: the C types a POSIX
-- call takes a pointer to, the marshalling of a 'FilePath' into the string a
-- POSIX call wants, and the calls themselves.
--
-- /The API of this module is unstable and not meant to be consumed by the/
-- /general public./ It exists because @unix@ and the IO library are written
-- against it.
--
-- The calls and the @open(2)@ flags come from "System.Posix.Internals.Syscalls",
-- which has one copy per platform because WASI has neither signal masks nor a
-- file mode of its own. They are re-exported here so that a user of this
-- module sees the single interface GHC's own offers.
module System.Posix.Internals
  ( -- * The C types a POSIX call points at
    module System.Posix.Internals.Types,

    -- * File paths across the FFI
    withFilePath,
    newFilePath,
    peekFilePath,
    peekFilePathLen,

    -- * The runtime
    hostIsThreaded,

    -- * Descriptors
    fdGetMode,
    setNonBlockingFD,
    setCloseOnExec,

    -- * File metadata
    sizeof_stat,
    lstat,
    st_mode,
    st_size,
    st_mtime,
    st_dev,
    st_ino,
    s_isreg,
    s_isdir,
    s_ischr,
    s_isblk,
    s_isfifo,
    s_issock,

    -- * The calls and their constants
    module System.Posix.Internals.Syscalls,
  )
where

import Control.Monad (when)
import Data.Bits (complement, (.&.), (.|.))
import Data.Bool (Bool (..))
import Data.Foldable (elem)
import Data.Maybe (Maybe (..))
import Foreign.C.Error (Errno (..), eINVAL, errnoToIOError, throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_, throwErrnoIfMinus1_)
import Foreign.C.String (CString, CStringLen, newCString, peekCString, peekCStringLen, withCString)
import Foreign.C.Types (CInt (..), CTime (..))
import Foreign.Ptr (Ptr)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.Runtime (decodeError, descriptorMode)
import GHC.Int (Int64 (..))
import GHC.Internal.Base (Monad (..), (>>))
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..), ioError)
import System.Posix.Internals.Syscalls
import System.Posix.Internals.Types
import System.Posix.Types (CDev (..), CIno (..), CMode (..))
import Prelude (FilePath, IO, Int, fromIntegral, (/=), (<), (==))

-- | Run an action on the encoded form of a file path.
--
-- The encoding is the file system encoding, which this library fixes as
-- UTF-8 ('GHC.IO.Encoding.getFileSystemEncoding' returns @utf8@ and nothing
-- can change it), so the path is encoded with the UTF-8 marshalling of
-- "Foreign.C.String" rather than by looking the encoding up first.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath path action = checkForInteriorNuls path >> withCString path action

-- | Copy the encoded form of a file path into fresh storage, which the
-- caller frees.
newFilePath :: FilePath -> IO CString
newFilePath path = checkForInteriorNuls path >> newCString path

-- | Decode the NUL-terminated file path a POSIX call wrote.
peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString

-- | Decode the file path of the given byte length.
peekFilePathLen :: CStringLen -> IO FilePath
peekFilePathLen = peekCStringLen

-- | Reject a file path holding a NUL.
--
-- A POSIX path is NUL-terminated, so a NUL inside one would silently cut the
-- path short and name a different file. GHC looks for a NUL octet in the
-- encoded bytes; this library encodes a NUL as the two bytes of modified
-- UTF-8, which no byte-level search would find, so the character is looked
-- for in the path itself. See GHC's #13660.
checkForInteriorNuls :: FilePath -> IO ()
checkForInteriorNuls path = when ('\0' `elem` path) (throwInternalNulError path)

throwInternalNulError :: FilePath -> IO a
throwInternalNulError path =
  ioError
    IOError
      { ioe_handle = Nothing,
        ioe_type = InvalidArgument,
        ioe_location = "checkForInteriorNuls",
        ioe_description = "FilePaths must not contain internal NUL code units.",
        ioe_errno = Nothing,
        ioe_filename = Just path
      }

-- | Whether the runtime runs Haskell threads on more than one OS thread.
--
-- @unix@ reads this to decide whether a blocking call has to go through a
-- safe foreign import to keep the other threads running. The aihc runtime
-- has one OS thread and schedules its Haskell threads on it, so it is the
-- non-threaded runtime and the answer is 'False'. GHC asks its RTS, which
-- comes in both forms.
hostIsThreaded :: Bool
hostIsThreaded = False

-- | The mode a descriptor was opened with.
--
-- A descriptor the program already has says how it may be used, and that is
-- what decides whether a 'GHC.IO.Handle.Types.Handle' over it reads, writes
-- or does both. GHC asks @fcntl@ for the @open(2)@ flags; here the runtime
-- answers, because it is the runtime that will own the descriptor, and the
-- host with no descriptors to report a mode for says so rather than
-- guessing.
fdGetMode :: FD -> IO IOMode
fdGetMode descriptor = do
  mode <- descriptorMode (fromIntegral descriptor)
  case mode < 0 of
    True -> failWith (Errno (fromIntegral (decodeError mode)))
    False -> case ioModeOfNumber mode of
      Just ioMode -> return ioMode
      Nothing -> failWith eINVAL
  where
    failWith errno = ioError (errnoToIOError "fdGetMode" errno Nothing Nothing)

-- | The 'IOMode' an open request numbers this way.
ioModeOfNumber :: Int -> Maybe IOMode
ioModeOfNumber number =
  case number == 0 of
    True -> Just ReadMode
    False -> case number == 1 of
      True -> Just WriteMode
      False -> case number == 2 of
        True -> Just AppendMode
        False -> case number == 3 of
          True -> Just ReadWriteMode
          False -> Nothing

-- | Set or clear the @O_NONBLOCK@ flag of a descriptor.
--
-- The call changes the flags only when they change. A socket library sets
-- the flag and then waits with 'GHC.Conc.threadWaitRead' when a call
-- returns @EAGAIN@.
setNonBlockingFD :: FD -> Bool -> IO ()
setNonBlockingFD descriptor enabled = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD" (c_fcntl_read descriptor const_f_getfl)
  let updated = case enabled of
        True -> flags .|. o_NONBLOCK
        False -> flags .&. complement o_NONBLOCK
  when
    (updated /= flags)
    (throwErrnoIfMinus1Retry_ "setNonBlockingFD" (c_fcntl_write descriptor const_f_setfl (fromIntegral updated)))

-- | Set the @FD_CLOEXEC@ flag of a descriptor.
setCloseOnExec :: FD -> IO ()
setCloseOnExec descriptor =
  throwErrnoIfMinus1_ "setCloseOnExec" (c_fcntl_write descriptor const_f_setfd (fromIntegral const_fd_cloexec))

-- | The size of a @struct stat@, for the caller that allocates one.
sizeof_stat :: Int
sizeof_stat = fromIntegral c_sizeof_stat

foreign import capi unsafe "aihc_stat.h value AIHC_SIZEOF_STAT"
  c_sizeof_stat :: CInt

-- | Read the metadata of a file into a @struct stat@. If the path names a
-- symbolic link, the call reads the metadata of the link and not of its
-- target.
foreign import capi unsafe "sys/stat.h lstat"
  lstat :: CFilePath -> Ptr CStat -> IO CInt

-- | The file type and permission bits of a @struct stat@.
foreign import capi unsafe "aihc_stat.h aihc_st_mode"
  st_mode :: Ptr CStat -> IO CMode

-- | The size in bytes of a @struct stat@ file.
foreign import capi unsafe "aihc_stat.h aihc_st_size"
  st_size :: Ptr CStat -> IO Int64

-- | The time of the last change to the contents of a @struct stat@ file.
foreign import capi unsafe "aihc_stat.h aihc_st_mtime"
  st_mtime :: Ptr CStat -> IO CTime

-- | The device that holds a @struct stat@ file.
foreign import capi unsafe "aihc_stat.h aihc_st_dev"
  st_dev :: Ptr CStat -> IO CDev

-- | The inode number of a @struct stat@ file.
foreign import capi unsafe "aihc_stat.h aihc_st_ino"
  st_ino :: Ptr CStat -> IO CIno

-- | Whether a file mode is that of a regular file.
s_isreg :: CMode -> Bool
s_isreg mode = c_s_isreg mode /= 0

-- | Whether a file mode is that of a directory.
s_isdir :: CMode -> Bool
s_isdir mode = c_s_isdir mode /= 0

-- | Whether a file mode is that of a character device.
s_ischr :: CMode -> Bool
s_ischr mode = c_s_ischr mode /= 0

-- | Whether a file mode is that of a block device.
s_isblk :: CMode -> Bool
s_isblk mode = c_s_isblk mode /= 0

-- | Whether a file mode is that of a FIFO.
s_isfifo :: CMode -> Bool
s_isfifo mode = c_s_isfifo mode /= 0

-- | Whether a file mode is that of a socket.
s_issock :: CMode -> Bool
s_issock mode = c_s_issock mode /= 0

foreign import capi unsafe "aihc_stat.h aihc_s_isreg" c_s_isreg :: CMode -> CInt

foreign import capi unsafe "aihc_stat.h aihc_s_isdir" c_s_isdir :: CMode -> CInt

foreign import capi unsafe "aihc_stat.h aihc_s_ischr" c_s_ischr :: CMode -> CInt

foreign import capi unsafe "aihc_stat.h aihc_s_isblk" c_s_isblk :: CMode -> CInt

foreign import capi unsafe "aihc_stat.h aihc_s_isfifo" c_s_isfifo :: CMode -> CInt

foreign import capi unsafe "aihc_stat.h aihc_s_issock" c_s_issock :: CMode -> CInt
