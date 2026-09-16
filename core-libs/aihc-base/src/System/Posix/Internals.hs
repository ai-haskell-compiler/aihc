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

    -- * The calls and their constants
    module System.Posix.Internals.Syscalls,
  )
where

import Control.Monad (when)
import Data.Bool (Bool (..))
import Data.Foldable (elem)
import Data.Maybe (Maybe (..))
import Foreign.C.String (CString, CStringLen, newCString, peekCString, peekCStringLen, withCString)
import GHC.Base ((>>))
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..), ioError)
import System.Posix.Internals.Syscalls
import System.Posix.Internals.Types
import Prelude (FilePath, IO)

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
