-- | Handles over file descriptors, and the standard handles and file opens
-- that GHC also exports from here.
module GHC.IO.Handle.FD
  ( fdToHandle,
    fdToHandle',
    mkHandleFromFD,
    stdin,
    stdout,
    stderr,
    openFile,
    openBinaryFile,
    openFileBlocking,
    withFile,
    withBinaryFile,
    withFileBlocking,
  )
where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isJust)
import GHC.Base (Monad (..), String, (++))
import GHC.IO (FilePath, IO)
import GHC.IO.Encoding (utf8)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.IO.FD (mkFD)
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.Internals (mkFileHandle)
import GHC.IO.Handle.Types (Handle (..), nativeNewlineMode, noNewlineTranslation)
import GHC.IO.IOMode (IOMode)
import GHC.IO.StdHandles (openBinaryFile, openFile, openFileBlocking, stderr, stdin, stdout, withBinaryFile, withFile, withFileBlocking)
import GHC.Internal.IO.Types (IODeviceType (..), IOErrorType (..), IOException (..), ioException)
import GHC.Show (show)
import System.Posix.Internals (FD, fdGetMode)

-- | A 'Handle' over a file descriptor the program already has.
--
-- The descriptor's own mode decides whether the handle reads, writes or does
-- both; nothing is opened. The handle owns the descriptor from here on, so
-- closing the handle closes the descriptor.
fdToHandle :: FD -> IO Handle
fdToHandle descriptor = do
  mode <- fdGetMode descriptor
  (fd, deviceType) <- mkFD descriptor mode Nothing False False
  mkHandleFromFD fd deviceType (descriptorName descriptor) mode False (Just utf8)

-- | 'fdToHandle' for a caller that already knows the descriptor's mode.
--
-- The device type, the path and the binary flag are the caller's too: a
-- binary handle translates neither the encoding nor the newlines. The
-- device type the runtime reports is the one the handle is built with,
-- because the runtime treats every resource as a stream.
fdToHandle' :: FD -> Maybe IODeviceType -> Bool -> FilePath -> IOMode -> Bool -> IO Handle
fdToHandle' descriptor _deviceType isSocket path mode binary = do
  (fd, deviceType) <- mkFD descriptor mode Nothing isSocket False
  mkHandleFromFD fd deviceType path mode False codec
  where
    codec = case binary of
      True -> Nothing
      False -> Just utf8

-- | A 'Handle' over an 'GHC.IO.FD.FD' the caller has already made.
--
-- GHC gives a stream opened for both reading and writing a duplex handle,
-- because a socket's two directions cannot share one buffer. It knows a
-- socket from a file; this runtime reports every resource as a stream, so
-- the same rule would make a duplex handle out of a read-write file too.
-- A read-write descriptor therefore gets the single handle that 'openFile'
-- gives the same mode.
mkHandleFromFD :: FD.FD -> IODeviceType -> FilePath -> IOMode -> Bool -> Maybe TextEncoding -> IO Handle
mkHandleFromFD fd deviceType path mode _setNonBlocking codec =
  case deviceType of
    Directory -> ioException (IOError Nothing InappropriateType "mkHandleFromFD" "is a directory" Nothing (Just path))
    _ -> mkFileHandle fd path mode codec newlines
  where
    newlines = case isJust codec of
      True -> nativeNewlineMode
      False -> noNewlineTranslation

-- | What a handle over a bare descriptor calls itself, as GHC names one.
descriptorName :: FD -> String
descriptorName descriptor = "<file descriptor: " ++ show descriptor ++ ">"
