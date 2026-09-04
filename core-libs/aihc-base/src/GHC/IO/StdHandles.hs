-- | The standard handles and the file open operations.
module GHC.IO.StdHandles
  ( stdin,
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
import Data.Maybe (Maybe (..))
import GHC.Base (Monad (..))
import GHC.IO (FilePath, IO, bracket)
import GHC.IO.Encoding (utf8)
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.Internals (hClose_help, mkFileHandle, mkHandle, withHandle', withHandle__')
import GHC.IO.Handle.Types (BufferMode (..), Handle (..), HandleType (..), Handle__ (..), nativeNewlineMode, noNewlineTranslation)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Internal.IO.Types (ioe_unsupportedOperation)
import GHC.MVar (MVar)

-- The three standard handles are CAFs, so each one owns one MVar. The
-- runtime cannot tell a terminal from a file. GHC gives a terminal line
-- buffering, so stdout is line buffered and stderr is unbuffered.
stdin :: Handle
stdin = unsafePerformIO (mkHandle FD.stdin "<stdin>" ReadHandle True (Just utf8) nativeNewlineMode Nothing Nothing)
{-# NOINLINE stdin #-}

stdout :: Handle
stdout =
  unsafePerformIO
    ( do
        handle <- mkHandle FD.stdout "<stdout>" WriteHandle True (Just utf8) nativeNewlineMode Nothing Nothing
        withHandle__' "stdout" handle (handleState handle) (\handle_ -> return handle_ {haBufferMode = LineBuffering})
        return handle
    )
{-# NOINLINE stdout #-}

handleState :: Handle -> MVar Handle__
handleState (FileHandle _ state) = state
handleState (DuplexHandle _ _ state) = state

stderr :: Handle
stderr = unsafePerformIO (mkHandle FD.stderr "<stderr>" WriteHandle False (Just utf8) nativeNewlineMode Nothing Nothing)
{-# NOINLINE stderr #-}

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = do
  (fd, _) <- FD.openFile path mode True
  mkFileHandle fd path mode (Just utf8) nativeNewlineMode

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile path mode = do
  (fd, _) <- FD.openFile path mode True
  mkFileHandle fd path mode Nothing noNewlineTranslation

openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking path mode = do
  (fd, _) <- FD.openFile path mode False
  mkFileHandle fd path mode (Just utf8) nativeNewlineMode

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode = bracket (openFile path mode) closeHandle

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile path mode = bracket (openBinaryFile path mode) closeHandle

withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileBlocking path mode = bracket (openFileBlocking path mode) closeHandle

-- | Close a file handle. "GHC.IO.Handle" imports this module, so the
-- close lives here.
closeHandle :: Handle -> IO ()
closeHandle handle@(FileHandle _ state) = do
  _ <- withHandle' "hClose" handle state hClose_help
  return ()
closeHandle DuplexHandle {} = ioe_unsupportedOperation
