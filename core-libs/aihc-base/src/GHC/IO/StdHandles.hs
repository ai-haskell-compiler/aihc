module GHC.IO.StdHandles
  ( stdin,
    stdout,
    stderr,
    openBinaryFile,
    IOHandle,
    stdinHandle,
    stdoutHandle,
    stderrHandle,
    copyAddrToByteArray,
    readIntoBuffer,
    writeFromBuffer,
  )
where

import GHC.Foreign (openUtf8FilePath)
import GHC.IO.Exception (ioError, ioErrorFromErrno)
import GHC.IO.FD
  ( IOHandle,
    copyAddrToByteArray,
    readIntoBuffer,
    stderrHandle,
    stdinHandle,
    stdoutHandle,
    writeFromBuffer,
  )
import GHC.IO.Handle.Types (Handle, newHandle)
import GHC.IO.IOMode (IOMode (..), ioModeNumber)
import GHC.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

stdin :: Handle
stdin = makeStandardHandle "<stdin>" ReadMode stdinHandle

stdout :: Handle
stdout = makeStandardHandle "<stdout>" WriteMode stdoutHandle

stderr :: Handle
stderr = makeStandardHandle "<stderr>" WriteMode stderrHandle

-- The three callers are CAFs, so each forced result owns one persistent MVar.
makeStandardHandle :: String -> IOMode -> IO (Ptr IOHandle) -> Handle
makeStandardHandle name mode rawHandleAction =
  unsafePerformIO
    ( do
        rawHandle <- rawHandleAction
        newHandle name rawHandle mode
    )

openBinaryFile :: String -> IOMode -> IO Handle
openBinaryFile path mode = do
  openResult <- openUtf8FilePath path (ioModeNumber mode)
  case openResult of
    Left openError -> ioError (ioErrorFromErrno "openBinaryFile" (Just path) openError)
    Right rawHandle -> newHandle path rawHandle mode
