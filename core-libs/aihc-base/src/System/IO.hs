module System.IO
  ( Handle,
    IOMode (..),
    hClose,
    hGetBuf,
    hPutBuf,
    hPutStr,
    openBinaryFile,
    stdin,
    stdout,
    stderr,
  )
where

import GHC.IO.Handle (Handle, hClose)
import GHC.IO.Handle.Text (hGetBuf, hPutBuf, hPutStr)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, stderr, stdin, stdout)
