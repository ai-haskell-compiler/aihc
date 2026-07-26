module System.IO
  ( Handle,
    IOMode (..),
    hClose,
    hGetBuf,
    hPutBuf,
    openBinaryFile,
    stdin,
    stdout,
    stderr,
  )
where

import GHC.IO.Handle (Handle, hClose)
import GHC.IO.Handle.Text (hGetBuf, hPutBuf)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, stderr, stdin, stdout)
