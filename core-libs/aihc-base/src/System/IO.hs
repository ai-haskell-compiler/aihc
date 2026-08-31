module System.IO
  ( Handle,
    IOMode (..),
    hClose,
    hGetBuf,
    hPutBuf,
    hPutStr,
    hPutStrLn,
    openBinaryFile,
    readFile,
    writeFile,
    stdin,
    stdout,
    stderr,
  )
where

import GHC.IO.Handle (Handle, hClose)
import GHC.IO.Handle.Text (hGetBuf, hPutBuf, hPutStr)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, stderr, stdin, stdout)
import Prelude (FilePath, IO, String, error, pure, (++), (>>))

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle value = hPutStr handle (value ++ "\n")

writeFile :: FilePath -> String -> IO ()
writeFile path value = do
  handle <- openBinaryFile path WriteMode
  hPutStr handle value
  hClose handle

readFile :: FilePath -> IO String
readFile path = pure (error ("System.IO.readFile is not available: " ++ path))
