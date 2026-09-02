module System.IO
  ( Handle,
    IOMode (..),
    BufferMode (..),
    withBinaryFile,
    withFile,
    openFile,
    hFlush,
    hFileSize,
    hGetBufSome,
    hGetBufNonBlocking,
    hPutBufNonBlocking,
    hSetBinaryMode,
    hSetBuffering,
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

import Control.Exception.Base (bracket)
import GHC.IO.Handle (Handle, hClose)
import GHC.IO.Handle.Text (hGetBuf, hPutBuf, hPutStr)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, stderr, stdin, stdout)
import GHC.Ptr (Ptr)
import Prelude (Bool (..), Eq (..), FilePath, IO, Int, Integer, Maybe (..), Ord (..), Ordering (..), Show (..), String, error, not, pure, showParen, showString, (++), (.), (>>))

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle value = hPutStr handle (value ++ "\n")

writeFile :: FilePath -> String -> IO ()
writeFile path value = do
  handle <- openBinaryFile path WriteMode
  hPutStr handle value
  hClose handle

readFile :: FilePath -> IO String
readFile path = pure (error ("System.IO.readFile is not available: " ++ path))

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

instance Eq BufferMode where
  NoBuffering == NoBuffering = True
  LineBuffering == LineBuffering = True
  BlockBuffering left == BlockBuffering right = left == right
  _ == _ = False
  left /= right = not (left == right)

instance Ord BufferMode where
  compare left right = compare (bufferModeIndex left) (bufferModeIndex right)
  left < right = compare left right == LT
  left <= right = compare left right /= GT
  left > right = compare left right == GT
  left >= right = compare left right /= LT
  min left right = if left <= right then left else right
  max left right = if left >= right then left else right

bufferModeIndex :: BufferMode -> (Int, Maybe Int)
bufferModeIndex NoBuffering = (0, Nothing)
bufferModeIndex LineBuffering = (1, Nothing)
bufferModeIndex (BlockBuffering size) = (2, size)

instance Show BufferMode where
  showsPrec _ NoBuffering = showString "NoBuffering"
  showsPrec _ LineBuffering = showString "LineBuffering"
  showsPrec precedence (BlockBuffering size) =
    showParen (precedence > 10) (showString "BlockBuffering " . showsPrec 11 size)

-- | Files are always opened in binary mode.
openFile :: FilePath -> IOMode -> IO Handle
openFile = openBinaryFile

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode = bracket (openFile path mode) hClose

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile path mode = bracket (openBinaryFile path mode) hClose

-- | Handles are unbuffered, so there is nothing to flush.
hFlush :: Handle -> IO ()
hFlush _ = pure ()

hFileSize :: Handle -> IO Integer
hFileSize _ = error "System.IO.hFileSize: file sizes are not available"

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome = hGetBuf

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = hGetBuf

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking handle buffer count = hPutBuf handle buffer count >> pure count

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode _ _ = pure ()

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = pure ()
