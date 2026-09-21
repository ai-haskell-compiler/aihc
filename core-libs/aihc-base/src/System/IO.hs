{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | The standard IO interface.
module System.IO
  ( IO,
    fixIO,
    FilePath,
    Handle,
    stdin,
    stdout,
    stderr,
    withFile,
    openFile,
    IOMode (..),
    hClose,
    readFile,
    readFile',
    writeFile,
    appendFile,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    BufferMode (..),
    hSetBuffering,
    hGetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    HandlePosn,
    hSeek,
    SeekMode (..),
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hSetEcho,
    hGetEcho,
    hShow,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents',
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,
    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    getContents',
    readIO,
    readLn,
    withBinaryFile,
    openBinaryFile,
    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
    hSetBinaryMode,
    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,
    hSetEncoding,
    hGetEncoding,
    TextEncoding,
    latin1,
    utf8,
    utf8_bom,
    utf16,
    utf16le,
    utf16be,
    utf32,
    utf32le,
    utf32be,
    localeEncoding,
    char8,
    mkTextEncoding,
    hSetNewlineMode,
    Newline (..),
    nativeNewline,
    NewlineMode (..),
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import Data.Bits ((.|.))
import Data.List (break, elem, reverse)
import Data.Maybe (Maybe (..))
import Foreign.C.Error (eEXIST, eINTR, errnoToIOError, getErrno)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import GHC.IO (mask_, onException)
import GHC.IO.Encoding
  ( TextEncoding,
    char8,
    latin1,
    mkTextEncoding,
    utf16,
    utf16be,
    utf16le,
    utf32,
    utf32be,
    utf32le,
    utf8,
    utf8_bom,
  )
import GHC.IO.Handle
import GHC.IO.Handle.FD (fdToHandle')
import GHC.IO.Handle.Text (hGetBuf, hGetBufNonBlocking, hGetBufSome, hGetChar, hGetContents, hGetContents', hGetLine, hPutBuf, hPutBufNonBlocking, hPutChar, hPutStr, hPutStrLn, hWaitForInput)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, openFile, stderr, stdin, stdout, withBinaryFile, withFile)
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..), ioError)
import System.Posix.Internals (c_unlink, o_CREAT, o_EXCL, o_RDWR, withFilePath)
import System.Posix.Types (CMode (..))
import Prelude (Bool (..), Char, FilePath, IO, Integer, Read (..), Show (..), String, appendFile, error, getChar, getContents, getLine, interact, otherwise, print, pure, putChar, putStr, putStrLn, readFile, readIO, readLn, writeFile, (+), (++), (<), (==), (>>=))

-- | Create a private temporary file with a read-write handle.
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile = openTempFile' "openTempFile" False 0o600

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = openTempFile' "openBinaryTempFile" True 0o600

-- | Apply the process umask to the default file permissions.
openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions = openTempFile' "openTempFileWithDefaultPermissions" False 0o666

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions = openTempFile' "openBinaryTempFileWithDefaultPermissions" True 0o666

openTempFile' :: String -> Bool -> CMode -> FilePath -> String -> IO (FilePath, Handle)
openTempFile' location binary permissions directory template
  | '/' `elem` template =
      ioError (IOError Nothing InvalidArgument location "template contains a path separator" Nothing (Just template))
  | otherwise = attempt 0
  where
    (prefix, suffix) =
      case break (== '.') (reverse template) of
        (_, []) -> (template, "")
        (extension, '.' : rest) -> (reverse rest, '.' : reverse extension)
        _ -> (template, "")
    base = case directory of
      "" -> ""
      _ -> directory ++ "/"
    attempt :: Integer -> IO (FilePath, Handle)
    attempt number = do
      let path = base ++ prefix ++ show number ++ suffix
      withFilePath path (create path number)
    create path number encoded =
      mask_
        ( do
            descriptor <- openTemporaryFile encoded (o_RDWR .|. o_CREAT .|. o_EXCL) permissions
            if descriptor < 0
              then do
                errno <- getErrno
                if errno == eEXIST
                  then attempt (number + 1)
                  else
                    if errno == eINTR
                      then attempt number
                      else ioError (errnoToIOError location errno Nothing (Just path))
              else do
                handle <-
                  fdToHandle' descriptor Nothing False path ReadWriteMode binary
                    `onException` ( do
                                      _ <- closeTemporaryFile descriptor
                                      _ <- c_unlink encoded
                                      pure ()
                                  )
                pure (path, handle)
        )

foreign import capi unsafe "fcntl.h open"
  openTemporaryFile :: CString -> CInt -> CMode -> IO CInt

foreign import capi unsafe "unistd.h close"
  closeTemporaryFile :: CInt -> IO CInt

-- | Handles cannot tell whether they are terminals.
hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice _ = pure False

hReady :: Handle -> IO Bool
hReady handle = hWaitForInput handle 0

hPrint :: (Show a) => Handle -> a -> IO ()
hPrint handle value = hPutStrLn handle (show value)

getContents' :: IO String
getContents' = hGetContents' stdin

readFile' :: FilePath -> IO String
readFile' path = withFile path ReadMode hGetContents'

fixIO :: (a -> IO a) -> IO a
fixIO _ = error "System.IO.fixIO: not available"

-- The runtime only has UTF-8, so the locale encoding is UTF-8.
localeEncoding :: TextEncoding
localeEncoding = utf8
