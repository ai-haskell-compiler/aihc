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
import GHC.IO.Handle.Text (hGetBuf, hGetBufNonBlocking, hGetBufSome, hGetChar, hGetContents, hGetContents', hGetLine, hPutBuf, hPutBufNonBlocking, hPutChar, hPutStr, hPutStrLn, hWaitForInput)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, openFile, stderr, stdin, stdout, withBinaryFile, withFile)
import Prelude (Bool (..), Char, FilePath, IO, Read (..), Show (..), String, appendFile, error, getChar, getContents, getLine, interact, print, pure, putChar, putStr, putStrLn, readFile, readIO, readLn, writeFile, (>>=))

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
