-- | This module provides type-safe access to IO operations.
--
--   It is designed to be imported instead of "System.IO".
--   (It is intended to provide versions of functions from that
--   module which have equivalent functionality but are more
--   typesafe). "System.Path" is a companion module providing
--   a type-safe alternative to "System.FilePath".
--
--   You will typically want to import as follows:
--
--   > import Prelude hiding (FilePath)
--   > import qualified System.Path as Path
--   > import qualified System.Path.Directory as Dir
--   > import qualified System.Path.IO as PIO
--
--
-- Ben Moseley - (c) 2009
--
module System.Path.IO
(
  -- * Covers for System.IO functions
  withFile,
  openFile,
  readFile,
  writeFile,
  appendFile,
  withBinaryFile,
  openBinaryFile,
  openTempFile,
  openBinaryTempFile,

  -- * Re-exports
  IO,
  SIO.fixIO,
  Handle,
  SIO.stdin,
  SIO.stdout,
  SIO.stderr,
  SIO.IOMode(..),
  SIO.hClose,
  SIO.hFileSize,
  SIO.hSetFileSize,
  SIO.hIsEOF,
  SIO.isEOF,
  SIO.BufferMode(..),
  SIO.hSetBuffering,
  SIO.hGetBuffering,
  SIO.hFlush,
  SIO.hGetPosn,
  SIO.hSetPosn,
  SIO.HandlePosn,
  SIO.hSeek,
  SIO.SeekMode(..),
  SIO.hTell,
  SIO.hIsOpen,
  SIO.hIsClosed,
  SIO.hIsReadable,
  SIO.hIsWritable,
  SIO.hIsSeekable,
  SIO.hIsTerminalDevice,
  SIO.hSetEcho,
  SIO.hGetEcho,
  SIO.hShow,
  SIO.hWaitForInput,
  SIO.hReady,
  SIO.hGetChar,
  SIO.hGetLine,
  SIO.hLookAhead,
  SIO.hGetContents,
  SIO.hPutChar,
  SIO.hPutStr,
  SIO.hPutStrLn,
  SIO.hPrint,
  interact,
  putChar,
  putStr,
  putStrLn,
  print,
  getChar,
  getLine,
  getContents,
  readIO,
  readLn,
  SIO.hSetBinaryMode,
  SIO.hPutBuf,
  SIO.hGetBuf,
  SIO.hPutBufNonBlocking,
  SIO.hGetBufNonBlocking,
)

where

import qualified System.Path.Internal.PartClass as Class
import qualified System.Path as Path
import System.Path (DirPath, FilePath, AbsFile, RelFile)

import qualified System.IO as SIO
import System.IO (IOMode, Handle)

import Control.Applicative ((<$>))
import Data.Tuple.HT (mapFst)

import Prelude hiding (FilePath, readFile, writeFile, appendFile)


------------------------------------------------------------------------
-- Covers for System.IO functions

withFile ::
    Class.AbsRel ar => FilePath ar -> IOMode -> (Handle -> IO r) -> IO r
withFile f = SIO.withFile (Path.toString f)

openFile :: Class.AbsRel ar => FilePath ar -> IOMode -> IO Handle
openFile f = SIO.openFile (Path.toString f)

readFile :: Class.AbsRel ar => FilePath ar -> IO String
readFile f = SIO.readFile (Path.toString f)

writeFile :: Class.AbsRel ar => FilePath ar -> String -> IO ()
writeFile f = SIO.writeFile (Path.toString f)

appendFile :: Class.AbsRel ar => FilePath ar -> String -> IO ()
appendFile f = SIO.appendFile (Path.toString f)

withBinaryFile ::
    Class.AbsRel ar => FilePath ar -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile f = SIO.withBinaryFile (Path.toString f)

openBinaryFile :: Class.AbsRel ar => FilePath ar -> IOMode -> IO Handle
openBinaryFile f = SIO.openBinaryFile (Path.toString f)

openTempFile ::
    Class.AbsRel ar => DirPath ar -> RelFile -> IO (AbsFile, Handle)
openTempFile f template =
    mapFst Path.absFile <$>
    SIO.openTempFile (Path.toString f) (Path.toString template)

openBinaryTempFile ::
    Class.AbsRel ar => DirPath ar -> RelFile -> IO (AbsFile, Handle)
openBinaryTempFile f template =
    mapFst Path.absFile <$>
    SIO.openBinaryTempFile (Path.toString f) (Path.toString template)
