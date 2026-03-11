module Sound.MED.Basic.Amiga (
  Peek, Reader(..), peekPTR,
  StorableReader, runStorable,
  ByteStringReader, runByteString,
  PTR, LONG, ULONG, WORD, UWORD, BYTE, UBYTE,
  loadMEM, freeMEM,
  ) where

import Sound.MED.Basic.AmigaPrivate

import Sound.MED.Basic.Storable (MEM)
import Sound.MED.Basic.Utility (PTR, LONG, ULONG, WORD, UWORD, BYTE, UBYTE)

import qualified System.IO as IO

import qualified Foreign.Marshal.Alloc as Alloc

import Control.Monad (when)
import Control.Applicative ((<$>))


loadMEM :: String -> IO MEM
loadMEM s =
  IO.withBinaryFile s IO.ReadMode $ \h -> do
    size <- fromInteger <$> IO.hFileSize h
    ptr <- Alloc.mallocBytes size
    readSize <- IO.hGetBuf h ptr size
    when (readSize<size) $ fail $ "loadMEM: incomplete load of " ++ s
    return ptr

freeMEM :: MEM -> IO ()
freeMEM = Alloc.free
