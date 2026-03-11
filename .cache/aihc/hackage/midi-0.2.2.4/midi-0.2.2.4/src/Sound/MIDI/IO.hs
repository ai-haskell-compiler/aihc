{- |
Taken from Haskore.
-}
module Sound.MIDI.IO
          (openBinaryFile, readBinaryFile, writeBinaryFile,
           ByteList, listCharFromByte, listByteFromChar)
   where

import System.IO
import Control.Exception(bracket)
import Control.Monad(liftM)
import Data.Char (ord, chr)
import Data.Word (Word8)

type ByteList = [Word8]

{- |
Hugs makes trouble here because it performs UTF-8 conversions.
E.g. @[255]@ is output as @[195,191]@
It would be easy to replace these routines by FastPackedString(fps).ByteList.Lazy,
however this introduces a new package dependency.
-}
writeBinaryFile :: FilePath -> ByteList -> IO ()
writeBinaryFile path str =
   bracket (openBinaryFile path WriteMode) hClose
           (flip hPutStr (listCharFromByte str))

listCharFromByte :: ByteList -> String
listCharFromByte = map (chr . fromIntegral)

readBinaryFile :: FilePath -> IO ByteList
readBinaryFile path =
   liftM listByteFromChar .
      hGetContents =<< openBinaryFile path ReadMode

listByteFromChar :: String -> ByteList
listByteFromChar = map (fromIntegral . ord)
