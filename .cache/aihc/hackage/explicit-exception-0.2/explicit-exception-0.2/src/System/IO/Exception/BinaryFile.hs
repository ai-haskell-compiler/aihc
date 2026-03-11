{- |
Files with binary content.
-}
module System.IO.Exception.BinaryFile where

import System.IO.Exception.File (EIO, close, )
import Control.Monad.Exception.Synchronous (bracketT, )
import System.IO.Straight (ioToExceptionalSIO, )
import System.IO (Handle, IOMode, )
import qualified System.IO as IO
import Data.Word (Word8, )
import Data.Char (ord, chr, )


open :: FilePath -> IOMode -> EIO Handle
open name mode =
   ioToExceptionalSIO $ IO.openBinaryFile name mode

with ::
   FilePath -> IOMode -> (Handle -> EIO r) -> EIO r
with name mode =
   bracketT (open name mode) close

getByte :: Handle -> EIO Word8
getByte h =
   ioToExceptionalSIO $ fmap (fromIntegral . ord) $ IO.hGetChar h

putByte :: Handle -> Word8 -> EIO ()
putByte h c =
   ioToExceptionalSIO $ IO.hPutChar h (chr . fromIntegral $ c)
