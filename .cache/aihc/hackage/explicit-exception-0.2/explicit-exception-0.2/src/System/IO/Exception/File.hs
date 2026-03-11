module System.IO.Exception.File where

import System.IO.Straight (ExceptionalT, IOException, SIO, ioToExceptionalSIO, )
import qualified System.IO as IO
-- import System.IO (Handle, IOMode, )


type EIO = ExceptionalT IOException SIO

close :: IO.Handle -> EIO ()
close h =
   ioToExceptionalSIO $ IO.hClose h
