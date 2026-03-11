module Sound.ALSA.PCM.Node.File (
   -- * Types
   Handle,
   Time, SampleFreq, Size,
   -- * Classes
   Class.SampleFmt,
   -- * Management of streams
   open, close,
   -- * Data transfer
   read, write,
   ) where

import qualified Sound.ALSA.PCM.Core.Class as Class
import Sound.ALSA.PCM.Parameters.Hardware (Time, SampleFreq, Size, )
import Sound.ALSA.PCM.Core.Handle (arraySize, )

import Control.Monad (liftM, )
import Foreign.Ptr (Ptr, )
import qualified System.IO as IO
import System.IO (IOMode, openBinaryFile, )

import Prelude hiding (read, )


newtype Handle y = Handle IO.Handle


open ::
   (Class.SampleFmt y) =>
   IOMode -> FilePath -> IO (Handle y)
open mode path =
   fmap Handle $ openBinaryFile path mode

close ::
   (Class.SampleFmt y) =>
   Handle y -> IO ()
close (Handle h) =
   IO.hClose h

{- |
This expects pad bytes that are needed in memory
in order to satisfy aligment constraints.
This is only a problem for samples sizes like 24 bit.
-}
read ::
   (Class.SampleFmt y) =>
   Handle y -> Ptr y -> Size -> IO Size
read (Handle h) buf n =
   liftM fromIntegral $
   liftM (`div` arraySize buf 1) $
   IO.hGetBuf h buf (arraySize buf (fromIntegral n))

{- |
Same restrictions as for 'fileRead'.
-}
write ::
   (Class.SampleFmt y) =>
   Handle y -> Ptr y -> Size -> IO ()
write (Handle h) buf n =
   IO.hPutBuf h buf (arraySize buf (fromIntegral n))
