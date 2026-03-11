module Sound.ALSA.PCM.Node.ALSA (
   -- * Types
   Handle,
   PCM.Stream(..),
   PCM.Mode, PCM.modes, PCM.nonBlock, PCM.async,
   Interleaved, Noninterleaved,
   Time, SampleFreq, Size,
   -- * Classes
   Class.Access, Class.SampleFmt, Class.MonoSampleFmt,
   -- * Management of streams
   open,
   PCM.close,
   PCM.prepare,
   PCM.start,
   PCM.drop,
   PCM.drain,
   -- * Data transfer
   readi, writei, readiRetry, writeiRetry,
   readn, writen,
   ) where

import Sound.ALSA.PCM.Parameters.Hardware (Time, SampleFreq, Size, )
import Sound.ALSA.PCM.Core.Handle (Handle, Interleaved, Noninterleaved, prepare, )
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam
import qualified Sound.ALSA.PCM.Core.Class as Class
import qualified Sound.ALSA.PCM.Core.Handle as PCM
import qualified Sound.ALSA.PCM.Debug as Debug
import qualified Sound.ALSA.Exception as AlsaExc

import Foreign.Marshal.Array (advancePtr, )
import Foreign (Ptr, )


open ::
   (Class.Access i, Class.SampleFmt y) =>
      PCM.Mode
   -> PCM.Stream
   -> HwParam.T i y a
   -> (a -> SwParam.T i y b)
   -> String -- ^ device, e.g @\"default\"@
   -> IO (b, Handle i y)
open mode stream hwp swp dev = do
   h <- PCM.open dev stream mode
   a <- Class.withHwParams h hwp
   b <- Class.withSwParams h $ swp a
   return (b, h)


readi ::
   (Class.SampleFmt y) =>
   Handle Interleaved y -> Ptr y -> Size -> IO Size
readi = PCM.readi

writei ::
   (Class.SampleFmt y) =>
   Handle Interleaved y -> Ptr y -> Size -> IO Size
writei = PCM.writei

{- |
The @Ptr (Ptr y)@ argument is actually a pointer to an array of pointers.
The array must have the size of number of channels.
In 'Noninterleaved' mode you must set the number of channels manually
using 'HwParam.setChannels' or its friends.
It is an unchecked error if the number of channels
set with 'HwParam.setChannels'
does not match the array size in the 'readn' call.
-}
readn ::
   (Class.MonoSampleFmt y) =>
   Handle Noninterleaved y -> Ptr (Ptr y) -> Size -> IO Size
readn = PCM.readn

{- |
Cf. 'readn'.
-}
writen ::
   (Class.MonoSampleFmt y) =>
   Handle Noninterleaved y -> Ptr (Ptr y) -> Size -> IO Size
writen = PCM.writen



{- |
retry on buffer over-run
-}
readiRetry ::
   Class.SampleFmt y =>
   Handle Interleaved y -> Ptr y -> Size -> IO Size
readiRetry h buf0 n =
   let go buf offset = do
          -- debug $ "Reading " ++ show n ++ " samples..."
          nread <-
             readi h buf (n-offset)
             `AlsaExc.catchXRun`
             do Debug.put "snd_pcm_readi reported buffer over-run"
                prepare h
                go buf offset
          let newOffset = offset+nread
          -- debug $ "Got " ++ show n' ++ " samples."
          if newOffset < n
            then go (advancePtr buf (fromIntegral nread)) newOffset
            else return newOffset
   in  go buf0 0

{- |
retry on buffer under-run
-}
writeiRetry ::
   Class.SampleFmt y =>
   Handle Interleaved y -> Ptr y -> Size -> IO Size
writeiRetry h buf0 n =
   let go buf offset = do
          -- debug $ "Writing " ++ show n ++ " samples..."
          nwritten <-
             writei h buf (n-offset)
             `AlsaExc.catchXRun`
             do Debug.put "snd_pcm_writei reported buffer under-run"
                prepare h
                go buf offset
          let newOffset = offset+nwritten
          --debug $ "Wrote " ++ show n' ++ " samples."
          if newOffset < n
            then go (advancePtr buf (fromIntegral nwritten)) newOffset
            else return newOffset
   in  go buf0 0
