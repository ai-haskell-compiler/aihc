{- |
Demonstrate how to adapt to the parameters 'sample rate' and 'buffer size'
that are supported natively by the hardware.
-}
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB

import Control.Exception (bracket, )



openPCM ::
   (PCM.Access i, PCM.SampleFmt y) =>
   IO (PCM.Size, PCM.SampleFreq, PCM.Handle i y)
openPCM = do
   Debug.put "alsaOpenTest"
   (((bufferSize,periodSize),(bufferTime,periodTime),sampleRate), h) <-
      PCM.open (PCM.modes []) PCM.StreamPlayback
         (setHwParams 44100 1024 64)
         (\q@(sizes,_,_) -> do
             uncurry SwParam.setBufferSize sizes
             return q)
         "plughw:0"
   PCM.prepare h
   Debug.put $ "bufferTime = " ++ show bufferTime
   Debug.put $ "bufferSize = " ++ show bufferSize
   Debug.put $ "periodTime = " ++ show periodTime
   Debug.put $ "periodSize = " ++ show periodSize
   return (periodSize, sampleRate, h)

closePCM :: (PCM.Size, PCM.SampleFreq, PCM.Handle i y) -> IO ()
closePCM (_,_,pcm) = do
   Debug.put "alsaClose"
   PCM.drain pcm
   PCM.close pcm

setHwParams ::
      PCM.SampleFreq -- ^ sample frequency
   -> PCM.Size -- ^ buffer size
   -> PCM.Size -- ^ period size
   -> HwParam.T i y ((PCM.Size,PCM.Size),(PCM.Time,PCM.Time),PCM.SampleFreq)
      -- ^ ((bufferSize,periodSize),(bufferTime,periodTime),sampleRate)
setHwParams rate bufferSize periodSize = do
{-
   (actualRate,ord) <- HwParam.getRateMax
   print ord
-}
   HwParam.setRateResample False
   (actualRate,_) <-
      HwParam.setRateNear rate EQ
   (actualPeriodSize,_) <-
      HwParam.setPeriodSizeNear periodSize EQ
   actualBufferSize <-
      HwParam.setBufferSizeNear
         (max bufferSize (actualPeriodSize*2))
{-
   let actualBufferSize = bufferSize
   HwParam.setBufferSize bufferSize
-}
   (actualBufferTime,_) <- HwParam.getBufferTime
   (actualPeriodTime,_) <- HwParam.getPeriodTime
   return ((actualBufferSize, actualPeriodSize),
           (actualBufferTime, actualPeriodTime),
           actualRate)

write ::
   (PCM.SampleFmt y) =>
   PCM.Handle PCM.Interleaved y -> SVB.Vector y -> IO ()
write h xs =
   SVB.withStartPtr xs $ \buf ->
      fmap (const ()) . PCM.writeiRetry h buf . fromIntegral


main :: IO ()
main =
   bracket openPCM closePCM $ \(size,rate,h) -> do
   putStrLn $ "period size: " ++ show size
   putStrLn $ "sample rate: " ++ show rate
   mapM_ (write h) $
      SVL.chunks $
      SVL.map ((0.99*) . sin . ((2*pi * 440 / fromIntegral rate :: Float)*)) $
      SVL.iterate (SVL.chunkSize $ fromIntegral size) (1 +) 0
