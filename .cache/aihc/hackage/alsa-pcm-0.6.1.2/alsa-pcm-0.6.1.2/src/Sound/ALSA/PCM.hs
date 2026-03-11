{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.PCM
    (Class.SampleFmt(..),
     SampleFreq,
     Time,
     Size,
     SoundFmt(..),
     SoundSource(..),
     SoundSink(..),
     SoundBufferTime(..),
     Pcm, File,
     withSoundSource,
     withSoundSourceRunning,
     withSoundSink,
     withSoundSinkRunning,
     soundFmtMIME,
     audioBytesPerSample,
     audioBytesPerFrame,
     soundSourceBytesPerFrame,
     soundSinkBytesPerFrame,
     copySound,
     alsaSoundSource,
     alsaSoundSink,
     alsaSoundSourceTime,
     alsaSoundSinkTime,
     alsaSoundSourceParams,
     alsaSoundSinkParams,
     fileSoundSource,
     fileSoundSink,
    ) where

import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Node.File as File

import Sound.ALSA.PCM.Parameters.Hardware (Time, SampleFreq, Size, )
import Sound.ALSA.PCM.Core.Handle (arraySize, )
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam
import qualified Sound.ALSA.PCM.Core.Class as Class
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Sound.Frame as Frame

import Control.Exception (bracket, bracket_, )
import Control.Monad (when, liftM2, liftM4, )

import qualified Foreign.C.Types as C
import Foreign.Marshal.Array (allocaArray, )
import Foreign.Ptr (Ptr, )
import Foreign.Storable (Storable, )
import qualified System.IO as IO

--
-- * Generic sound API
--

data SoundFmt y = SoundFmt {
        sampleFreq :: SampleFreq
        }
  deriving (Show)

data SoundBufferTime = SoundBufferTime {
        bufferTime, periodTime :: Time
        }
  deriving (Show)


type Pcm = PCM.Handle PCM.Interleaved

type File = File.Handle


-- | Counts are in samples, not bytes. Multi-channel data is interleaved.
data SoundSource handle y =
   SoundSource {
      soundSourceOpen  :: IO (handle y),
      soundSourceClose :: handle y -> IO (),
      soundSourceStart :: handle y -> IO (),
      soundSourceStop  :: handle y -> IO (),
      soundSourceRead  :: handle y -> Ptr y -> Size -> IO Size
   }

data SoundSink handle y =
   SoundSink {
      soundSinkOpen  :: IO (handle y),
      soundSinkClose :: handle y -> IO (),
      soundSinkWrite :: handle y -> Ptr y -> Size -> IO (),
      soundSinkStart :: handle y -> IO (),
      soundSinkStop  :: handle y -> IO ()
   }


withSoundSource :: SoundSource handle y -> (handle y -> IO a) -> IO a
withSoundSource source =
    bracket (soundSourceOpen source) (soundSourceClose source)

withSoundSourceRunning :: SoundSource handle y -> handle y -> IO a -> IO a
withSoundSourceRunning src h =
    bracket_ (soundSourceStart src h) (soundSourceStop src h)

withSoundSink :: SoundSink handle y -> (handle y -> IO a) -> IO a
withSoundSink sink =
    bracket (soundSinkOpen sink) (soundSinkClose sink)

withSoundSinkRunning :: SoundSink handle y -> handle y -> IO a -> IO a
withSoundSinkRunning src h =
    bracket_ (soundSinkStart src h) (soundSinkStop src h)


withSampleFmt :: (y -> a) -> (SoundFmt y -> a)
withSampleFmt f _ = f undefined

withNodeSample :: (y -> a) -> (node y -> a)
withNodeSample f _ = f undefined


soundFmtMIME :: Class.SampleFmt y => SoundFmt y -> String
soundFmtMIME fmt = t ++ r ++ c
  where t = "audio/basic"
{-
        t = case sampleFmt fmt of
                SampleFmtLinear16BitSignedLE -> "audio/L16"
                SampleFmtMuLaw8Bit           -> "audio/basic"
-}
        r = ";rate=" ++ show (sampleFreq fmt)
        c =
           if numChannels fmt == 1
             then ""
             else ";channels=" ++ show (numChannels fmt)

numChannels :: Class.SampleFmt y => SoundFmt y -> Int
numChannels = withSampleFmt Frame.numberOfChannels

audioBytesPerSample :: Class.SampleFmt y => SoundFmt y -> Int
audioBytesPerSample = withSampleFmt Frame.sizeOfElement

{-
assumes interleaved data

Due to alignment constraints
a frame might occupy more than the calculated size
in an array in memory.
-}
audioBytesPerFrame :: Class.SampleFmt y => SoundFmt y -> Int
audioBytesPerFrame fmt = numChannels fmt * audioBytesPerSample fmt

soundSourceBytesPerFrame :: Class.SampleFmt y => SoundSource handle y -> Int
soundSourceBytesPerFrame =
   withNodeSample $ \y -> Frame.numberOfChannels y * Frame.sizeOfElement y

soundSinkBytesPerFrame :: Class.SampleFmt y => SoundSink handle y -> Int
soundSinkBytesPerFrame =
   withNodeSample $ \y -> Frame.numberOfChannels y * Frame.sizeOfElement y

copySound ::
   Class.SampleFmt y =>
      SoundSource handleIn y
   -> SoundSink handleOut y
   -> Size -- ^ Buffer size (in sample frames) to use
   -> IO ()
copySound source sink bufSize =
    allocaArray     (fromIntegral bufSize) $ \buf ->
    withSoundSource source  $ \from ->
    withSoundSink   sink    $ \to ->
       let loop = do n <- soundSourceRead source from buf bufSize
                     when (n > 0) $ do soundSinkWrite sink to buf n
                                       loop
        in loop

--
-- * Alsa stuff
--


{- |
The buffer is initialized with an empty block
which means that the zero bit pattern
should equal the number zero in the Class.SampleFmt type.
-}
alsaOpen :: Class.SampleFmt y =>
   PCM.Stream ->
   HwParam.T PCM.Interleaved y a ->
   (a -> SwParam.T PCM.Interleaved y ()) ->
   String {- ^ device, e.g @"default"@ -} ->
   IO (Pcm y)
alsaOpen stream hwParams swParams dev = AlsaExc.rethrow $ do
   Debug.put "alsaOpen"
   {-
   Debug.put $ "requested bufferTime = " ++ show (bufferTime time)
   Debug.put $ "requested periodTime = " ++ show (periodTime time)
   -}
   ((bufferTime_,bufferSize_,periodTime_,periodSize_), h) <-
      PCM.open (PCM.modes []) stream
         (liftM2 (,) hwParams $
          liftM4 (,,,)
             HwParam.getBufferSize
             HwParam.getBufferTime
             (fmap fst HwParam.getPeriodSize)
             (fmap fst HwParam.getPeriodTime))
         (\(a, params) -> swParams a >> return params)
         dev
   PCM.prepare h
   Debug.put $ "bufferTime = " ++ show bufferTime_
   Debug.put $ "bufferSize = " ++ show bufferSize_
   Debug.put $ "periodTime = " ++ show periodTime_
   Debug.put $ "periodSize = " ++ show periodSize_
   when (stream == PCM.StreamPlayback) $
      callocaArray periodSize_ $ \buf ->
         PCM.writei h buf (fromIntegral periodSize_) >> return ()
   return h

alsaClose :: Pcm y -> IO ()
alsaClose h = AlsaExc.rethrow $ do
   Debug.put "alsaClose"
   PCM.drain h
   PCM.close h


alsaStart :: Pcm y -> IO ()
alsaStart h = AlsaExc.rethrow $ do
   Debug.put "alsaStart"
   PCM.prepare h
   PCM.start h

-- FIXME: use PCM.drain for sinks?
alsaStop :: Pcm y -> IO ()
alsaStop h = AlsaExc.rethrow $ do
   Debug.put "alsaStop"
   PCM.drain h


alsaRead ::
   Class.SampleFmt y =>
   Pcm y -> Ptr y -> Size -> IO Size
alsaRead h buf0 n =
   AlsaExc.rethrow $ PCM.readiRetry h buf0 n

alsaWrite ::
   Class.SampleFmt y =>
   Pcm y -> Ptr y -> Size -> IO Size
alsaWrite h buf0 n =
   AlsaExc.rethrow $ PCM.writeiRetry h buf0 n


defaultBufferTime :: SoundBufferTime
defaultBufferTime =
   SoundBufferTime {
      bufferTime = 500000, -- 0.5s
      periodTime = 100000  -- 0.1s
   }

bufferTimeParams ::
   SoundFmt y ->
   SoundBufferTime ->
   (HwParam.T PCM.Interleaved y (Size,Size),
    (Size,Size) -> SwParam.T PCM.Interleaved y ())
bufferTimeParams fmt time =
   (HwParam.setRateBufferTime
       (sampleFreq fmt)
       (bufferTime time)
       (periodTime time),
    uncurry SwParam.setBufferSize)

alsaSoundSource ::
   Class.SampleFmt y =>
   String -> SoundFmt y -> SoundSource Pcm y
alsaSoundSource dev fmt =
   alsaSoundSourceTime dev fmt defaultBufferTime

alsaSoundSink ::
   Class.SampleFmt y =>
   String -> SoundFmt y -> SoundSink Pcm y
alsaSoundSink dev fmt =
   alsaSoundSinkTime dev fmt defaultBufferTime

alsaSoundSourceTime ::
   Class.SampleFmt y =>
   String -> SoundFmt y -> SoundBufferTime -> SoundSource Pcm y
alsaSoundSourceTime dev fmt time =
   uncurry (alsaSoundSourceParams dev) $
   bufferTimeParams fmt time

alsaSoundSinkTime ::
   Class.SampleFmt y =>
   String -> SoundFmt y -> SoundBufferTime -> SoundSink Pcm y
alsaSoundSinkTime dev fmt time =
   uncurry (alsaSoundSinkParams dev) $
   bufferTimeParams fmt time

alsaSoundSourceParams ::
   Class.SampleFmt y =>
   String ->
   HwParam.T PCM.Interleaved y a ->
   (a -> SwParam.T PCM.Interleaved y ()) ->
   SoundSource Pcm y
alsaSoundSourceParams dev hwParams swParams =
   SoundSource {
      soundSourceOpen  = alsaOpen PCM.StreamCapture hwParams swParams dev,
      soundSourceClose = alsaClose,
      soundSourceStart = alsaStart,
      soundSourceStop  = alsaStop,
      soundSourceRead  = alsaRead
   }

alsaSoundSinkParams ::
   Class.SampleFmt y =>
   String ->
   HwParam.T PCM.Interleaved y a ->
   (a -> SwParam.T PCM.Interleaved y ()) ->
   SoundSink Pcm y
alsaSoundSinkParams dev hwParams swParams =
   SoundSink {
      soundSinkOpen  = alsaOpen PCM.StreamPlayback hwParams swParams dev,
      soundSinkClose = alsaClose,
      soundSinkStart = alsaStart,
      soundSinkStop  = alsaStop,
      soundSinkWrite = \h buf n -> alsaWrite h buf n >> return ()
   }

--
-- * File stuff
--

fileSoundSource ::
   Class.SampleFmt y =>
   FilePath -> SoundSource File y
fileSoundSource file =
   SoundSource {
      soundSourceOpen  = File.open IO.ReadMode file,
      soundSourceClose = File.close,
      soundSourceRead  = File.read,
      soundSourceStart = const $ return (),
      soundSourceStop  = const $ return ()
   }

fileSoundSink ::
   Class.SampleFmt y =>
   FilePath -> SoundSink File y
fileSoundSink file =
   SoundSink {
      soundSinkOpen  = File.open IO.WriteMode file,
      soundSinkClose = File.close,
      soundSinkWrite = File.write,
      soundSinkStart = const $ return (),
      soundSinkStop  = const $ return ()
   }

--
-- * Marshalling utilities
--

callocaArray :: Storable y => Size -> (Ptr y -> IO b) -> IO b
callocaArray n0 f =
   case fromIntegral n0 of
      n ->
         allocaArray n $ \p ->
            clearBytes p (arraySize p n) >>
            f p

clearBytes :: Ptr a -> Int -> IO ()
clearBytes p n = memset p 0 (fromIntegral n) >> return ()

foreign import ccall unsafe "string.h" memset :: Ptr a -> C.CInt -> C.CSize -> IO (Ptr a)
