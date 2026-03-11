module Sound.ALSA.PCM.Core.Class where

import qualified Sound.ALSA.PCM.Core.Handle as Core
import qualified Sound.ALSA.PCM.Core.HwParams as HwParams
import qualified Sound.ALSA.PCM.Core.SwParams as SwParams

import qualified Sound.Frame as Frame
import qualified Sound.Frame.Stereo as Stereo
import qualified Sound.Frame.MuLaw  as MuLaw

import Data.Word (Word8, Word16, Word32, )
import Data.Int (Int8, Int16, Int32, )

import Foreign (Storable, )


class Access i where
   access :: HwParams.T i y HwParams.Access
   setChannels :: (Frame.C y) => HwParams.T i y ()

instance Access Core.Interleaved where
   access = return HwParams.accessRwInterleaved
   setChannels =
      HwParams.setChannels . fromIntegral =<<
         withSampleFmt Frame.numberOfChannels

instance Access Core.Noninterleaved where
   access = return HwParams.accessRwNoninterleaved
   setChannels = return ()



class (Storable y, Frame.C y) => SampleFmt y where
   sampleFmtToPcmFormat :: y -> HwParams.Format

instance SampleFmt Word8 where
   sampleFmtToPcmFormat _ = HwParams.formatU8

instance SampleFmt Int8 where
   sampleFmtToPcmFormat _ = HwParams.formatS8

instance SampleFmt Word16 where
   sampleFmtToPcmFormat _ = HwParams.formatU16

instance SampleFmt Int16 where
   sampleFmtToPcmFormat _ = HwParams.formatS16

instance SampleFmt Word32 where
   sampleFmtToPcmFormat _ = HwParams.formatU32

instance SampleFmt Int32 where
   sampleFmtToPcmFormat _ = HwParams.formatS32

instance SampleFmt Float where
   sampleFmtToPcmFormat _ = HwParams.formatFloat

instance SampleFmt Double where
   sampleFmtToPcmFormat _ = HwParams.formatFloat64

instance SampleFmt MuLaw.T where
   sampleFmtToPcmFormat _ = HwParams.formatMuLaw

instance SampleFmt a => SampleFmt (Stereo.T a) where
   sampleFmtToPcmFormat y =
      sampleFmtToPcmFormat (Stereo.left y)


{- |
Sample types of this class must have exactly one channel,
i.e. Frame.numberOfChannels == 1.
-}
class (SampleFmt y) => MonoSampleFmt y where

instance MonoSampleFmt Word8 where
instance MonoSampleFmt Int8 where
instance MonoSampleFmt Word16 where
instance MonoSampleFmt Int16 where
instance MonoSampleFmt Word32 where
instance MonoSampleFmt Int32 where
instance MonoSampleFmt Float where
instance MonoSampleFmt Double where
instance MonoSampleFmt MuLaw.T where


withSampleFmt :: (y -> a) -> HwParams.T i y a
withSampleFmt f = return $ f undefined


withHwParams ::
   (Access i, SampleFmt y) =>
   Core.Handle i y -> HwParams.T i y a -> IO a
withHwParams h f =
   HwParams.withIO h $ (\(HwParams.Cons act) -> act h) $ do
      HwParams.setAccess =<< access
      HwParams.setFormat =<< withSampleFmt sampleFmtToPcmFormat
      setChannels
      f

withSwParams ::
   (Access i, SampleFmt y) =>
   Core.Handle i y -> SwParams.T i y a -> IO a
withSwParams h (SwParams.Cons f) =
   SwParams.withIO h $ f h
