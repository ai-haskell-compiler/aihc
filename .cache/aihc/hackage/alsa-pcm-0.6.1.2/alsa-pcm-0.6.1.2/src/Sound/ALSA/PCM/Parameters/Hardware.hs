module Sound.ALSA.PCM.Parameters.Hardware (
   -- * Types
   T, SampleFreq, Time, Size,
   Interleaved, Noninterleaved,

{-
   HwParams.Access,
   HwParams.accessMmapInterleaved,
   HwParams.accessMmapNoninterleaved,
   HwParams.accessMmapComplex,
   HwParams.accessRwInterleaved,
   HwParams.accessRwNoninterleaved,

   HwParams.Format,
   HwParams.formatUnknown,
   HwParams.formatS8,
   HwParams.formatU8,
   HwParams.formatS16Le,
   HwParams.formatS16Be,
   HwParams.formatU16Le,
   HwParams.formatU16Be,
   HwParams.formatS24Le,
   HwParams.formatS24Be,
   HwParams.formatU24Le,
   HwParams.formatU24Be,
   HwParams.formatS32Le,
   HwParams.formatS32Be,
   HwParams.formatU32Le,
   HwParams.formatU32Be,
   HwParams.formatFloatLe,
   HwParams.formatFloatBe,
   HwParams.formatFloat64Le,
   HwParams.formatFloat64Be,
   HwParams.formatIec958SubframeLe,
   HwParams.formatIec958SubframeBe,
   HwParams.formatMuLaw,
   HwParams.formatALaw,
   HwParams.formatImaAdpcm,
   HwParams.formatMpeg,
   HwParams.formatGsm,
   HwParams.formatSpecial,
   HwParams.formatS243le,
   HwParams.formatS243be,
   HwParams.formatU243le,
   HwParams.formatU243be,
   HwParams.formatS203le,
   HwParams.formatS203be,
   HwParams.formatU203le,
   HwParams.formatU203be,
   HwParams.formatS183le,
   HwParams.formatS183be,
   HwParams.formatU183le,
   HwParams.formatU183be,
   HwParams.formatS16,
   HwParams.formatU16,
   HwParams.formatS24,
   HwParams.formatU24,
   HwParams.formatS32,
   HwParams.formatU32,
   HwParams.formatFloat,
   HwParams.formatFloat64,
   HwParams.formatIec958Subframe,

   HwParams.Subformat,
-}

   -- * Primitive accessors to hardware parameters
   HwParams.canMmapSampleResolution,
   HwParams.isDouble,
   HwParams.isBatch,
   HwParams.isBlockTransfer,
   HwParams.canOverrange,
   HwParams.canPause,
   HwParams.canResume,
   HwParams.isHalfDuplex,
   HwParams.isJointDuplex,
   HwParams.canSyncStart,
   HwParams.getRateNumden,
   HwParams.getSbits,
   HwParams.getFifoSize,
{- integrated in 'with'
   HwParams.getAccess,
   HwParams.testAccess,
   HwParams.setAccess,
   HwParams.setAccessFirst,
   HwParams.setAccessLast,
   HwParams.getFormat,
   HwParams.testFormat,
   HwParams.setFormat,
   HwParams.setFormatFirst,
   HwParams.setFormatLast,
-}
{- don't know how to use this reasonably
   HwParams.getSubformat,
   HwParams.testSubformat,
   HwParams.setSubformat,
   HwParams.setSubformatFirst,
   HwParams.setSubformatLast,
-}
   HwParams.getChannels,
   HwParams.getChannelsMin,
   HwParams.getChannelsMax,
   HwParams.testChannels,
   setChannels,
   setChannelsMin,
   setChannelsMax,
   setChannelsMinmax,
   setChannelsNear,
   setChannelsFirst,
   setChannelsLast,
   HwParams.getRate,
   HwParams.getRateMin,
   HwParams.getRateMax,
   HwParams.testRate,
   HwParams.setRate,
   HwParams.setRateMin,
   HwParams.setRateMax,
   HwParams.setRateMinmax,
   HwParams.setRateNear,
   HwParams.setRateFirst,
   HwParams.setRateLast,
   HwParams.setRateResample,
   HwParams.getRateResample,
   HwParams.setExportBuffer,
   HwParams.getExportBuffer,
   HwParams.getPeriodTime,
   HwParams.getPeriodTimeMin,
   HwParams.getPeriodTimeMax,
   HwParams.testPeriodTime,
   HwParams.setPeriodTime,
   HwParams.setPeriodTimeMin,
   HwParams.setPeriodTimeMax,
   HwParams.setPeriodTimeMinmax,
   HwParams.setPeriodTimeNear,
   HwParams.setPeriodTimeFirst,
   HwParams.setPeriodTimeLast,
   HwParams.getPeriodSize,
   HwParams.getPeriodSizeMin,
   HwParams.getPeriodSizeMax,
   HwParams.testPeriodSize,
   HwParams.setPeriodSize,
   HwParams.setPeriodSizeMin,
   HwParams.setPeriodSizeMax,
   HwParams.setPeriodSizeMinmax,
   HwParams.setPeriodSizeNear,
   HwParams.setPeriodSizeFirst,
   HwParams.setPeriodSizeLast,
   HwParams.setPeriodSizeInteger,
   HwParams.getPeriods,
   HwParams.getPeriodsMin,
   HwParams.getPeriodsMax,
   HwParams.testPeriods,
   HwParams.setPeriods,
   HwParams.setPeriodsMin,
   HwParams.setPeriodsMax,
   HwParams.setPeriodsMinmax,
   HwParams.setPeriodsNear,
   HwParams.setPeriodsFirst,
   HwParams.setPeriodsLast,
   HwParams.setPeriodsInteger,
   HwParams.getBufferTime,
   HwParams.getBufferTimeMin,
   HwParams.getBufferTimeMax,
   HwParams.testBufferTime,
   HwParams.setBufferTime,
   HwParams.setBufferTimeMin,
   HwParams.setBufferTimeMax,
   HwParams.setBufferTimeMinmax,
   HwParams.setBufferTimeNear,
   HwParams.setBufferTimeFirst,
   HwParams.setBufferTimeLast,
   HwParams.getBufferSize,
   HwParams.getBufferSizeMin,
   HwParams.getBufferSizeMax,
   HwParams.testBufferSize,
   HwParams.setBufferSize,
   HwParams.setBufferSizeMin,
   HwParams.setBufferSizeMax,
   HwParams.setBufferSizeMinmax,
   HwParams.setBufferSizeNear,
   HwParams.setBufferSizeFirst,
   HwParams.setBufferSizeLast,
   HwParams.getTickTime,
   HwParams.getTickTimeMin,
   HwParams.getTickTimeMax,
   HwParams.testTickTime,
   HwParams.setTickTime,
   HwParams.setTickTimeMin,
   HwParams.setTickTimeMax,
   HwParams.setTickTimeMinmax,
   HwParams.setTickTimeNear,
   HwParams.setTickTimeFirst,
   HwParams.setTickTimeLast,
   HwParams.getMinAlign,

   -- * Complex hardware parameter configuration
   setRateBufferTime,
   ) where

import qualified Sound.ALSA.PCM.Core.HwParams as HwParams
import Sound.ALSA.PCM.Core.HwParams (T, )
import Sound.ALSA.PCM.Core.Handle
          (SampleFreq, Time, Size, Interleaved, Noninterleaved, )

import Data.Word (Word, )


{-
Our approach cannot assert, that setChannels is called.
Is this necessary, at all?
If yes, setChannel accessors could return a secret value,
that must be passed to 'writen' in order to prove,
that setChannel was called.
-}

{- |
For non-interleaved access the number of channels must be set manually.
It can be chosen at runtime,
but the compiler cannot check whether the number of allocated channels
matches the one in 'readn' and 'writen'.

In interleaved access the number of channels is derived from the type
and must not be set manually.
The number is static
but the compiler checks consistency with 'readi' and 'writei'.
-}
setChannels :: Word -> T Noninterleaved y ()
setChannels = HwParams.setChannels

setChannelsMin :: Word -> T Noninterleaved y Word
setChannelsMin = HwParams.setChannelsMin

setChannelsMax :: Word -> T Noninterleaved y Word
setChannelsMax = HwParams.setChannelsMax

setChannelsMinmax :: Word -> Word -> T Noninterleaved y (Word, Word)
setChannelsMinmax = HwParams.setChannelsMinmax

setChannelsNear :: Word -> T Noninterleaved y Word
setChannelsNear = HwParams.setChannelsNear

setChannelsFirst :: Word -> T Noninterleaved y Word
setChannelsFirst = HwParams.setChannelsFirst

setChannelsLast :: Word -> T Noninterleaved y Word
setChannelsLast = HwParams.setChannelsLast



setRateBufferTime ::
      SampleFreq -- ^ sample frequency
   -> Time -- ^ buffer time
   -> Time -- ^ period time
   -> T Interleaved y (Size,Size)
      -- ^ (bufferSize,periodSize)
setRateBufferTime rate bufferTime periodTime = do
   HwParams.setRate rate EQ
   _ <- HwParams.setBufferTimeNear bufferTime EQ
   bufferSize <- HwParams.getBufferSize
   _ <- HwParams.setPeriodTimeNear periodTime EQ
   (periodSize,_) <- HwParams.getPeriodSize
   return (bufferSize,periodSize)
