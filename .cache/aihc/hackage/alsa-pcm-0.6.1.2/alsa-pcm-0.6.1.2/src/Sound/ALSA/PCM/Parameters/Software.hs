module Sound.ALSA.PCM.Parameters.Software (
   -- * Types
   SwParams.T, Size,
   TimestampMode, timestampNone, timestampMmap,

   -- * Primitive accessors to software parameters
   getAvailMin,         setAvailMin,
   getSilenceSize,      setSilenceSize,
   getSilenceThreshold, setSilenceThreshold,
   getSleepMin,         setSleepMin,
   getStartThreshold,   setStartThreshold,
   getStopThreshold,    setStopThreshold,
   getTimestampMode,    setTimestampMode,
   getXferAlign,        setXferAlign,

   -- * Complex software parameter configuration
   setBufferSize,
   ) where

import Sound.ALSA.PCM.Core.SwParams as SwParams
import Sound.ALSA.PCM.Core.Handle (Size, )


setBufferSize ::
      Size -- ^ buffer size
   -> Size -- ^ period size
   -> SwParams.T i y ()
setBufferSize _bufferSize periodSize = do
   -- let startThreshold =
   --        (setBufferSize `div` periodSize) * periodSize
   -- setStartThreshold startThreshold
   setStartThreshold 0
   setAvailMin $ fromIntegral periodSize
   setXferAlign 1
   -- pad buffer with silence when needed
   -- setSilenceSize periodSize
   -- setSilenceThreshold periodSize
