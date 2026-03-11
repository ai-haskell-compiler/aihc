{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module Sound.VAD.WebRTC
  ( VAD(), IOVAD
  , create
  , Aggressiveness, setMode
  , validRateAndFrameLength
  , process
  , VADException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Functor
import Data.Int
import Data.Typeable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C

newtype VADException = VADException String
  deriving (Show, Typeable)

instance Exception VADException

orDie :: IO CInt -> String -> IO ()
orDie m err = do
  res <- m
  unless (res == 0) $ throwIO $ VADException err

data VadInst

-- | A VAD instance with a state token (like `MVector`).
newtype VAD s = VAD (ForeignPtr VadInst)

-- | A VAD instance for use in `IO`.
type IOVAD = VAD RealWorld

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Create" _WebRtcVad_Create :: Ptr (Ptr VadInst) -> IO CInt
foreign import ccall unsafe "webrtc_vad.h & WebRtcVad_Free" _WebRtcVad_Free :: FunPtr (Ptr VadInst -> IO ())
foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Init" _WebRtcVad_Init :: Ptr VadInst -> IO CInt

-- | Create and initialize a VAD instance.
create :: PrimMonad m => m (VAD (PrimState m))
create = unsafePrimToPrim $ alloca $ \ptr -> do
  _WebRtcVad_Create ptr `orDie` "Could not create VAD instance."
  vad <- peek ptr
  _WebRtcVad_Init vad `orDie` "Could not initialize VAD instance - NULL pointer or Default mode could not be set."
  VAD <$> newForeignPtr _WebRtcVad_Free vad

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_set_mode" _WebRtcVad_set_mode :: Ptr VadInst -> CInt -> IO CInt

-- | Aggressiveness mode (0, 1, 2, or 3).
type Aggressiveness = Int

withVAD :: PrimMonad m => (Ptr VadInst -> IO a) -> VAD (PrimState m) -> m a
withVAD f (VAD finst) = unsafePrimToPrim $ withForeignPtr finst f

-- | @setMode mode vad@
--
-- Sets the VAD operating mode. A more aggressive (higher mode) VAD is more
-- restrictive in reporting speech. Put in other words the probability of being
-- speech when the VAD returns 1 is increased with increasing mode. As a
-- consequence also the missed detection rate goes up.
setMode :: PrimMonad m => Aggressiveness -> VAD (PrimState m) -> m ()
setMode aggr = withVAD $ \vad ->
  _WebRtcVad_set_mode vad (fromIntegral aggr) `orDie` "NULL pointer, mode could not be set or the VAD instance has not been initialized"

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Process" _WebRtcVad_Process :: Ptr VadInst -> CInt -> Ptr Int16 -> CInt -> IO CInt

-- | @process sampleRate audioFrame vad@
--
-- Calculates a VAD decision for the @audioFrame@. For valid sampling rates
-- frame lengths, see the description of `validRatesAndFrameLengths`.
--
-- @sampleRate@: Sampling frequency (Hz): 8000, 16000, or 32000
--
-- @audioFrame@: Audio frame buffer (mono signed 16-bit).
process :: PrimMonad m => Int -> V.Vector Int16 -> VAD (PrimState m) -> m Bool
process sampleRate buffer = withVAD $ \vad -> do
  res <- V.unsafeWith buffer $ \ptr ->
    _WebRtcVad_Process vad (fromIntegral sampleRate) ptr (fromIntegral $ V.length buffer)
  case res of
    1 -> return True
    0 -> return False
    _ -> throwIO $ VADException "Error while processing buffer."

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_ValidRateAndFrameLength" _WebRtcVad_ValidRateAndFrameLength :: CInt -> CInt -> CInt

-- | @validRateAndFrameLength rate frameLength@
--
-- Checks for valid combinations of @rate@ and @frameLength@. We support 10,
-- 20 and 30 ms frames and the rates 8000, 16000 and 32000 Hz.
--
-- @rate@: Sampling frequency (Hz).
--
-- @frameLength@: Speech frame buffer length in number of samples.
validRateAndFrameLength :: Int -> Int -> Bool
validRateAndFrameLength rate frameLength =
  case _WebRtcVad_ValidRateAndFrameLength (fromIntegral rate) (fromIntegral frameLength) of
    0  -> True
    -1 -> False
    _  -> throw $ VADException "Unknown result value."