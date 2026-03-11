{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.PCM.Core.Handle where

import Sound.ALSA.Exception (checkResult, checkResult_, )

import qualified Foreign.Storable.Newtype as Store

import qualified Foreign.C.Types as C
import Foreign.C.String (CString, withCString, )
import Foreign.Ptr (Ptr, minusPtr, )
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, )
import Foreign.Marshal.Array (advancePtr, )
import Foreign.Marshal.Alloc (alloca, )

import Data.Bits ((.|.), )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>), )

import Prelude hiding (any, )


#include <alsa/asoundlib.h>


data Struct i y = Struct

newtype Handle i y = Handle {dePcm :: Ptr (Struct i y)}

data Interleaved = Interleaved
data Noninterleaved = Noninterleaved


instance Storable (Handle i y) where
   sizeOf = Store.sizeOf dePcm
   alignment = Store.alignment dePcm
   peek = Store.peek Handle
   poke = Store.poke dePcm


type SampleFreq = Int
type Time = Int
type Size = Int


data Stream =
     StreamPlayback
   | StreamCapture
   deriving (Eq,Show,Bounded)

instance Enum Stream where
   fromEnum StreamPlayback = #{const SND_PCM_STREAM_PLAYBACK}
   fromEnum StreamCapture  = #{const SND_PCM_STREAM_CAPTURE}

   toEnum #{const SND_PCM_STREAM_PLAYBACK} = StreamPlayback
   toEnum #{const SND_PCM_STREAM_CAPTURE}  = StreamCapture
   toEnum unmatched = error ("Stream.toEnum: Cannot match " ++ show unmatched)


newtype Mode = Mode C.CInt

#{enum Mode, Mode,
   nonBlock = SND_PCM_NONBLOCK,
   async = SND_PCM_ASYNC }

instance Semigroup Mode where
   Mode a <> Mode b = Mode (a .|. b)

instance Monoid Mode where
   mempty = Mode 0
   mappend = (<>)

modes :: [Mode] -> Mode
modes = mconcat


foreign import ccall safe "alsa/pcm.h snd_pcm_open"
   open_ :: Ptr (Handle i y) -> CString -> C.CInt -> Mode -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_close"
   close_ :: Handle i y -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_prepare"
   prepare_ :: Handle i y -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_start"
   start_ :: Handle i y -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_drop"
   drop_ :: Handle i y -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_drain"
   drain_ :: Handle i y -> IO C.CInt


open :: String -> Stream -> Mode -> IO (Handle i y)
open device dir mode =
   alloca $ \ptr ->
   withCString device $ \deviceCStr ->
   open_ ptr deviceCStr (fromIntegral $ fromEnum dir) mode >>=
   checkResult_ "PCM.open" >>
   peek ptr

close :: Handle i y -> IO ()
close h =
   close_ h >>= checkResult_ "PCM.close"

prepare :: Handle i y -> IO ()
prepare h =
   prepare_ h >>= checkResult_ "PCM.prepare"

start :: Handle i y -> IO ()
start h =
   start_ h >>= checkResult_ "PCM.start"

drop :: Handle i y -> IO ()
drop h =
   drop_ h >>= checkResult_ "PCM.drop"

drain :: Handle i y -> IO ()
drain h =
   drain_ h >>= checkResult_ "PCM.drain"



foreign import ccall safe "alsa/pcm.h snd_pcm_readi"
   readi_ :: Handle Interleaved y -> Ptr y -> C.CULong -> IO C.CLong

foreign import ccall safe "alsa/pcm.h snd_pcm_writei"
   writei_ :: Handle Interleaved y -> Ptr y -> C.CULong -> IO C.CLong

foreign import ccall safe "alsa/pcm.h snd_pcm_readn"
   readn_ :: Handle Noninterleaved y -> Ptr (Ptr y) -> C.CULong -> IO C.CLong

foreign import ccall safe "alsa/pcm.h snd_pcm_writen"
   writen_ :: Handle Noninterleaved y -> Ptr (Ptr y) -> C.CULong -> IO C.CLong


readi :: Handle Interleaved y -> Ptr y -> Size -> IO Size
readi h buf n =
   fmap fromIntegral $
   checkResult "PCM.readi" =<< readi_ h buf (fromIntegral n)

writei :: Handle Interleaved y -> Ptr y -> Size -> IO Size
writei h buf n =
   fmap fromIntegral $
   checkResult "PCM.writei" =<< writei_ h buf (fromIntegral n)

readn :: Handle Noninterleaved y -> Ptr (Ptr y) -> Size -> IO Size
readn h buf n =
   fmap fromIntegral $
   checkResult "PCM.readn" =<< readn_ h buf (fromIntegral n)

writen :: Handle Noninterleaved y -> Ptr (Ptr y) -> Size -> IO Size
writen h buf n =
   fmap fromIntegral $
   checkResult "PCM.writen" =<< writen_ h buf (fromIntegral n)


{-# INLINE arraySize #-}
arraySize :: Storable y => Ptr y -> Int -> Int
arraySize p n = advancePtr p n `minusPtr` p
