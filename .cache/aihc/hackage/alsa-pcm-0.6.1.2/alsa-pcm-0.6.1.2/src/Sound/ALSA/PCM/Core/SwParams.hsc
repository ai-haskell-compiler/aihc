{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.PCM.Core.SwParams where

import Sound.ALSA.PCM.Core.Handle (Handle, Size, )
import qualified Sound.ALSA.PCM.Core.Handle as H -- expose Handle constructor to FFI
import qualified Sound.ALSA.PCM.Core.Convert as Conv

import Sound.ALSA.Exception (checkResult_, )

import Control.Applicative (Applicative(pure, (<*>)))

import Control.Exception (bracket, )

import qualified Foreign.Storable.Newtype as Store

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, )
import Foreign.Marshal.Alloc (alloca, )

import Data.Word (Word, )


#include <alsa/asoundlib.h>

newtype T i y a = Cons (H.Handle i y -> Ptr Params -> IO a)

data Params = Params


{-
T is a Reader monad.
-}
instance Functor (T i y) where
   fmap f (Cons act) = Cons $ \h p -> fmap f $ act h p

instance Applicative (T i y) where
   pure a = Cons $ \ _h _p -> pure a
   Cons f <*> Cons x = Cons $ \h p -> f h p <*> x h p

instance Monad (T i y) where
   return = pure
   Cons x >>= k =
      Cons $ \h p -> x h p >>= \a -> case k a of Cons y -> y h p



withIO :: Handle i y -> (Ptr Params -> IO a) -> IO a
withIO h f =
   bracket malloc free $ \p -> do
      current h p
      x <- f p
      set h p
      return x
--   bracket_ (current h p) (set h p) (f p)


foreign import ccall safe "alsa/pcm.h snd_pcm_sw_params_malloc"
   malloc_ :: Ptr (Ptr Params) -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_sw_params_free"
   free :: Ptr Params -> IO ()

malloc :: IO (Ptr Params)
malloc =
   alloca $ \pp ->
   malloc_ pp >>=
   checkResult_ "SwParams.malloc" >>
   peek pp



foreign import ccall safe "alsa/pcm.h snd_pcm_sw_params"
   set_ :: Handle i y -> Ptr Params -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_sw_params_current"
   current_ :: Handle i y -> Ptr Params -> IO C.CInt

set :: Handle i y -> Ptr Params -> IO ()
set h p =
   set_ h p >>= checkResult_ "SwParams.set"

current :: Handle i y -> Ptr Params -> IO ()
current h p =
   current_ h p >>= checkResult_ "SwParams.current"



newtype TimestampMode = TimestampMode {fromTimestampMode :: C.CInt}
   deriving (Eq, Ord)

instance Enum TimestampMode where
   toEnum n = TimestampMode $ fromIntegral n
   fromEnum (TimestampMode n) = fromIntegral n

instance Storable TimestampMode where
   sizeOf = Store.sizeOf fromTimestampMode
   alignment = Store.alignment fromTimestampMode
   peek = Store.peek TimestampMode
   poke = Store.poke fromTimestampMode

#{enum TimestampMode, TimestampMode,
   timestampNone = SND_PCM_TSTAMP_NONE,
   timestampMmap = SND_PCM_TSTAMP_MMAP}



#{let accessor hsName, cName, conv, hsType, cType =
"foreign import ccall safe \"alsa/pcm.h snd_pcm_sw_params_set_"cName"\"\n"
"   set"hsName"_ :: Handle i y -> Ptr Params -> "cType" -> IO C.CInt\n"
"\n"
"foreign import ccall safe \"alsa/pcm.h snd_pcm_sw_params_get_"cName"\"\n"
"   get"hsName"_ :: Ptr Params -> Ptr "cType" -> IO C.CInt\n"
"\n"
"set"hsName" :: "hsType" -> T i y ()\n"
"set"hsName" x =\n"
"   Cons $ \\h p ->\n"
"   set"hsName"_ h p (Conv.fromHaskell "conv" x) >>=\n"
"   checkResult_ \"SwParams.set"hsName"\"\n"
"\n"
"get"hsName" :: T i y "hsType"\n"
"get"hsName" =\n"
"   Cons $ \\_ p ->\n"
"   alloca $ \\ptr ->\n"
"   get"hsName"_ p ptr >>=\n"
"   checkResult_ \"SwParams.get"hsName"\" >>\n"
"   Conv.peek "conv" ptr\n"
}


#accessor "TimestampMode", "tstamp_mode", "Conv.id", "TimestampMode", "TimestampMode"
#accessor "SleepMin", "sleep_min", "Conv.int", "Word", "C.CUInt"
#accessor "AvailMin", "avail_min", "Conv.int", "Size", "C.CULong"
#accessor "XferAlign", "xfer_align", "Conv.int", "Size", "C.CULong"
#accessor "StartThreshold", "start_threshold", "Conv.int", "Size", "C.CULong"
#accessor "StopThreshold", "stop_threshold", "Conv.int", "Size", "C.CULong"
#accessor "SilenceThreshold", "silence_threshold", "Conv.int", "Size", "C.CULong"
#accessor "SilenceSize", "silence_size", "Conv.int", "Size", "C.CULong"
