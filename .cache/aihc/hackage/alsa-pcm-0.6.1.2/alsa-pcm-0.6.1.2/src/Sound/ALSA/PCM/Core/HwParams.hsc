{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.PCM.Core.HwParams where

import Sound.ALSA.PCM.Core.Handle (Handle, Time, Size, SampleFreq, )
import qualified Sound.ALSA.PCM.Core.Handle as H -- expose Handle constructor to FFI
import qualified Sound.ALSA.PCM.Core.Convert as Conv
import Sound.ALSA.PCM.Core.Convert (fromHaskell, )

import Sound.ALSA.Exception (checkResult, checkResult_, )

import Control.Applicative (Applicative(pure, (<*>)))

import Control.Exception (bracket, )

import qualified Foreign.Storable.Newtype as Store

import qualified Foreign.C.Types as C
import qualified Foreign.C.Error as E
import Foreign.Ptr (Ptr, )
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, )
import Foreign.Marshal.Alloc (alloca, )

import Data.Word (Word, )

import Prelude hiding (any, )

#include <alsa/asoundlib.h>
#include <Sound/ALSA/PCM/Core/Params.h>



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
      any h p
      x <- f p
      set h p
      return x
--    bracket_ (any h p) (set h p) (f p)


foreign import ccall safe "alsa/pcm.h snd_pcm_hw_params_malloc"
   malloc_ :: Ptr (Ptr Params) -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_hw_params_free"
   free :: Ptr Params -> IO ()

malloc :: IO (Ptr Params)
malloc =
   alloca $ \pp ->
   malloc_ pp >>=
   checkResult_ "HwParams.malloc" >>
   peek pp



foreign import ccall safe "alsa/pcm.h snd_pcm_hw_params"
   set_ :: Handle i y -> Ptr Params -> IO C.CInt

foreign import ccall safe "alsa/pcm.h snd_pcm_hw_params_any"
   any_ :: Handle i y -> Ptr Params -> IO C.CInt

set :: Handle i y -> Ptr Params -> IO ()
set h p =
   set_ h p >>= checkResult_ "HwParams.set"

any :: Handle i y -> Ptr Params -> IO ()
any h p =
   any_ h p >>= checkResult_ "HwParams.any"

-- #accessor checked, "any", pcm_params, NULL


newtype Bool_ = Bool_ {fromBool_ :: C.CInt}
   deriving (Eq, Ord)

instance Storable Bool_ where
   sizeOf = Store.sizeOf fromBool_
   alignment = Store.alignment fromBool_
   peek = Store.peek Bool_
   poke = Store.poke fromBool_

boolConv :: Conv.T Bool Bool_
boolConv =
   Conv.Cons
      (\b -> Bool_ $ fromIntegral $ fromEnum b)
      (Bool_ 0 /=)


newtype Direction = Direction {fromDirection :: C.CInt}
   deriving (Eq, Ord)

instance Storable Direction where
   sizeOf = Store.sizeOf fromDirection
   alignment = Store.alignment fromDirection
   peek = Store.peek Direction
   poke = Store.poke fromDirection

ordConv :: Conv.T Ordering Direction
ordConv =
   Conv.Cons
      (\o -> Direction $ fromIntegral $ fromEnum o - 1)
      (\d -> compare d $ Direction 0)


newtype Access = Access {fromAccess :: C.CInt}
   deriving (Eq, Ord)

instance Enum Access where
   toEnum n = Access $ fromIntegral n
   fromEnum (Access n) = fromIntegral n

instance Bounded Access where
   minBound = Access #{const SND_PCM_ACCESS_MMAP_INTERLEAVED}
   maxBound = Access #{const SND_PCM_ACCESS_LAST}

instance Storable Access where
   sizeOf = Store.sizeOf fromAccess
   alignment = Store.alignment fromAccess
   peek = Store.peek Access
   poke = Store.poke fromAccess

#{enum Access, Access,
   accessMmapInterleaved    = SND_PCM_ACCESS_MMAP_INTERLEAVED,
   accessMmapNoninterleaved = SND_PCM_ACCESS_MMAP_NONINTERLEAVED,
   accessMmapComplex        = SND_PCM_ACCESS_MMAP_COMPLEX,
   accessRwInterleaved      = SND_PCM_ACCESS_RW_INTERLEAVED,
   accessRwNoninterleaved   = SND_PCM_ACCESS_RW_NONINTERLEAVED }



newtype Format = Format {fromFormat :: C.CInt}
   deriving (Eq, Ord)

instance Enum Format where
   toEnum n = Format $ fromIntegral n
   fromEnum (Format n) = fromIntegral n

instance Bounded Format where
   minBound = formatUnknown
   maxBound = Format #{const SND_PCM_FORMAT_LAST}

instance Storable Format where
   sizeOf = Store.sizeOf fromFormat
   alignment = Store.alignment fromFormat
   peek = Store.peek Format
   poke = Store.poke fromFormat

#{enum Format, Format,
   formatUnknown          = SND_PCM_FORMAT_UNKNOWN,
   formatS8               = SND_PCM_FORMAT_S8,
   formatU8               = SND_PCM_FORMAT_U8,
   formatS16Le            = SND_PCM_FORMAT_S16_LE,
   formatS16Be            = SND_PCM_FORMAT_S16_BE,
   formatU16Le            = SND_PCM_FORMAT_U16_LE,
   formatU16Be            = SND_PCM_FORMAT_U16_BE,
   formatS24Le            = SND_PCM_FORMAT_S24_LE,
   formatS24Be            = SND_PCM_FORMAT_S24_BE,
   formatU24Le            = SND_PCM_FORMAT_U24_LE,
   formatU24Be            = SND_PCM_FORMAT_U24_BE,
   formatS32Le            = SND_PCM_FORMAT_S32_LE,
   formatS32Be            = SND_PCM_FORMAT_S32_BE,
   formatU32Le            = SND_PCM_FORMAT_U32_LE,
   formatU32Be            = SND_PCM_FORMAT_U32_BE,
   formatFloatLe          = SND_PCM_FORMAT_FLOAT_LE,
   formatFloatBe          = SND_PCM_FORMAT_FLOAT_BE,
   formatFloat64Le        = SND_PCM_FORMAT_FLOAT64_LE,
   formatFloat64Be        = SND_PCM_FORMAT_FLOAT64_BE,
   formatIec958SubframeLe = SND_PCM_FORMAT_IEC958_SUBFRAME_LE,
   formatIec958SubframeBe = SND_PCM_FORMAT_IEC958_SUBFRAME_BE,
   formatMuLaw            = SND_PCM_FORMAT_MU_LAW,
   formatALaw             = SND_PCM_FORMAT_A_LAW,
   formatImaAdpcm         = SND_PCM_FORMAT_IMA_ADPCM,
   formatMpeg             = SND_PCM_FORMAT_MPEG,
   formatGsm              = SND_PCM_FORMAT_GSM,
   formatSpecial          = SND_PCM_FORMAT_SPECIAL,
   formatS243le           = SND_PCM_FORMAT_S24_3LE,
   formatS243be           = SND_PCM_FORMAT_S24_3BE,
   formatU243le           = SND_PCM_FORMAT_U24_3LE,
   formatU243be           = SND_PCM_FORMAT_U24_3BE,
   formatS203le           = SND_PCM_FORMAT_S20_3LE,
   formatS203be           = SND_PCM_FORMAT_S20_3BE,
   formatU203le           = SND_PCM_FORMAT_U20_3LE,
   formatU203be           = SND_PCM_FORMAT_U20_3BE,
   formatS183le           = SND_PCM_FORMAT_S18_3LE,
   formatS183be           = SND_PCM_FORMAT_S18_3BE,
   formatU183le           = SND_PCM_FORMAT_U18_3LE,
   formatU183be           = SND_PCM_FORMAT_U18_3BE,
   formatS16              = SND_PCM_FORMAT_S16,
   formatU16              = SND_PCM_FORMAT_U16,
   formatS24              = SND_PCM_FORMAT_S24,
   formatU24              = SND_PCM_FORMAT_U24,
   formatS32              = SND_PCM_FORMAT_S32,
   formatU32              = SND_PCM_FORMAT_U32,
   formatFloat            = SND_PCM_FORMAT_FLOAT,
   formatFloat64          = SND_PCM_FORMAT_FLOAT64,
   formatIec958Subframe   = SND_PCM_FORMAT_IEC958_SUBFRAME }


newtype Subformat = Subformat {fromSubformat :: C.CUInt}
   deriving (Eq, Ord)

instance Storable Subformat where
   sizeOf = Store.sizeOf fromSubformat
   alignment = Store.alignment fromSubformat
   peek = Store.peek Subformat
   poke = Store.poke fromSubformat



#accessor boolresult, "can_mmap_sample_resolution", params_only, NULL
#accessor boolresult, "is_double", params_only, NULL
#accessor boolresult, "is_batch", params_only, NULL
#accessor boolresult, "is_block_transfer", params_only, NULL
#accessor boolresult, "can_overrange", params_only, NULL
#accessor boolresult, "can_pause", params_only, NULL
#accessor boolresult, "can_resume", params_only, NULL
#accessor boolresult, "is_half_duplex", params_only, NULL
#accessor boolresult, "is_joint_duplex", params_only, NULL
#accessor boolresult, "can_sync_start", params_only, NULL


#accessor checked, "get_rate_numden", params_only, \
               out(&uint_p, "rateNum"), out(&uint_p, "rateDen"), NULL
#accessor uintresult, "get_sbits", params_only, NULL
#accessor uintresult, "get_fifo_size", params_only, NULL

#accessor checked, "get_access", params_only, out(&access_p, "access"), NULL
#accessor errnoresult, "test_access", pcm_params, in(&access_p, "access"), NULL
#accessor checked, "set_access", pcm_params, in(&access_p, "access"), NULL
#accessor checked, "set_access_first", pcm_params, inout(&access_p, "access"), NULL
#accessor checked, "set_access_last", pcm_params, inout(&access_p, "access"), NULL
{- in order to use this, we would have to implement mask accessors, too
#accessor checked, "set_access_mask", pcm_params, inout(&access_mask, "mask"), NULL
#accessor checked, "get_access_mask", params_only, out(&access_mask, "mask"), NULL
-}

#accessor checked, "get_format", params_only, out(&format_p, "val"), NULL
#accessor errnoresult, "test_format", pcm_params, in(&format_p, "val"), NULL
#accessor checked, "set_format", pcm_params, in(&format_p, "val"), NULL
#accessor checked, "set_format_first", pcm_params, inout(&format_p, "format"), NULL
#accessor checked, "set_format_last", pcm_params, inout(&format_p, "format"), NULL
{-
#accessor checked, "set_format_mask", pcm_params, inout(&format_mask_p, "mask"), NULL
#accessor noresult, "get_format_mask", params_only, out(&format_mask_p, "mask"), NULL
-}

#accessor checked, "get_subformat", params_only, out(&subformat_p, "subformat"), NULL
#accessor errnoresult, "test_subformat", pcm_params, in(&subformat_p, "subformat"), NULL
#accessor checked, "set_subformat", pcm_params, in(&subformat_p, "subformat"), NULL
#accessor checked, "set_subformat_first", pcm_params, inout(&subformat_p, "subformat"), NULL
#accessor checked, "set_subformat_last", pcm_params, inout(&subformat_p, "subformat"), NULL
{-
#accessor checked, "set_subformat_mask", pcm_params, inout(&subformat_mask_p, "mask"), NULL
#accessor noresult, "get_subformat_mask", params_only, out(&subformat_mask_p, "mask"), NULL
-}

#accessor checked, "get_channels", params_only, out(&uint_p, "val"), NULL
#accessor checked, "get_channels_min", params_only, out(&uint_p, "val"), NULL
#accessor checked, "get_channels_max", params_only, out(&uint_p, "val"), NULL
#accessor errnoresult, "test_channels", pcm_params, in(&uint_p, "val"), NULL
#accessor checked, "set_channels", pcm_params, in(&uint_p, "val"), NULL
#accessor checked, "set_channels_min", pcm_params, inout(&uint_p, "val"), NULL
#accessor checked, "set_channels_max", pcm_params, inout(&uint_p, "val"), NULL
#accessor checked, "set_channels_minmax", pcm_params, inout(&uint_p, "vmin"), inout(&uint_p, "vmax"), NULL
#accessor checked, "set_channels_near", pcm_params, inout(&uint_p, "val"), NULL
#accessor checked, "set_channels_first", pcm_params, inout(&uint_p, "val"), NULL
#accessor checked, "set_channels_last", pcm_params, inout(&uint_p, "val"), NULL

#accessor checked, "get_rate", params_only, out(&rate_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_rate_min", params_only, out(&rate_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_rate_max", params_only, out(&rate_p, "val"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_rate", pcm_params, in(&rate_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_rate", pcm_params, in(&rate_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_rate_min", pcm_params, inout(&rate_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_rate_max", pcm_params, inout(&rate_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_rate_minmax", pcm_params, inout(&rate_p, "vmin"), inout(&direction_p, "mindir"), inout(&rate_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_rate_near", pcm_params, inout(&rate_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_rate_first", pcm_params, inout(&rate_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_rate_last", pcm_params, inout(&rate_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_rate_resample", pcm_params, in(&bool_p, "val"), NULL
#accessor checked, "get_rate_resample", pcm_params, out(&bool_p, "val"), NULL
#accessor checked, "set_export_buffer", pcm_params, in(&bool_p, "val"), NULL
#accessor checked, "get_export_buffer", pcm_params, out(&bool_p, "val"), NULL

#accessor checked, "get_period_time", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_period_time_min", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_period_time_max", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_period_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_period_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_period_time_min", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_time_max", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_time_minmax", pcm_params, inout(&time_p, "vmin"), inout(&direction_p, "mindir"), inout(&time_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_period_time_near", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_time_first", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_time_last", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL

#accessor checked, "get_period_size", params_only, out(&uframes_p, "frames"), out(&direction_p, "dir"), NULL
#accessor checked, "get_period_size_min", params_only, out(&uframes_p, "frames"), out(&direction_p, "dir"), NULL
#accessor checked, "get_period_size_max", params_only, out(&uframes_p, "frames"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_period_size", pcm_params, in(&uframes_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_period_size", pcm_params, in(&uframes_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_min", pcm_params, inout(&uframes_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_max", pcm_params, inout(&uframes_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_minmax", pcm_params, inout(&uframes_p, "vmin"), inout(&direction_p, "mindir"), inout(&uframes_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_period_size_near", pcm_params, inout(&uframes_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_first", pcm_params, inout(&uframes_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_last", pcm_params, inout(&uframes_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_period_size_integer", pcm_params, NULL

#accessor checked, "get_periods", params_only, out(&uint_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_periods_min", params_only, out(&uint_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_periods_max", params_only, out(&uint_p, "val"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_periods", pcm_params, in(&uint_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_periods", pcm_params, in(&uint_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_periods_min", pcm_params, inout(&uint_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_periods_max", pcm_params, inout(&uint_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_periods_minmax", pcm_params, inout(&uint_p, "vmin"), inout(&direction_p, "mindir"), inout(&uint_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_periods_near", pcm_params, inout(&uint_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_periods_first", pcm_params, inout(&uint_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_periods_last", pcm_params, inout(&uint_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_periods_integer", pcm_params, NULL

#accessor checked, "get_buffer_time", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_buffer_time_min", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_buffer_time_max", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_buffer_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time_min", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time_max", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time_minmax", pcm_params, inout(&time_p, "vmin"), inout(&direction_p, "mindir"), inout(&time_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_buffer_time_near", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time_first", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_buffer_time_last", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL

#accessor checked, "get_buffer_size", params_only, out(&uframes_p, "val"), NULL
#accessor checked, "get_buffer_size_min", params_only, out(&uframes_p, "val"), NULL
#accessor checked, "get_buffer_size_max", params_only, out(&uframes_p, "val"), NULL
#accessor errnoresult, "test_buffer_size", pcm_params, in(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size", pcm_params, in(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size_min", pcm_params, inout(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size_max", pcm_params, inout(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size_minmax", pcm_params, inout(&uframes_p, "vmin"), inout(&uframes_p, "vmax"), NULL
#accessor checked, "set_buffer_size_near", pcm_params, inout(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size_first", pcm_params, inout(&uframes_p, "val"), NULL
#accessor checked, "set_buffer_size_last", pcm_params, inout(&uframes_p, "val"), NULL

#accessor checked, "get_tick_time", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_tick_time_min", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor checked, "get_tick_time_max", params_only, out(&time_p, "val"), out(&direction_p, "dir"), NULL
#accessor errnoresult, "test_tick_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time", pcm_params, in(&time_p, "val"), in(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time_min", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time_max", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time_minmax", pcm_params, inout(&time_p, "vmin"), inout(&direction_p, "mindir"), inout(&time_p, "vmax"), inout(&direction_p, "maxdir"), NULL
#accessor checked, "set_tick_time_near", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time_first", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL
#accessor checked, "set_tick_time_last", pcm_params, inout(&time_p, "val"), inout(&direction_p, "dir"), NULL

#accessor checked, "get_min_align", params_only, out(&uframes_p, "val"), NULL
