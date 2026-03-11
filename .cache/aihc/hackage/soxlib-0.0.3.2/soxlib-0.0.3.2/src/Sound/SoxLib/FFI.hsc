{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SoxLib.FFI where

#include <sox.h>
#include <Sound/SoxLib/Template.h>

import qualified Foreign.C.String as CStr
import qualified Foreign.C.Types as C
import qualified Foreign.C.Error as E
import Foreign.Storable (Storable, sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff, )
import Foreign.Ptr (FunPtr, Ptr, nullPtr, plusPtr, )

import Control.Applicative (pure, (<*>), )

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )

-- we must import it unqualified, because Hsc calls it that way
import Data.Word
import Data.Int

import qualified Prelude as P
import Prelude hiding (Bool, init, length, )



newtype IOType = IOType #{type lsx_io_type}
   deriving (Storable)

#{enum IOType, IOType
   , ioFile = lsx_io_file
   , ioPipe = lsx_io_pipe
   , ioURL = lsx_io_url
   }

instance Show IOType where
   show (IOType #{const lsx_io_file}) = "ioFile"
   show (IOType #{const lsx_io_pipe}) = "ioPipe"
   show (IOType #{const lsx_io_url }) = "ioURL"
   show (IOType n) = error $ "SoxLib.IOType.show: invalid number " ++ show n


type FormatPtr mode = Ptr (Format mode)

data Format mode =
   Format {
      filename :: FilePath,
      signalInfo :: SignalInfo,
      encodingInfo :: EncodingInfo,
      filetype :: FileType,
      -- oob :: OOB,
      seekable :: P.Bool,
      olength :: Int,
      clips :: Int,
      soxErrno :: E.Errno,
      soxErrStr :: String,
      fp :: Ptr C.CFile,
      ioType :: IOType,
      tellOff :: C.CLong,
      dataStart :: C.CLong,
      -- handler :: FormatHandler,
      priv :: Ptr ()
   }

instance Mode mode => Storable (Format mode) where
   sizeOf _    = #{size sox_format_t}
   alignment _ = #{alignment sox_format_t}
   peek p =
      pure Format
         <*> (peekEmptyCString =<< #{peek sox_format_t, filename} p)
         <*> #{peek sox_format_t, signal} p
         <*> #{peek sox_format_t, encoding} p
         <*> (fmap FileType . peekEmptyCString =<<
              #{peek sox_format_t, filetype} p)
         -- <*> #{peek sox_format_t, oob} p
         <*> peekBool (#{ptr sox_format_t, seekable} p)
         <*> (#{peek_int sox_format_t, olength} p)
         <*> (#{peek_int sox_format_t, clips} p)
         <*> fmap E.Errno (#{peek sox_format_t, sox_errno} p)
         <*> (CStr.peekCStringLen (#{ptr sox_format_t, sox_errstr} p, 256))
         <*> #{peek sox_format_t, fp} p
         <*> #{peek sox_format_t, io_type} p
         <*> #{peek sox_format_t, tell_off} p
         <*> #{peek sox_format_t, data_start} p
         -- <*> #{peek sox_format_t, handler} p
         <*> #{peek sox_format_t, priv} p
   poke = error "SoxLib.Format.poke cannot be implemented because it requires temporary memory"


peekEmptyCString :: CStr.CString -> IO String
peekEmptyCString ptr =
   if ptr == nullPtr
     then return ""
     else CStr.peekCString ptr


class Mode mode where
   getModeChar :: Format mode -> C.CChar

data ReadMode  = ReadMode
data WriteMode = WriteMode

instance Mode ReadMode where
   getModeChar _ = CStr.castCharToCChar 'r'

instance Mode WriteMode where
   getModeChar _ = CStr.castCharToCChar 'w'


type Rate = Double

ignoreLength :: Int
ignoreLength = -1

data SignalInfo =
   SignalInfo {
      rate :: Maybe Rate,
      channels :: Maybe Int,
      precision :: Maybe Int,
      length :: Maybe Int,
      mult :: Maybe Double
   }
   deriving (Show)

defaultSignalInfo :: SignalInfo
defaultSignalInfo =
   SignalInfo Nothing Nothing Nothing Nothing Nothing

readSINumber :: (Num a, Eq a) => a -> Maybe a
readSINumber n =
   toMaybe (n /= #{const SOX_UNSPEC}) n

writeSINumber :: (Num a, Eq a) => Maybe a -> a
writeSINumber =
   fromMaybe (#{const SOX_UNSPEC})


instance Storable SignalInfo where
   sizeOf _    = #{size sox_signalinfo_t}
   alignment _ = #{alignment sox_signalinfo_t}
   peek p =
      pure SignalInfo
         <*> (fmap readSINumber $ peekDouble (#{ptr sox_signalinfo_t, rate} p))
         <*> (fmap readSINumber $ #{peek_int sox_signalinfo_t, channels} p)
         <*> (fmap readSINumber $ #{peek_int sox_signalinfo_t, precision} p)
         <*> (fmap readSINumber $ #{peek_int sox_signalinfo_t, length} p)
         <*> (do pd <- #{peek sox_signalinfo_t, mult} p
                 if pd == nullPtr
                   then return Nothing
                   else fmap Just $ peekDouble pd)
   poke p v =
      pokeDouble (#{ptr sox_signalinfo_t, rate} p) (writeSINumber $ rate v) >>
      #{poke sox_signalinfo_t, channels } p (writeSINumber $ channels  v) >>
      #{poke sox_signalinfo_t, precision} p (writeSINumber $ precision v) >>
      #{poke sox_signalinfo_t, length   } p (writeSINumber $ length    v) >>
      case mult v of
         Nothing -> #{poke sox_signalinfo_t, mult} p nullPtr
         Just _ ->
            error "SoxLib.SignalInfo.poke: Just-mult cannot simply be poked because it requires temporary memory"


newtype Option = Option #{type sox_option_t}
   deriving (Storable)

#if SOX_LIB_VERSION_CODE < SOX_LIB_VERSION(14, 4, 0)

#define OPTION_NO      SOX_OPTION_NO
#define OPTION_YES     SOX_OPTION_YES
#define OPTION_DEFAULT SOX_OPTION_DEFAULT

#else

#define OPTION_NO      sox_option_no
#define OPTION_YES     sox_option_yes
#define OPTION_DEFAULT sox_option_default

#endif

#{enum Option, Option
   , optionNo = OPTION_NO
   , optionYes = OPTION_YES
   , optionDefault = OPTION_DEFAULT
 }

instance Show Option where
   show (Option #{const OPTION_NO     }) = "optionNo"
   show (Option #{const OPTION_YES    }) = "optionYes"
   show (Option #{const OPTION_DEFAULT}) = "optionDefault"
   show (Option n) = error $ "SoxLib.Option.show: invalid number " ++ show n


data EncodingInfo =
   EncodingInfo {
      encoding :: Encoding,
      bitsPerSample :: Int,
      compression :: Double,

      reverseBytes, reverseNibbles, reverseBits :: Option,
      oppositeEndian :: P.Bool
   }
   deriving (Show)

instance Storable EncodingInfo where
   sizeOf _    = #{size sox_encodinginfo_t}
   alignment _ = #{alignment sox_encodinginfo_t}
   peek p =
      pure EncodingInfo
         <*> (#{peek sox_encodinginfo_t, encoding} p)
         <*> (#{peek_int sox_encodinginfo_t, bits_per_sample} p)
         <*> (peekDouble $ #{ptr sox_encodinginfo_t, compression} p)
         <*> (#{peek sox_encodinginfo_t, reverse_bytes} p)
         <*> (#{peek sox_encodinginfo_t, reverse_nibbles} p)
         <*> (#{peek sox_encodinginfo_t, reverse_bits} p)
         <*> (peekBool $ #{ptr sox_encodinginfo_t, opposite_endian} p)
   poke p v =
      #{poke     sox_encodinginfo_t, encoding       } p (encoding      v) >>
      #{poke_int sox_encodinginfo_t, bits_per_sample} p (bitsPerSample v) >>
      pokeDouble (#{ptr sox_encodinginfo_t, compression} p) (compression v) >>
      #{poke     sox_encodinginfo_t, reverse_bytes  } p (reverseBytes   v) >>
      #{poke     sox_encodinginfo_t, reverse_nibbles} p (reverseNibbles v) >>
      #{poke     sox_encodinginfo_t, reverse_bits   } p (reverseBits    v) >>
      pokeBool (#{ptr sox_encodinginfo_t, opposite_endian} p) (oppositeEndian v)


newtype Bool = Bool #{inttype sox_bool}
   deriving (Show, Eq, Storable)

#{enum Bool, Bool
   , false = sox_false
   , true = sox_true
   }

packBool :: P.Bool -> Bool
packBool False = false
packBool True = true

unpackBool :: Bool -> P.Bool
unpackBool x = x/=false


peekBool :: Ptr Bool -> IO P.Bool
peekBool p = fmap unpackBool $ peek p

pokeBool :: Ptr Bool -> P.Bool -> IO ()
pokeBool p b = poke p $ packBool b


peekDouble :: Ptr C.CDouble -> IO Double
peekDouble p = fmap realToFrac $ peek p

pokeDouble :: Ptr C.CDouble -> Double -> IO ()
pokeDouble p x = poke p $ realToFrac x


newtype Encoding = Encoding #{inttype sox_encoding_t}
   deriving (Storable)

instance Bounded Encoding where
   minBound = Encoding #{const SOX_ENCODING_UNKNOWN}
   maxBound = Encoding $ pred #{const SOX_ENCODINGS}

#{enum Encoding, Encoding
   , encodingUnknown    = SOX_ENCODING_UNKNOWN
   , encodingSign2      = SOX_ENCODING_SIGN2
   , encodingUnsigned   = SOX_ENCODING_UNSIGNED
   , encodingFloat      = SOX_ENCODING_FLOAT
   , encodingFloatText  = SOX_ENCODING_FLOAT_TEXT
   , encodingFlac       = SOX_ENCODING_FLAC
   , encodingHcom       = SOX_ENCODING_HCOM
   , encodingWavpack    = SOX_ENCODING_WAVPACK
   , encodingWavpackf   = SOX_ENCODING_WAVPACKF
   , encodingUlaw       = SOX_ENCODING_ULAW
   , encodingAlaw       = SOX_ENCODING_ALAW
   , encodingG721       = SOX_ENCODING_G721
   , encodingG723       = SOX_ENCODING_G723
   , encodingClADPCM    = SOX_ENCODING_CL_ADPCM
   , encodingClADPCM16  = SOX_ENCODING_CL_ADPCM16
   , encodingMsADPCM    = SOX_ENCODING_MS_ADPCM
   , encodingImaADPCM   = SOX_ENCODING_IMA_ADPCM
   , encodingOkiADPCM   = SOX_ENCODING_OKI_ADPCM
   , encodingDPCM       = SOX_ENCODING_DPCM
   , encodingDWVW       = SOX_ENCODING_DWVW
   , encodingDWVWN      = SOX_ENCODING_DWVWN
   , encodingGSM        = SOX_ENCODING_GSM
   , encodingMP3        = SOX_ENCODING_MP3
   , encodingVorbis     = SOX_ENCODING_VORBIS
   , encodingAmrWB      = SOX_ENCODING_AMR_WB
   , encodingAmrNB      = SOX_ENCODING_AMR_NB
   , encodingCVSD       = SOX_ENCODING_CVSD
   , encodingLPC10      = SOX_ENCODING_LPC10
}

instance Show Encoding where
   show (Encoding #{const SOX_ENCODING_UNKNOWN   }) = "encodingUnknown"
   show (Encoding #{const SOX_ENCODING_SIGN2     }) = "encodingSign2"
   show (Encoding #{const SOX_ENCODING_UNSIGNED  }) = "encodingUnsigned"
   show (Encoding #{const SOX_ENCODING_FLOAT     }) = "encodingFloat"
   show (Encoding #{const SOX_ENCODING_FLOAT_TEXT}) = "encodingFloatText"
   show (Encoding #{const SOX_ENCODING_FLAC      }) = "encodingFlac"
   show (Encoding #{const SOX_ENCODING_HCOM      }) = "encodingHcom"
   show (Encoding #{const SOX_ENCODING_WAVPACK   }) = "encodingWavpack"
   show (Encoding #{const SOX_ENCODING_WAVPACKF  }) = "encodingWavpackf"
   show (Encoding #{const SOX_ENCODING_ULAW      }) = "encodingUlaw"
   show (Encoding #{const SOX_ENCODING_ALAW      }) = "encodingAlaw"
   show (Encoding #{const SOX_ENCODING_G721      }) = "encodingG721"
   show (Encoding #{const SOX_ENCODING_G723      }) = "encodingG723"
   show (Encoding #{const SOX_ENCODING_CL_ADPCM  }) = "encodingClADPCM"
   show (Encoding #{const SOX_ENCODING_CL_ADPCM16}) = "encodingClADPCM16"
   show (Encoding #{const SOX_ENCODING_MS_ADPCM  }) = "encodingMsADPCM"
   show (Encoding #{const SOX_ENCODING_IMA_ADPCM }) = "encodingImaADPCM"
   show (Encoding #{const SOX_ENCODING_OKI_ADPCM }) = "encodingOkiADPCM"
   show (Encoding #{const SOX_ENCODING_DPCM      }) = "encodingDPCM"
   show (Encoding #{const SOX_ENCODING_DWVW      }) = "encodingDWVW"
   show (Encoding #{const SOX_ENCODING_DWVWN     }) = "encodingDWVWN"
   show (Encoding #{const SOX_ENCODING_GSM       }) = "encodingGSM"
   show (Encoding #{const SOX_ENCODING_MP3       }) = "encodingMP3"
   show (Encoding #{const SOX_ENCODING_VORBIS    }) = "encodingVorbis"
   show (Encoding #{const SOX_ENCODING_AMR_WB    }) = "encodingAmrWB"
   show (Encoding #{const SOX_ENCODING_AMR_NB    }) = "encodingAmrNB"
   show (Encoding #{const SOX_ENCODING_CVSD      }) = "encodingCVSD"
   show (Encoding #{const SOX_ENCODING_LPC10     }) = "encodingLPC10"
   show (Encoding n) = "(Encoding " ++ show n ++ ")"



newtype FileType = FileType {unFileType :: String}

instance Show FileType where
   showsPrec prec (FileType str) =
      showParen (prec>=10) $ showString "FileType " . showString str

type CFileType = CStr.CString
type Sample = #{type sox_sample_t}
type OOB = ()
-- type FormatHandler = ()
type Whence = C.CInt


foreign import ccall unsafe "sox.h sox_init"
   init :: IO C.CInt

foreign import ccall unsafe "sox.h sox_quit"
   quit :: IO C.CInt

foreign import ccall unsafe "sox.h sox_format_init"
   formatInit :: IO C.CInt

foreign import ccall unsafe "sox.h sox_format_quit"
   formatQuit :: IO ()

foreign import ccall unsafe "sox.h sox_open_read"
   openRead :: CStr.CString -> Ptr SignalInfo -> Ptr EncodingInfo -> CFileType -> IO (FormatPtr ReadMode)

foreign import ccall safe "sox.h sox_open_mem_read"
   openMemRead :: Ptr Word8 -> C.CSize -> Ptr SignalInfo -> Ptr EncodingInfo -> CStr.CString -> IO (FormatPtr ReadMode)

foreign import ccall safe "sox.h sox_open_write"
   openWrite :: CStr.CString -> Ptr SignalInfo -> Ptr EncodingInfo -> CFileType -> Ptr OOB -> FunPtr (CStr.CString -> IO Bool) -> IO (FormatPtr WriteMode)

foreign import ccall safe "sox.h sox_open_mem_write"
   openMemWrite :: Ptr Word8 -> C.CSize -> Ptr SignalInfo -> Ptr EncodingInfo -> CFileType -> Ptr OOB -> IO (FormatPtr WriteMode)

foreign import ccall safe "sox.h sox_open_memstream_write"
   openMemstreamWrite :: Ptr (Ptr Word8) -> Ptr C.CSize -> Ptr SignalInfo -> Ptr EncodingInfo -> CFileType -> Ptr OOB -> IO (FormatPtr WriteMode)

foreign import ccall unsafe "sox.h sox_read"
   read :: FormatPtr ReadMode -> Ptr Sample -> C.CSize -> IO C.CSize

foreign import ccall unsafe "sox.h sox_write"
   write :: FormatPtr WriteMode -> Ptr Sample -> C.CSize -> IO C.CSize

foreign import ccall unsafe "sox.h sox_close"
   close :: FormatPtr mode -> IO C.CInt

foreign import ccall unsafe "sox.h sox_seek"
   seek :: FormatPtr mode -> C.CSize -> Whence -> IO C.CInt


-- Is it safe on every platform to pretend that (IO CInt) is (IO ()) ?
foreign import ccall unsafe "sox.h &sox_close"
   closeFun :: FunPtr (FormatPtr mode -> IO ())
