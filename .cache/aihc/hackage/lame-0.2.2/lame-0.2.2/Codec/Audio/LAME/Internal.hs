{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Codec.Audio.LAME.Internal
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level non-public helpers.
module Codec.Audio.LAME.Internal
  ( -- * Types
    Lame,
    VbrMode (..),
    LameException (..),

    -- * Low-level API
    withLame,
    initParams,

    -- ** Input stream description
    setNumSamples,
    setInputSampleRate,
    setNumChannels,
    setScale,
    setOutputSampleRate,

    -- ** General control parameters
    setWriteVbrTag,
    setQuality,
    setFreeFormat,
    setFindReplayGain,
    setNoGapTotal,
    setNoGapCurrentIndex,
    setBitrate,
    setCompressionRatio,

    -- ** Frame parameters
    setCopyright,
    setOriginal,
    setErrorProtection,
    setStrictISO,

    -- ** VBR control
    setVBR,
    setVBRQ,
    setVBRMinBitrate,
    setVBRMeanBitrate,
    setVBRMaxBitrate,
    setVBRHardMin,

    -- ** Filtering control
    setLowpassFreq,
    setLowpassWidth,
    setHighpassFreq,
    setHighpassWidth,

    -- * Tags
    id3TagInit,
    id3TagAddV2,
    id3TagV1Only,
    id3TagV2Only,
    id3TagSetTextInfo,
    id3TagSetComment,
    id3TagSetAlbumArt,

    -- * Encoding
    encodingHelper,
  )
where

import Codec.Audio.Wave
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as B
import Data.Coerce
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Void
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types (CSize (..))

----------------------------------------------------------------------------
-- Types

-- | An opaque newtype wrapper around @'Ptr' 'Void'@ that represents the
-- pointer to the structure that does all the bookkeeping in LAME.
newtype Lame = Lame (Ptr Void)

-- | Enumeration of VBR modes.
data VbrMode
  = -- | VBR RH
    VbrRh
  | -- | VBR ABR
    VbrAbr
  | -- | VBR MTRH
    VbrMtrh
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Enumeration of problems you may have with LAME.
data LameException
  = -- | A “generic error” happened
    LameGenericError
  | -- | Memory allocation issue
    LameNoMemory
  | -- | Unsupported bitrate
    LameBadBitrate
  | -- | Unsupported sample rate
    LameBadSampleFreq
  | -- | An “Internal error” happened
    LameInternalError
  | -- | Invalid track number (first argument) or total number of tracks
    -- (second argument) was supplied
    LameInvalidTrackNumber Word8 (Maybe Word8)
  | -- | This sample format is not supported at this time
    LameUnsupportedSampleFormat SampleFormat
  deriving (Eq, Show, Read)

instance Exception LameException

----------------------------------------------------------------------------
-- Low-level API

-- | Create and use a 'Lame' (pointer structure needed for talking to the
-- LAME API).
--
-- If memory cannot be allocated, 'LameNoMemory' is thrown.
withLame :: (Lame -> IO a) -> IO a
withLame f = bracket lameInit (mapM_ lameClose) $ \mlame ->
  case mlame of
    Nothing -> throwM LameNoMemory
    Just x -> f x

-- | Create a new 'Lame'. In the case of a memory allocation problem
-- 'Nothing' is returned.
lameInit :: IO (Maybe Lame)
lameInit = maybePtr <$> c_lame_init

foreign import ccall unsafe "lame_init"
  c_lame_init :: IO Lame

-- | Free 'Lame' structure and all associated buffers.
lameClose :: Lame -> IO ()
lameClose = c_lame_close

foreign import ccall unsafe "lame_close"
  c_lame_close :: Lame -> IO ()

-- | Set more internal configuration based on previously set parameters.
-- Should be called when everything else is set.
initParams :: Lame -> IO ()
initParams = handleErrors . c_lame_init_params

foreign import ccall unsafe "lame_init_params"
  c_lame_init_params :: Lame -> IO Int

----------------------------------------------------------------------------
-- Input stream description

-- | Set the total number of samples to encode.
setNumSamples :: Lame -> Word64 -> IO ()
setNumSamples l x = handleErrors (c_lame_set_num_samples l x)

foreign import ccall unsafe "lame_set_num_samples"
  c_lame_set_num_samples :: Lame -> Word64 -> IO Int

-- | Set the sample rate of the input stream.
setInputSampleRate :: Lame -> Int -> IO ()
setInputSampleRate l x = handleErrors (c_lame_set_in_samplerate l x)

foreign import ccall unsafe "lame_set_in_samplerate"
  c_lame_set_in_samplerate :: Lame -> Int -> IO Int

-- | Set the number of channels in the input stream.
setNumChannels :: Lame -> Int -> IO ()
setNumChannels l x = handleErrors (c_lame_set_num_channels l x)

foreign import ccall unsafe "lame_set_num_channels"
  c_lame_set_num_channels :: Lame -> Int -> IO Int

-- | Scale the input by this amount before encoding.
setScale :: Lame -> Float -> IO ()
setScale l x = handleErrors (c_lame_set_scale l x)

foreign import ccall unsafe "lame_set_scale"
  c_lame_set_scale :: Lame -> Float -> IO Int

-- | Set the output sample rate in Hz. 0 (default) means that LAME will pick
-- this value automatically.
setOutputSampleRate :: Lame -> Int -> IO ()
setOutputSampleRate l x = handleErrors (c_lame_set_out_samplerate l x)

foreign import ccall unsafe "lame_set_out_samplerate"
  c_lame_set_out_samplerate :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- General control parameters

-- | Set whether to write Xing VBR header frame.
setWriteVbrTag :: Lame -> Bool -> IO ()
setWriteVbrTag l x = handleErrors (c_lame_set_bWriteVbrTag l (fromBool x))

foreign import ccall unsafe "lame_set_bWriteVbrTag"
  c_lame_set_bWriteVbrTag :: Lame -> Int -> IO Int

-- | Select the algorithm. This variable will affect the quality by
-- selecting expensive or cheap algorithms. 0 gives the best quality (very
-- slow). 9 is very fast, but gives the worst quality.
--
--     * 2—near-best quality, not too slow.
--     * 5—good quality, fast.
--     * 7—OK quality, really fast.
setQuality :: Lame -> Int -> IO ()
setQuality l x = handleErrors (c_lame_set_quality l x)

foreign import ccall unsafe "lame_set_quality"
  c_lame_set_quality :: Lame -> Int -> IO Int

-- | Set whether we should use free format.
setFreeFormat :: Lame -> Bool -> IO ()
setFreeFormat l x = handleErrors (c_lame_set_free_format l (fromBool x))

foreign import ccall unsafe "lame_set_free_format"
  c_lame_set_free_format :: Lame -> Int -> IO Int

-- | Set whether we should do ReplayGain analysis.
setFindReplayGain :: Lame -> Bool -> IO ()
setFindReplayGain l x = handleErrors (c_lame_set_findReplayGain l (fromBool x))

foreign import ccall unsafe "lame_set_findReplayGain"
  c_lame_set_findReplayGain :: Lame -> Int -> IO Int

-- | Set the total number of tracks to encode in “no gap” mode.
setNoGapTotal :: Lame -> Int -> IO ()
setNoGapTotal l x = handleErrors (c_lame_set_nogap_total l x)

foreign import ccall unsafe "lame_set_nogap_total"
  c_lame_set_nogap_total :: Lame -> Int -> IO Int

-- | Set the index of the current track to encode in “no gap” mode.
setNoGapCurrentIndex :: Lame -> Int -> IO ()
setNoGapCurrentIndex l x = handleErrors (c_lame_set_nogap_currentindex l x)

foreign import ccall unsafe "lame_set_nogap_currentindex"
  c_lame_set_nogap_currentindex :: Lame -> Int -> IO Int

-- | Set the bitrate.
setBitrate :: Lame -> Int -> IO ()
setBitrate l x = handleErrors (c_lame_set_brate l x)

foreign import ccall unsafe "lame_set_brate"
  c_lame_set_brate :: Lame -> Int -> IO Int

-- | Set the compression ratio.
setCompressionRatio :: Lame -> Float -> IO ()
setCompressionRatio l x = handleErrors (c_lame_set_compression_ratio l x)

foreign import ccall unsafe "lame_set_compression_ratio"
  c_lame_set_compression_ratio :: Lame -> Float -> IO Int

----------------------------------------------------------------------------
-- Frame parameters

-- | Mark as copyright protected.
setCopyright :: Lame -> Bool -> IO ()
setCopyright l x = handleErrors (c_lame_set_copyright l (fromBool x))

foreign import ccall unsafe "lame_set_copyright"
  c_lame_set_copyright :: Lame -> Int -> IO Int

-- | Mark as original.
setOriginal :: Lame -> Bool -> IO ()
setOriginal l x = handleErrors (c_lame_set_original l (fromBool x))

foreign import ccall unsafe "lame_set_original"
  c_lame_set_original :: Lame -> Int -> IO Int

-- | Set whether to use 2 bytes from each frame for CRC checksum.
setErrorProtection :: Lame -> Bool -> IO ()
setErrorProtection l x =
  handleErrors (c_lame_set_error_protection l (fromBool x))

foreign import ccall unsafe "lame_set_error_protection"
  c_lame_set_error_protection :: Lame -> Int -> IO Int

-- | Enforce strict ISO compliance.
setStrictISO :: Lame -> Bool -> IO ()
setStrictISO l x = handleErrors (c_lame_set_strict_ISO l (fromBool x))

foreign import ccall unsafe "lame_set_strict_ISO"
  c_lame_set_strict_ISO :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- VBR control

-- | Set type of VBR.
setVBR :: Lame -> VbrMode -> IO ()
setVBR l x' = handleErrors (c_lame_set_VBR l x)
  where
    x = case x' of
      VbrRh -> 2
      VbrAbr -> 3
      VbrMtrh -> 4

foreign import ccall unsafe "lame_set_VBR"
  c_lame_set_VBR :: Lame -> Int -> IO Int

-- | Set VBR quality level, 0 is highest, 9 is lowest.
setVBRQ :: Lame -> Int -> IO ()
setVBRQ l x = handleErrors (c_lame_set_VBR_q l x)

foreign import ccall unsafe "lame_set_VBR_q"
  c_lame_set_VBR_q :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set the min bitrate in kbps.
setVBRMinBitrate :: Lame -> Int -> IO ()
setVBRMinBitrate l x = handleErrors (c_lame_set_VBR_min_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_min_bitrate_kbps"
  c_lame_set_VBR_min_bitrate_kbps :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set the mean bitrate in kbps.
setVBRMeanBitrate :: Lame -> Int -> IO ()
setVBRMeanBitrate l x = handleErrors (c_lame_set_VBR_mean_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_mean_bitrate_kbps"
  c_lame_set_VBR_mean_bitrate_kbps :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set the max bitrate in kbps.
setVBRMaxBitrate :: Lame -> Int -> IO ()
setVBRMaxBitrate l x = handleErrors (c_lame_set_VBR_max_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_max_bitrate_kbps"
  c_lame_set_VBR_max_bitrate_kbps :: Lame -> Int -> IO Int

-- | Set whether to strictly enforce VBR min bitrate. Normally it will be
-- violated for analog silence.
setVBRHardMin :: Lame -> Bool -> IO ()
setVBRHardMin l x = handleErrors (c_lame_set_VBR_hard_min l (fromBool x))

foreign import ccall unsafe "lame_set_VBR_hard_min"
  c_lame_set_VBR_hard_min :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Filtering control

-- | Set the frequency to put the low-pass filter on. Default is 0 (LAME
-- chooses), -1 will disable the filter altogether.
setLowpassFreq :: Lame -> Int -> IO ()
setLowpassFreq l x = handleErrors (c_lame_set_lowpassfreq l x)

foreign import ccall unsafe "lame_set_lowpassfreq"
  c_lame_set_lowpassfreq :: Lame -> Int -> IO Int

-- | Set width of low-pass transition band. Default is one polyphase filter
-- band.
setLowpassWidth :: Lame -> Int -> IO ()
setLowpassWidth l x = handleErrors (c_lame_set_lowpasswidth l x)

foreign import ccall unsafe "lame_set_lowpasswidth"
  c_lame_set_lowpasswidth :: Lame -> Int -> IO Int

-- | Set the frequency to put the high-pass filter on. Default is 0 (LAME
-- chooses), -1 will disable the filter altogether.
setHighpassFreq :: Lame -> Int -> IO ()
setHighpassFreq l x = handleErrors (c_lame_set_highpassfreq l x)

foreign import ccall unsafe "lame_set_highpassfreq"
  c_lame_set_highpassfreq :: Lame -> Int -> IO Int

-- | Set width of high-pass transition band. Default is one polyphase filter
-- band.
setHighpassWidth :: Lame -> Int -> IO ()
setHighpassWidth l x = handleErrors (c_lame_set_highpasswidth l x)

foreign import ccall unsafe "lame_set_highpasswidth"
  c_lame_set_highpasswidth :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Tags

-- | Initialize something about the tag editing library. The docs are silent
-- what this does.
id3TagInit :: Lame -> IO ()
id3TagInit = c_id3tag_init

foreign import ccall unsafe "id3tag_init"
  c_id3tag_init :: Lame -> IO ()

-- | Force addition of a version 2 tag.
id3TagAddV2 :: Lame -> IO ()
id3TagAddV2 = c_id3tag_add_v2

foreign import ccall unsafe "id3tag_add_v2"
  c_id3tag_add_v2 :: Lame -> IO ()

-- | Add only a version 1 tag.
id3TagV1Only :: Lame -> IO ()
id3TagV1Only = c_id3tag_v1_only

foreign import ccall unsafe "id3tag_v1_only"
  c_id3tag_v1_only :: Lame -> IO ()

-- | Add only a version 2 tag.
id3TagV2Only :: Lame -> IO ()
id3TagV2Only = c_id3tag_v2_only

foreign import ccall unsafe "id3tag_v2_only"
  c_id3tag_v2_only :: Lame -> IO ()

-- | Set a textual tag identifying it by its ID.
id3TagSetTextInfo :: Lame -> String -> Text -> IO ()
id3TagSetTextInfo l id' text =
  handleErrors . withCString id' $ \idPtr ->
    let utf16encodedBs = TE.encodeUtf16LE text
        len = fromIntegral (B.length utf16encodedBs `div` 2)
     in B.unsafeUseAsCString utf16encodedBs $ \textPtr ->
          c_id3tag_set_textinfo_utf16 l idPtr (castPtr textPtr) len

foreign import ccall unsafe "id3tag_set_textinfo_utf16_wrapped"
  c_id3tag_set_textinfo_utf16 :: Lame -> CString -> Ptr Word16 -> Int -> IO Int

-- | Set the comment tag.
id3TagSetComment :: Lame -> Text -> IO ()
id3TagSetComment l text =
  let utf16encodedBs = TE.encodeUtf16LE text
      len = fromIntegral (B.length utf16encodedBs `div` 2)
   in handleErrors . B.unsafeUseAsCString utf16encodedBs $ \textPtr ->
        c_id3tag_set_comment_utf16 l (castPtr textPtr) len

foreign import ccall unsafe "id3tag_set_comment_utf16_wrapped"
  c_id3tag_set_comment_utf16 :: Lame -> Ptr Word16 -> Int -> IO Int

-- | Set the album art.
id3TagSetAlbumArt :: Lame -> ByteString -> IO ()
id3TagSetAlbumArt l img =
  handleErrors . B.unsafeUseAsCStringLen img $ \(dataPtr, dataLen) ->
    c_id3tag_set_albumart l dataPtr (fromIntegral dataLen)

foreign import ccall unsafe "id3tag_set_albumart"
  c_id3tag_set_albumart :: Lame -> CString -> CSize -> IO Int

----------------------------------------------------------------------------
-- Encoding

-- | Encode a given input file.
encodingHelper ::
  -- | The settings
  Lame ->
  -- | Information about input WAVE file
  Wave ->
  -- | Location of input file (normalized)
  FilePath ->
  -- | Location of output file (normalized)
  FilePath ->
  IO ()
encodingHelper l wave@Wave {..} ipath opath =
  handleErrors . withCString ipath $ \ipathPtr ->
    withCString opath $ \opathPtr ->
      c_lame_encoding_helper
        l -- lame settings structure
        (fromIntegral waveDataOffset) -- offset of the data chunk
        waveDataSize -- the size of the data chunk
        ( case waveSampleFormat of
            SampleFormatPcmInt _ -> 0
            SampleFormatIeeeFloat32Bit -> 1
            SampleFormatIeeeFloat64Bit -> 2
        )
        (waveBitsPerSample wave) -- bits per sample
        ipathPtr -- path to input file
        opathPtr -- path to output file

foreign import ccall unsafe "lame_encoding_helper"
  c_lame_encoding_helper ::
    Lame -> -- lame settings structure
    Word64 -> -- offset of data chunk
    Word64 -> -- size of data chunk
    Word16 -> -- code of sample format
    Word16 -> -- bits per sample
    CString -> -- path to input file
    CString -> -- path to output file
    IO Int

----------------------------------------------------------------------------
-- Helpers

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged. Needless to say,
-- this is unsafe.
maybePtr :: (Coercible a (Ptr p)) => a -> Maybe a
maybePtr a
  | coerce a == nullPtr = Nothing
  | otherwise = Just a

-- | Treat an 'Int' value as an error code. Unless it's 0, throw a
-- corresponding 'LameException', otherwise just return the unit.
handleErrors :: IO Int -> IO ()
handleErrors m = do
  n <- m
  case n of
    0 -> return ()
    -10 -> throwM LameNoMemory
    -11 -> throwM LameBadBitrate
    -12 -> throwM LameBadSampleFreq
    -13 -> throwM LameInternalError
    _ -> throwM LameGenericError
