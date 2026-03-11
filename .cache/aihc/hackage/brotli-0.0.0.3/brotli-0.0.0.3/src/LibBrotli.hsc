{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CApiFFI            #-}
{-# LANGUAGE RecordWildCards    #-}

-- Copyright (C) 2016  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Internal low-level bindings to libbrotli
module LibBrotli
   ( CompressParams(..), defaultCompressParams
   , CompressionLevel(..), CompressionWindowSize(..), CompressionMode(..)
   , DecompressParams(..), defaultDecompressParams
   , BrotliState(..), BrotliEncOp(..)
   , newBrotliEncoder, newBrotliDecoder
   , finalizeBrotliEncoder, finalizeBrotliDecoder
   , runBrotliEncoder, runBrotliDecoder
   , readBrotliEncoder, readBrotliDecoder
   , brotliEncoderVersion, brotliDecoderVersion
   , BrotliDecoderErrorCode(..), showBrotliDecoderErrorCode
   ) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.ByteString (ByteString)
import           Foreign
import           Foreign.C
import           System.IO.Unsafe as Unsafe (unsafePerformIO)
import           Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import           Control.Monad.Trans.Maybe (MaybeT(..))

#include "hs_brotli.h"

packBS :: Ptr Word8 -> CSize -> IO ByteString
packBS p sz
  | sz > 0    = BS.packCStringLen (castPtr p, fromIntegral sz)
  | otherwise = pure BS.empty

----------------------------------------------------------------------------

type BrotliBool = #type BROTLI_BOOL

fromBrotliBool :: BrotliBool -> Maybe Bool
fromBrotliBool #{const BROTLI_TRUE}  = Just True
fromBrotliBool #{const BROTLI_FALSE} = Just False
fromBrotliBool _ = Nothing

toBrotliBool :: Bool -> BrotliBool
toBrotliBool False =  #{const BROTLI_FALSE}
toBrotliBool True  =  #{const BROTLI_TRUE}

----------------------------------------------------------------------------

-- | Errors signalled by the Brotli decoder. Use 'showBrotliDecoderErrorCode' to convert the numeric code into an error message.
newtype BrotliDecoderErrorCode = BrotliDecoderErrorCode Int deriving (Eq, Show)

instance Exception BrotliDecoderErrorCode

-- | Convert numeric 'BrotliDecoderErrorCode' into textual error message.
{-# NOINLINE showBrotliDecoderErrorCode #-}
showBrotliDecoderErrorCode :: BrotliDecoderErrorCode -> String
showBrotliDecoderErrorCode (BrotliDecoderErrorCode ec) = Unsafe.unsafePerformIO $ do
  bufptr <- c_BrotliDecoderErrorString (fromIntegral ec)
  if bufptr == nullPtr then pure "" else peekCAString bufptr

----------------------------------------------------------------------------

-- internal state enumeration
data BrotliState
    = BSNeedsInput -- NB: initial state
    | BSHasOutput
    | BSFinished
    | BSFail
    | BSInternalError -- the impossible happened
    deriving (Eq,Show)

type HsBrotliState = #{type HsBrotliState}

fromHsBrotliState :: HsBrotliState -> BrotliState
fromHsBrotliState x = case x of
  #{const HS_BS_NEEDS_INPUT} -> BSNeedsInput
  #{const HS_BS_HAS_OUTPUT}  -> BSHasOutput
  #{const HS_BS_FINISHED}    -> BSFinished
  #{const HS_BS_FAIL}        -> BSFail
  _                          -> BSInternalError

----------------------------------------------------------------------------

-- | Set of parameters for compression. The defaults are 'defaultCompressParams'.
data CompressParams = CompressParams
    { compressLevel :: !CompressionLevel -- ^ 'CompressParams' field: See documentation of 'CompressionLevel'
    , compressWindowSize :: !CompressionWindowSize -- ^ 'CompressParams' field: See documentation of 'CompressionWindowSize'
    , compressMode  :: !CompressionMode  -- ^ 'CompressParams' field: See documentation of 'CompressionMode'
    , compressSizeHint :: !Word32 -- ^ 'CompressParams' field: Estimated total input size. The default value is @0@, which means that the total input size is unknown.
    }

-- | Set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
data DecompressParams = DecompressParams
    { decompressDisableRingBufferReallocation :: !Bool -- ^ 'DecompressParams' field: If 'True', ring buffer is allocated according to window size, despite the real size of the content. (default: 'False')
    }

-- | The default set of parameters for compression. This is typically
-- used with the 'Codec.Compression.Brotli.compressWith' function with specific parameters
-- overridden.
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {..}
  where
    compressLevel      = maxBound
    compressWindowSize = CompressionWindowBits22
    compressMode       = CompressionModeGeneric
    compressSizeHint   = 0

-- | The default set of parameters for decompression. This is
-- typically used with the 'Codec.Compression.Brotli.decompressWith' function with specific
-- parameters overridden.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams False

-- | C pointer to @BrotliEncoderState@
newtype BrotliEncoder = BrotliEncoder (ForeignPtr BrotliEncoder)

-- | C pointer to @BrotliDecoderState@
newtype BrotliDecoder = BrotliDecoder (ForeignPtr BrotliDecoder)

newBrotliEncoder :: CompressParams -> ST s (Maybe BrotliEncoder)
newBrotliEncoder CompressParams{..} = unsafeIOToST $ runMaybeT $ do
    fp <- MaybeT createBrotliEncoder

    unless (#{const BROTLI_DEFAULT_QUALITY} == qualityParm) $ do
      rc <- setParm fp #{const BROTLI_PARAM_QUALITY} qualityParm
      unless rc $ fail "invalid BROTLI_PARAM_QUALITY"

    unless (#{const BROTLI_DEFAULT_MODE} == modeParm) $ do
      rc <- setParm fp #{const BROTLI_PARAM_MODE} modeParm
      unless rc $ fail "invalid BROTLI_PARAM_MODE"

    unless (#{const BROTLI_DEFAULT_WINDOW} == winParm) $ do
      unless ((#{const BROTLI_MIN_WINDOW_BITS} <= winParm) && (winParm <= #{const BROTLI_MAX_WINDOW_BITS})) $
        fail "invalid BROTLI_PARAM_LGWIN (internal inconsistency)"

      rc <- setParm fp #{const BROTLI_PARAM_LGWIN} winParm
      unless rc $ fail "invalid BROTLI_PARAM_LGWIN"

    unless (0 == compressSizeHint) $ do
      rc <- setParm fp #{const BROTLI_PARAM_SIZE_HINT} compressSizeHint
      unless rc $ fail "invalid BROTLI_PARAM_SIZE_HINT"

    pure fp
  where
    setParm (BrotliEncoder fp) k v = MaybeT (withForeignPtr fp $ \p -> fromBrotliBool <$> c_BrotliEncoderSetParameter p k v)

    qualityParm = fromIntegral (fromEnum compressLevel)
    winParm = fromIntegral (fromEnum compressWindowSize)
    modeParm    = case compressMode of
                    CompressionModeGeneric -> #{const BROTLI_MODE_GENERIC}
                    CompressionModeText    -> #{const BROTLI_MODE_TEXT}
                    CompressionModeFont    -> #{const BROTLI_MODE_FONT}

newBrotliDecoder :: DecompressParams -> ST s (Maybe BrotliDecoder)
newBrotliDecoder DecompressParams{..} = unsafeIOToST $ runMaybeT $ do
    fp <- MaybeT createBrotliDecoder

    when decompressDisableRingBufferReallocation $ do
      rc <- setParm fp #{const BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION}
                       (fromIntegral $ toBrotliBool decompressDisableRingBufferReallocation)
      unless rc $ fail "invalid BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION"

    pure fp
  where
    setParm (BrotliDecoder fp) k v = MaybeT (withForeignPtr fp $ \p -> fromBrotliBool <$> c_BrotliDecoderSetParameter p k v)

createBrotliEncoder :: IO (Maybe BrotliEncoder)
createBrotliEncoder = mask_ $ do
    p <- c_BrotliEncoderCreateInstance nullPtr nullPtr nullPtr
    case () of
      _ | p == nullPtr -> pure Nothing
        | otherwise -> do
            !fp <- newForeignPtr cp_BrotliEncoderDestroyInstance p
            pure (Just (BrotliEncoder fp))

createBrotliDecoder :: IO (Maybe BrotliDecoder)
createBrotliDecoder = mask_ $ do
    p <- c_BrotliDecoderCreateInstance nullPtr nullPtr nullPtr
    case () of
      _ | p == nullPtr -> pure Nothing
        | otherwise -> do
            !fp <- newForeignPtr cp_BrotliDecoderDestroyInstance p
            pure (Just (BrotliDecoder fp))

finalizeBrotliEncoder :: BrotliEncoder -> ST s ()
finalizeBrotliEncoder (BrotliEncoder s) = unsafeIOToST (finalizeForeignPtr s)

finalizeBrotliDecoder :: BrotliDecoder -> ST s ()
finalizeBrotliDecoder (BrotliDecoder s) = unsafeIOToST (finalizeForeignPtr s)

data BrotliEncOp
     = BrotliEncOpProcess
     | BrotliEncOpFlush
     | BrotliEncOpFinish
--   | BrotliEncOpEmitMetadata -- TODO

runBrotliEncoder :: BrotliEncoder
                 -> ByteString
                 -> BrotliEncOp
                 -> ST s (BrotliState,Int) {- (state, unused-input) -}
runBrotliEncoder (BrotliEncoder fp) ibs action0
  = unsafeIOToST $ withForeignPtr fp $ \encPtr -> do
      BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) ->
        with (fromIntegral ibslen) $ \availIn ->
          with ibsptr $ \nextIn -> do
            with 0 $ \availOut -> do
              rc <- fromHsBrotliState <$>
                    c_BrotliEncoderCompressStream encPtr action availIn (castPtr nextIn) availOut nullPtr nullPtr
              availIn' <- fromIntegral <$> peek availIn
              pure (rc, availIn')
  where
    action = case action0 of
               BrotliEncOpProcess -> #{const BROTLI_OPERATION_PROCESS}
               BrotliEncOpFinish  -> #{const BROTLI_OPERATION_FINISH}
               BrotliEncOpFlush   -> #{const BROTLI_OPERATION_FLUSH}

runBrotliDecoder :: BrotliDecoder
                 -> ByteString
                 -> ST s (BrotliState,BrotliDecoderErrorCode,Int {- unused-input -})
runBrotliDecoder (BrotliDecoder fp) ibs
  = unsafeIOToST $ withForeignPtr fp $ \encPtr -> do
      BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) ->
        with (fromIntegral ibslen) $ \availIn ->
          with ibsptr $ \nextIn -> do
            with 0 $ \availOut -> do
              rc <- fromHsBrotliState <$>
                    c_BrotliDecoderDecompressStream encPtr availIn (castPtr nextIn) availOut nullPtr nullPtr
              availIn' <- fromIntegral <$> peek availIn
              ecode <- BrotliDecoderErrorCode <$> if rc == BSFail
                then fromIntegral <$> c_BrotliDecoderGetErrorCode encPtr
                else pure 0
              pure (rc, ecode, availIn')

readBrotliEncoder :: BrotliEncoder -> Int {- max-output -} -> ST s (BrotliState, ByteString)
readBrotliEncoder (BrotliEncoder fp) maxobs
  = unsafeIOToST $ withForeignPtr fp $ \encPtr -> do
      with (fromIntegral maxobs) $ \availOutPtr -> do
        alloca $ \obsptrptr -> do
          rc <- fromHsBrotliState <$>
                c_BrotliEncoderTakeOutput encPtr availOutPtr obsptrptr
          availOut' <- peek availOutPtr
          obsptr <- peek obsptrptr
          buf <- packBS obsptr availOut'
          pure (rc, buf)

readBrotliDecoder :: BrotliDecoder -> Int {- max-output -} -> ST s (BrotliState, ByteString)
readBrotliDecoder (BrotliDecoder fp) maxobs
  = unsafeIOToST $ withForeignPtr fp $ \encPtr -> do
      with (fromIntegral maxobs) $ \availOutPtr -> do
        alloca $ \obsptrptr -> do
          rc <- fromHsBrotliState <$>
                c_BrotliDecoderTakeOutput encPtr availOutPtr obsptrptr
          availOut' <- peek availOutPtr
          obsptr <- peek obsptrptr
          buf <- packBS obsptr availOut'
          pure (rc, buf)

----------------------------------------------------------------------------

-- | Encoding profile
data CompressionMode
    = CompressionModeGeneric -- ^ Format-agnostic default mode
    | CompressionModeText    -- ^ UTF-8 formatted text data
    | CompressionModeFont    -- ^ Compression mode used in WOFF 2.0
    deriving (Eq, Read, Show)

-- | Compression quality setting
data CompressionLevel
    = CompressionLevel0 -- ^ fastest/lowest compression level
    | CompressionLevel1
    | CompressionLevel2
    | CompressionLevel3
    | CompressionLevel4
    | CompressionLevel5
    | CompressionLevel6
    | CompressionLevel7
    | CompressionLevel8
    | CompressionLevel9
    | CompressionLevel10
    | CompressionLevel11 -- ^ slowest/highest compression level (default)
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Recommended sliding LZ77 window size.
--
-- The encoder may reduce this value (if e.g. input is much smaller than window size).
--
data CompressionWindowSize
    = CompressionWindowBits10 -- ^ 1008 bytes
    | CompressionWindowBits11 -- ^ 2032 bytes
    | CompressionWindowBits12 -- ^ 4080 bytes
    | CompressionWindowBits13 -- ^ 8176 bytes
    | CompressionWindowBits14 -- ^ 16368 bytes
    | CompressionWindowBits15 -- ^ 32752 bytes
    | CompressionWindowBits16 -- ^ 65520 bytes
    | CompressionWindowBits17 -- ^ 131056 bytes
    | CompressionWindowBits18 -- ^ 262128 bytes
    | CompressionWindowBits19 -- ^ 524272 bytes
    | CompressionWindowBits20 -- ^ 1048560 bytes
    | CompressionWindowBits21 -- ^ 2097136 bytes
    | CompressionWindowBits22 -- ^ 4194288 bytes (default)
    | CompressionWindowBits23 -- ^ 8388592 bytes
    | CompressionWindowBits24 -- ^ 16777200 bytes
    deriving (Eq, Ord, Read, Show, Bounded)

-- | This 'Enum' instance is offset by 10.
--
-- >>> toEnum CompressionWindowBits10
-- 10
--
-- >>> fromEnum 22 :: CompressionWindowSize
-- CompressionWindowBits22
--
instance Enum CompressionWindowSize where
  toEnum i = case i of
    10 -> CompressionWindowBits10
    11 -> CompressionWindowBits11
    12 -> CompressionWindowBits12
    13 -> CompressionWindowBits13
    14 -> CompressionWindowBits14
    15 -> CompressionWindowBits15
    16 -> CompressionWindowBits16
    17 -> CompressionWindowBits17
    18 -> CompressionWindowBits18
    19 -> CompressionWindowBits19
    20 -> CompressionWindowBits20
    21 -> CompressionWindowBits21
    22 -> CompressionWindowBits22
    23 -> CompressionWindowBits23
    24 -> CompressionWindowBits24
    _  -> error "toEnum(CompressionWindowSize): bad argument"

  fromEnum x = case x of
    CompressionWindowBits10 -> 10
    CompressionWindowBits11 -> 11
    CompressionWindowBits12 -> 12
    CompressionWindowBits13 -> 13
    CompressionWindowBits14 -> 14
    CompressionWindowBits15 -> 15
    CompressionWindowBits16 -> 16
    CompressionWindowBits17 -> 17
    CompressionWindowBits18 -> 18
    CompressionWindowBits19 -> 19
    CompressionWindowBits20 -> 20
    CompressionWindowBits21 -> 21
    CompressionWindowBits22 -> 22
    CompressionWindowBits23 -> 23
    CompressionWindowBits24 -> 24

----------------------------------------------------------------------------
-- FFI imports
--
-- encoder

foreign import capi "hs_brotli.h BrotliEncoderCreateInstance"
    c_BrotliEncoderCreateInstance :: Ptr () -> Ptr () -> Ptr () -> IO (Ptr BrotliEncoder)

-- foreign import capi "hs_brotli.h BrotliEncoderDestroyInstance"
--     c_BrotliEncoderDestroyInstance :: Ptr BrotliEncoder -> IO ()

foreign import capi "hs_brotli.h &BrotliEncoderDestroyInstance"
    cp_BrotliEncoderDestroyInstance :: FunPtr (Ptr BrotliEncoder -> IO ())

foreign import capi "hs_brotli.h HsBrotliEncoderCompressStream"
    c_BrotliEncoderCompressStream :: Ptr BrotliEncoder
                                  -> #{type BrotliEncoderOperation}
                                  -> Ptr CSize       {- available_in -}
                                  -> Ptr (Ptr Word8) {- next_in -}
                                  -> Ptr CSize       {- available_out -}
                                  -> Ptr (Ptr Word8) {- next_out -}
                                  -> Ptr CSize       {- total_out -}
                                  -> IO HsBrotliState

-- foreign import capi unsafe "hs_brotli.h BrotliEncoderIsFinished"
--     c_BrotliEncoderIsFinished :: Ptr BrotliEncoder -> IO BrotliBool
--
-- foreign import capi unsafe "hs_brotli.h BrotliEncoderHasMoreOutput"
--     c_BrotliEncoderHasMoreOutput :: Ptr BrotliEncoder -> IO BrotliBool

foreign import capi unsafe "hs_brotli.h HsBrotliEncoderTakeOutput"
    c_BrotliEncoderTakeOutput :: Ptr BrotliEncoder -> Ptr CSize -> Ptr (Ptr Word8) -> IO HsBrotliState

-- e.g. 0x01000000
foreign import capi unsafe "hs_brotli.h BrotliEncoderVersion" brotliEncoderVersion :: Word32


-- BROTLI_BOOL BrotliEncoderSetParameter(BrotliEncoderState *state, BrotliEncoderParameter param, uint32_t value)

foreign import capi unsafe "hs_brotli.h BrotliEncoderSetParameter"
    c_BrotliEncoderSetParameter :: Ptr BrotliEncoder -> #{type BrotliEncoderParameter} -> Word32 -> IO BrotliBool

----------------------------------------------------------------------------
-- decoder

foreign import capi "hs_brotli.h BrotliDecoderCreateInstance"
    c_BrotliDecoderCreateInstance :: Ptr () -> Ptr () -> Ptr () -> IO (Ptr BrotliDecoder)

-- foreign import capi "hs_brotli.h BrotliDecoderDestroyInstance"
--     c_BrotliDecoderDestroyInstance :: Ptr BrotliDecoder -> IO ()

foreign import capi "hs_brotli.h &BrotliDecoderDestroyInstance"
    cp_BrotliDecoderDestroyInstance :: FunPtr (Ptr BrotliDecoder -> IO ())

foreign import capi "hs_brotli.h HsBrotliDecoderDecompressStream"
    c_BrotliDecoderDecompressStream :: Ptr BrotliDecoder
                                    -> Ptr CSize       {- available_in -}
                                    -> Ptr (Ptr Word8) {- next_in -}
                                    -> Ptr CSize       {- available_out -}
                                    -> Ptr (Ptr Word8) {- next_out -}
                                    -> Ptr CSize       {- total_out -}
                                    -> IO #{type HsBrotliState}

-- foreign import capi unsafe "hs_brotli.h BrotliDecoderIsFinished"
--     c_BrotliDecoderIsFinished :: Ptr BrotliDecoder -> IO BrotliBool
--
-- foreign import capi unsafe "hs_brotli.h BrotliDecoderHasMoreOutput"
--     c_BrotliDecoderHasMoreOutput :: Ptr BrotliDecoder -> IO BrotliBool

foreign import capi unsafe "hs_brotli.h HsBrotliDecoderTakeOutput"
    c_BrotliDecoderTakeOutput :: Ptr BrotliDecoder -> Ptr CSize -> Ptr (Ptr Word8) -> IO HsBrotliState

-- e.g. 0x01000000
foreign import capi unsafe "hs_brotli.h BrotliDecoderVersion" brotliDecoderVersion :: Word32

foreign import capi unsafe "hs_brotli.h BrotliDecoderSetParameter"
    c_BrotliDecoderSetParameter :: Ptr BrotliDecoder -> #{type BrotliDecoderParameter} -> Word32 -> IO BrotliBool

foreign import capi unsafe "hs_brotli.h BrotliDecoderGetErrorCode"
    c_BrotliDecoderGetErrorCode :: Ptr BrotliDecoder -> IO #{type BrotliDecoderErrorCode}

foreign import capi unsafe "hs_brotli.h HsBrotliDecoderErrorString"
    c_BrotliDecoderErrorString :: #{type BrotliDecoderErrorCode} -> IO CString
