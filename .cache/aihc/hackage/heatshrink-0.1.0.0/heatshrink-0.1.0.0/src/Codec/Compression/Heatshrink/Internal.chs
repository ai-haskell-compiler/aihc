{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}


--------------------------------------------------------------------------------
module Codec.Compression.Heatshrink.Internal where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word


--------------------------------------------------------------------------------
{#pointer *size_t as CSizePtr -> CSize #}
{#pointer *uint8_t as CUCharPtr -> CUChar #}


-- * decoding
--------------------------------------------------------------------------------
#include "heatshrink_decoder.h"

--------------------------------------------------------------------------------
{#enum HSD_sink_res as HsdSinkRes {underscoreToCase} deriving (Show, Eq) #}
{#enum HSD_poll_res as HsdPollRes {underscoreToCase} deriving (Show, Eq) #}
{#enum HSD_finish_res as HsdFinishRes {underscoreToCase} deriving (Show, Eq) #}

--------------------------------------------------------------------------------
data HeatshrinkDecoderT

--------------------------------------------------------------------------------
{#pointer *heatshrink_decoder as HeatshrinkDecoderTPtr foreign finalizer heatshrink_decoder_free #}
--{#pointer *heatshrink_decoder as HeatshrinkDecoderTPtr foreign #}

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
{#fun pure unsafe heatshrink_decoder_alloc as ^
  { `Int'
  , `Int'
  , `Int'} -> `HeatshrinkDecoderTPtr'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_decoder_free as ^
  { `HeatshrinkDecoderTPtr' } -> `()'
#}

--------------------------------------------------------------------------------
{#fun unsafe heatshrink_decoder_reset as ^
  { `HeatshrinkDecoderTPtr' } -> `()'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_decoder_sink as ^
  { `HeatshrinkDecoderTPtr'
  , castPtr `Ptr Word8'
  , cIntConv `CSize'
  , alloca- `CSize' peek* } -> `HsdSinkRes'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_decoder_poll as ^
  { `HeatshrinkDecoderTPtr'
  , castPtr `Ptr Word8'
  , cIntConv `CSize'
  , alloca- `CSize' peek* } -> `HsdPollRes'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_decoder_finish as ^
  { `HeatshrinkDecoderTPtr' } -> `HsdFinishRes'
#}


--------------------------------------------------------------------------------
cIntConv :: (Integral a, Num b) =>  a -> b
cIntConv = fromIntegral


--------------------------------------------------------------------------------

-- encoding
#include "heatshrink_encoder.h"

{#enum HSE_sink_res as HseSinkRes {underscoreToCase} deriving (Show, Eq) #}
{#enum HSE_poll_res as HsePollRes {underscoreToCase} deriving (Show, Eq) #}
{#enum HSE_finish_res as HseFinishRes {underscoreToCase} deriving (Show, Eq) #}

data HeatshrinkEncoderT

{#pointer *heatshrink_encoder as HeatshrinkEncoderTPtr foreign finalizer heatshrink_encoder_free #}

{#fun pure unsafe heatshrink_encoder_alloc as ^
  { `Int'
  , `Int'} -> `HeatshrinkEncoderTPtr'
#}

--------------------------------------------------------------------------------
{#fun unsafe heatshrink_encoder_free as ^
  { `HeatshrinkEncoderTPtr' } -> `()'
#}

--------------------------------------------------------------------------------
{#fun unsafe heatshrink_encoder_reset as ^
  { `HeatshrinkEncoderTPtr' } -> `()'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_encoder_sink as ^
  { `HeatshrinkEncoderTPtr'
  , castPtr `Ptr Word8'
  , cIntConv `CSize'
  , alloca- `CSize' peek* } -> `HseSinkRes'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_encoder_poll as ^
  { `HeatshrinkEncoderTPtr'
  , castPtr `Ptr Word8'
  , cIntConv `CSize'
  , alloca- `CSize' peek* } -> `HsePollRes'
#}


--------------------------------------------------------------------------------
{#fun unsafe heatshrink_encoder_finish as ^
  { `HeatshrinkEncoderTPtr' } -> `HseFinishRes'
#}
