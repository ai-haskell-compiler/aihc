-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.SRGBFormats (
  -- * Extension Support
    gl_NV_sRGB_formats

  -- * GL_NV_sRGB_formats
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_NV
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_NV
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_NV
  , pattern GL_COMPRESSED_SRGB_S3TC_DXT1_NV
  , pattern GL_ETC1_SRGB8_NV
  , pattern GL_SLUMINANCE8_ALPHA8_NV
  , pattern GL_SLUMINANCE8_NV
  , pattern GL_SLUMINANCE_ALPHA_NV
  , pattern GL_SLUMINANCE_NV
  , pattern GL_SRGB8_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_sRGB_formats.txt GL_NV_sRGB_formats> extension is available.

gl_NV_sRGB_formats :: Bool
gl_NV_sRGB_formats = member "GL_NV_sRGB_formats" extensions
{-# NOINLINE gl_NV_sRGB_formats #-}

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_NV  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_NV = 0x8C4D

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_NV  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_NV = 0x8C4E

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_NV  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_NV = 0x8C4F

pattern GL_COMPRESSED_SRGB_S3TC_DXT1_NV  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_S3TC_DXT1_NV = 0x8C4C

pattern GL_ETC1_SRGB8_NV  :: (Eq a, Num a) => a

pattern GL_ETC1_SRGB8_NV = 0x88EE

pattern GL_SLUMINANCE8_ALPHA8_NV  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE8_ALPHA8_NV = 0x8C45

pattern GL_SLUMINANCE8_NV  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE8_NV = 0x8C47

pattern GL_SLUMINANCE_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE_ALPHA_NV = 0x8C44

pattern GL_SLUMINANCE_NV  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE_NV = 0x8C46

pattern GL_SRGB8_NV  :: (Eq a, Num a) => a

pattern GL_SRGB8_NV = 0x8C41