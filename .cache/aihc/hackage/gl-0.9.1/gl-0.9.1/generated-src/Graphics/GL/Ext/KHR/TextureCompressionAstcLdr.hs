-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.TextureCompressionAstcLdr (
  -- * Extension Support
    gl_KHR_texture_compression_astc_ldr

  -- * GL_KHR_texture_compression_astc_ldr
  , pattern GL_COMPRESSED_RGBA_ASTC_10x10_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_10x5_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_10x6_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_10x8_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_12x10_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_12x12_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_4x4_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_5x4_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_5x5_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_6x5_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_6x6_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_8x5_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_8x6_KHR
  , pattern GL_COMPRESSED_RGBA_ASTC_8x8_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/KHR_texture_compression_astc_ldr.txt GL_KHR_texture_compression_astc_ldr> extension is available.

gl_KHR_texture_compression_astc_ldr :: Bool
gl_KHR_texture_compression_astc_ldr = member "GL_KHR_texture_compression_astc_ldr" extensions
{-# NOINLINE gl_KHR_texture_compression_astc_ldr #-}