-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.TextureCompressionAstcHdr (
  -- * Extension Support
    gl_KHR_texture_compression_astc_hdr

  -- * GL_KHR_texture_compression_astc_hdr
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

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/KHR/texture_compression_astc_hdr.txt GL_KHR_texture_compression_astc_hdr> extension is available.

gl_KHR_texture_compression_astc_hdr :: Bool
gl_KHR_texture_compression_astc_hdr = member "GL_KHR_texture_compression_astc_hdr" extensions
{-# NOINLINE gl_KHR_texture_compression_astc_hdr #-}