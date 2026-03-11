-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.TextureCompressionPvrtc (
  -- * Extension Support
    gl_IMG_texture_compression_pvrtc

  -- * GL_IMG_texture_compression_pvrtc
  , pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG
  , pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG
  , pattern GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG
  , pattern GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc.txt GL_IMG_texture_compression_pvrtc> extension is available.

gl_IMG_texture_compression_pvrtc :: Bool
gl_IMG_texture_compression_pvrtc = member "GL_IMG_texture_compression_pvrtc" extensions
{-# NOINLINE gl_IMG_texture_compression_pvrtc #-}

pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03

pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02

pattern GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = 0x8C01

pattern GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = 0x8C00