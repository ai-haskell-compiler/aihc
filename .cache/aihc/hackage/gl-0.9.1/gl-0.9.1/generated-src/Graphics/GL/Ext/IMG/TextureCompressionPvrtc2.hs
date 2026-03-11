-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.TextureCompressionPvrtc2 (
  -- * Extension Support
    gl_IMG_texture_compression_pvrtc2

  -- * GL_IMG_texture_compression_pvrtc2
  , pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG
  , pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt GL_IMG_texture_compression_pvrtc2> extension is available.

gl_IMG_texture_compression_pvrtc2 :: Bool
gl_IMG_texture_compression_pvrtc2 = member "GL_IMG_texture_compression_pvrtc2" extensions
{-# NOINLINE gl_IMG_texture_compression_pvrtc2 #-}

pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG = 0x9137

pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG = 0x9138