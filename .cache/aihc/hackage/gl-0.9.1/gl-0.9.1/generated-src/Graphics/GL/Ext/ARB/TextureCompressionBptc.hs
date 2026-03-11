-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureCompressionBptc (
  -- * Extension Support
    gl_ARB_texture_compression_bptc

  -- * GL_ARB_texture_compression_bptc
  , pattern GL_COMPRESSED_RGBA_BPTC_UNORM_ARB
  , pattern GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB
  , pattern GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB
  , pattern GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_compression_bptc.txt GL_ARB_texture_compression_bptc> extension is available.

gl_ARB_texture_compression_bptc :: Bool
gl_ARB_texture_compression_bptc = member "GL_ARB_texture_compression_bptc" extensions
{-# NOINLINE gl_ARB_texture_compression_bptc #-}

pattern GL_COMPRESSED_RGBA_BPTC_UNORM_ARB  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_BPTC_UNORM_ARB = 0x8E8C

pattern GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB = 0x8E8E

pattern GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB = 0x8E8F

pattern GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB = 0x8E8D