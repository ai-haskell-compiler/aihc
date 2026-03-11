-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCompressionLatc (
  -- * Extension Support
    gl_EXT_texture_compression_latc

  -- * GL_EXT_texture_compression_latc
  , pattern GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT
  , pattern GL_COMPRESSED_LUMINANCE_LATC1_EXT
  , pattern GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT
  , pattern GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_compression_latc.txt GL_EXT_texture_compression_latc> extension is available.

gl_EXT_texture_compression_latc :: Bool
gl_EXT_texture_compression_latc = member "GL_EXT_texture_compression_latc" extensions
{-# NOINLINE gl_EXT_texture_compression_latc #-}

pattern GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = 0x8C72

pattern GL_COMPRESSED_LUMINANCE_LATC1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_LUMINANCE_LATC1_EXT = 0x8C70

pattern GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = 0x8C73

pattern GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = 0x8C71