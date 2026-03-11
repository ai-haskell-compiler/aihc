-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCompressionS3tc (
  -- * Extension Support
    gl_EXT_texture_compression_s3tc

  -- * GL_EXT_texture_compression_s3tc
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
  , pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_compression_s3tc.txt GL_EXT_texture_compression_s3tc> extension is available.

gl_EXT_texture_compression_s3tc :: Bool
gl_EXT_texture_compression_s3tc = member "GL_EXT_texture_compression_s3tc" extensions
{-# NOINLINE gl_EXT_texture_compression_s3tc #-}

pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = 0x83F2

pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = 0x83F3