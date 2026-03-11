-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCompressionDxt1 (
  -- * Extension Support
    gl_EXT_texture_compression_dxt1

  -- * GL_EXT_texture_compression_dxt1
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_compression_dxt1.txt GL_EXT_texture_compression_dxt1> extension is available.

gl_EXT_texture_compression_dxt1 :: Bool
gl_EXT_texture_compression_dxt1 = member "GL_EXT_texture_compression_dxt1" extensions
{-# NOINLINE gl_EXT_texture_compression_dxt1 #-}