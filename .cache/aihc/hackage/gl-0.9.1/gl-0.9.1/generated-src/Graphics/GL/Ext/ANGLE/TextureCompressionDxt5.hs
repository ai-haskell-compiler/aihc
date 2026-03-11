-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.TextureCompressionDxt5 (
  -- * Extension Support
    gl_ANGLE_texture_compression_dxt5

  -- * GL_ANGLE_texture_compression_dxt5
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT5_ANGLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_texture_compression_dxt5.txt GL_ANGLE_texture_compression_dxt5> extension is available.

gl_ANGLE_texture_compression_dxt5 :: Bool
gl_ANGLE_texture_compression_dxt5 = member "GL_ANGLE_texture_compression_dxt5" extensions
{-# NOINLINE gl_ANGLE_texture_compression_dxt5 #-}

pattern GL_COMPRESSED_RGBA_S3TC_DXT5_ANGLE  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_S3TC_DXT5_ANGLE = 0x83F3