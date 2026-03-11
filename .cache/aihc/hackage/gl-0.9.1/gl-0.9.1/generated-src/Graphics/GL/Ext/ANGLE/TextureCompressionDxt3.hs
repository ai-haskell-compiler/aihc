-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.TextureCompressionDxt3 (
  -- * Extension Support
    gl_ANGLE_texture_compression_dxt3

  -- * GL_ANGLE_texture_compression_dxt3
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT3_ANGLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_texture_compression_dxt3.txt GL_ANGLE_texture_compression_dxt3> extension is available.

gl_ANGLE_texture_compression_dxt3 :: Bool
gl_ANGLE_texture_compression_dxt3 = member "GL_ANGLE_texture_compression_dxt3" extensions
{-# NOINLINE gl_ANGLE_texture_compression_dxt3 #-}

pattern GL_COMPRESSED_RGBA_S3TC_DXT3_ANGLE  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_S3TC_DXT3_ANGLE = 0x83F2