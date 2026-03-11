-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ThreeDFX.TextureCompressionFXT1 (
  -- * Extension Support
    gl_3DFX_texture_compression_FXT1

  -- * GL_3DFX_texture_compression_FXT1
  , pattern GL_COMPRESSED_RGBA_FXT1_3DFX
  , pattern GL_COMPRESSED_RGB_FXT1_3DFX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/3DFX/texture_compression_FXT1.txt GL_3DFX_texture_compression_FXT1> extension is available.

gl_3DFX_texture_compression_FXT1 :: Bool
gl_3DFX_texture_compression_FXT1 = member "GL_3DFX_texture_compression_FXT1" extensions
{-# NOINLINE gl_3DFX_texture_compression_FXT1 #-}

pattern GL_COMPRESSED_RGBA_FXT1_3DFX  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGBA_FXT1_3DFX = 0x86B1

pattern GL_COMPRESSED_RGB_FXT1_3DFX  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RGB_FXT1_3DFX = 0x86B0