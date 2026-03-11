-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.TextureFilterCubic (
  -- * Extension Support
    gl_IMG_texture_filter_cubic

  -- * GL_IMG_texture_filter_cubic
  , pattern GL_CUBIC_IMG
  , pattern GL_CUBIC_MIPMAP_LINEAR_IMG
  , pattern GL_CUBIC_MIPMAP_NEAREST_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/IMG/IMG_texture_filter_cubic.txt GL_IMG_texture_filter_cubic> extension is available.

gl_IMG_texture_filter_cubic :: Bool
gl_IMG_texture_filter_cubic = member "GL_IMG_texture_filter_cubic" extensions
{-# NOINLINE gl_IMG_texture_filter_cubic #-}

pattern GL_CUBIC_IMG  :: (Eq a, Num a) => a

pattern GL_CUBIC_IMG = 0x9139

pattern GL_CUBIC_MIPMAP_LINEAR_IMG  :: (Eq a, Num a) => a

pattern GL_CUBIC_MIPMAP_LINEAR_IMG = 0x913B

pattern GL_CUBIC_MIPMAP_NEAREST_IMG  :: (Eq a, Num a) => a

pattern GL_CUBIC_MIPMAP_NEAREST_IMG = 0x913A