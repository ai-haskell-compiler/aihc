-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGI.TextureColorTable (
  -- * Extension Support
    gl_SGI_texture_color_table

  -- * GL_SGI_texture_color_table
  , pattern GL_PROXY_TEXTURE_COLOR_TABLE_SGI
  , pattern GL_TEXTURE_COLOR_TABLE_SGI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGI/texture_color_table.txt GL_SGI_texture_color_table> extension is available.

gl_SGI_texture_color_table :: Bool
gl_SGI_texture_color_table = member "GL_SGI_texture_color_table" extensions
{-# NOINLINE gl_SGI_texture_color_table #-}

pattern GL_PROXY_TEXTURE_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_COLOR_TABLE_SGI = 0x80BD

pattern GL_TEXTURE_COLOR_TABLE_SGI  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COLOR_TABLE_SGI = 0x80BC