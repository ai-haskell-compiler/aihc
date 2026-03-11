-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureRectangle (
  -- * Extension Support
    gl_NV_texture_rectangle

  -- * GL_NV_texture_rectangle
  , pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_NV
  , pattern GL_PROXY_TEXTURE_RECTANGLE_NV
  , pattern GL_TEXTURE_BINDING_RECTANGLE_NV
  , pattern GL_TEXTURE_RECTANGLE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_rectangle.txt GL_NV_texture_rectangle> extension is available.

gl_NV_texture_rectangle :: Bool
gl_NV_texture_rectangle = member "GL_NV_texture_rectangle" extensions
{-# NOINLINE gl_NV_texture_rectangle #-}

pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = 0x84F8

pattern GL_PROXY_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_RECTANGLE_NV = 0x84F7

pattern GL_TEXTURE_BINDING_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_RECTANGLE_NV = 0x84F6

pattern GL_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RECTANGLE_NV = 0x84F5