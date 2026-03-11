-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureRectangle (
  -- * Extension Support
    gl_ARB_texture_rectangle

  -- * GL_ARB_texture_rectangle
  , pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB
  , pattern GL_PROXY_TEXTURE_RECTANGLE_ARB
  , pattern GL_TEXTURE_BINDING_RECTANGLE_ARB
  , pattern GL_TEXTURE_RECTANGLE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_rectangle.txt GL_ARB_texture_rectangle> extension is available.

gl_ARB_texture_rectangle :: Bool
gl_ARB_texture_rectangle = member "GL_ARB_texture_rectangle" extensions
{-# NOINLINE gl_ARB_texture_rectangle #-}

pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = 0x84F8

pattern GL_PROXY_TEXTURE_RECTANGLE_ARB  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_RECTANGLE_ARB = 0x84F7

pattern GL_TEXTURE_BINDING_RECTANGLE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BINDING_RECTANGLE_ARB = 0x84F6

pattern GL_TEXTURE_RECTANGLE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RECTANGLE_ARB = 0x84F5