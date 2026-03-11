-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureShader2 (
  -- * Extension Support
    gl_NV_texture_shader2

  -- * GL_NV_texture_shader2
  , pattern GL_DOT_PRODUCT_TEXTURE_3D_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_shader2.txt GL_NV_texture_shader2> extension is available.

gl_NV_texture_shader2 :: Bool
gl_NV_texture_shader2 = member "GL_NV_texture_shader2" extensions
{-# NOINLINE gl_NV_texture_shader2 #-}

pattern GL_DOT_PRODUCT_TEXTURE_3D_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_TEXTURE_3D_NV = 0x86EF