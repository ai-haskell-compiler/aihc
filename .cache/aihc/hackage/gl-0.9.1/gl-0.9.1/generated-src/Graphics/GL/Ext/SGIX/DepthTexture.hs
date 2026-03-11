-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.DepthTexture (
  -- * Extension Support
    gl_SGIX_depth_texture

  -- * GL_SGIX_depth_texture
  , pattern GL_DEPTH_COMPONENT16_SGIX
  , pattern GL_DEPTH_COMPONENT24_SGIX
  , pattern GL_DEPTH_COMPONENT32_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/depth_texture.txt GL_SGIX_depth_texture> extension is available.

gl_SGIX_depth_texture :: Bool
gl_SGIX_depth_texture = member "GL_SGIX_depth_texture" extensions
{-# NOINLINE gl_SGIX_depth_texture #-}

pattern GL_DEPTH_COMPONENT16_SGIX  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT16_SGIX = 0x81A5

pattern GL_DEPTH_COMPONENT24_SGIX  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT24_SGIX = 0x81A6

pattern GL_DEPTH_COMPONENT32_SGIX  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT32_SGIX = 0x81A7