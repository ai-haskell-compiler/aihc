-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DepthTexture (
  -- * Extension Support
    gl_ARB_depth_texture

  -- * GL_ARB_depth_texture
  , pattern GL_DEPTH_COMPONENT16_ARB
  , pattern GL_DEPTH_COMPONENT24_ARB
  , pattern GL_DEPTH_COMPONENT32_ARB
  , pattern GL_DEPTH_TEXTURE_MODE_ARB
  , pattern GL_TEXTURE_DEPTH_SIZE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/depth_texture.txt GL_ARB_depth_texture> extension is available.

gl_ARB_depth_texture :: Bool
gl_ARB_depth_texture = member "GL_ARB_depth_texture" extensions
{-# NOINLINE gl_ARB_depth_texture #-}

pattern GL_DEPTH_COMPONENT16_ARB  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT16_ARB = 0x81A5

pattern GL_DEPTH_COMPONENT24_ARB  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT24_ARB = 0x81A6

pattern GL_DEPTH_COMPONENT32_ARB  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT32_ARB = 0x81A7

pattern GL_DEPTH_TEXTURE_MODE_ARB  :: (Eq a, Num a) => a

pattern GL_DEPTH_TEXTURE_MODE_ARB = 0x884B

pattern GL_TEXTURE_DEPTH_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DEPTH_SIZE_ARB = 0x884A