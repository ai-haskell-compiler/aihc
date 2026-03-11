-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.MESA.YcbcrTexture (
  -- * Extension Support
    gl_MESA_ycbcr_texture

  -- * GL_MESA_ycbcr_texture
  , pattern GL_UNSIGNED_SHORT_8_8_MESA
  , pattern GL_UNSIGNED_SHORT_8_8_REV_MESA
  , pattern GL_YCBCR_MESA
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/MESA/ycbcr_texture.txt GL_MESA_ycbcr_texture> extension is available.

gl_MESA_ycbcr_texture :: Bool
gl_MESA_ycbcr_texture = member "GL_MESA_ycbcr_texture" extensions
{-# NOINLINE gl_MESA_ycbcr_texture #-}

pattern GL_UNSIGNED_SHORT_8_8_MESA  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_8_8_MESA = 0x85BA

pattern GL_UNSIGNED_SHORT_8_8_REV_MESA  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_8_8_REV_MESA = 0x85BB

pattern GL_YCBCR_MESA  :: (Eq a, Num a) => a

pattern GL_YCBCR_MESA = 0x8757