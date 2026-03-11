-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSharedExponent (
  -- * Extension Support
    gl_EXT_texture_shared_exponent

  -- * GL_EXT_texture_shared_exponent
  , pattern GL_RGB9_E5_EXT
  , pattern GL_TEXTURE_SHARED_SIZE_EXT
  , pattern GL_UNSIGNED_INT_5_9_9_9_REV_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_shared_exponent.txt GL_EXT_texture_shared_exponent> extension is available.

gl_EXT_texture_shared_exponent :: Bool
gl_EXT_texture_shared_exponent = member "GL_EXT_texture_shared_exponent" extensions
{-# NOINLINE gl_EXT_texture_shared_exponent #-}

pattern GL_RGB9_E5_EXT  :: (Eq a, Num a) => a

pattern GL_RGB9_E5_EXT = 0x8C3D

pattern GL_TEXTURE_SHARED_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SHARED_SIZE_EXT = 0x8C3F

pattern GL_UNSIGNED_INT_5_9_9_9_REV_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_5_9_9_9_REV_EXT = 0x8C3E