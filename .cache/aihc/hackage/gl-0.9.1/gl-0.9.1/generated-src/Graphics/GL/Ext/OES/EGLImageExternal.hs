-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.EGLImageExternal (
  -- * Extension Support
    gl_OES_EGL_image_external

  -- * GL_OES_EGL_image_external
  , pattern GL_REQUIRED_TEXTURE_IMAGE_UNITS_OES
  , pattern GL_SAMPLER_EXTERNAL_OES
  , pattern GL_TEXTURE_BINDING_EXTERNAL_OES
  , pattern GL_TEXTURE_EXTERNAL_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_EGL_image_external.txt GL_OES_EGL_image_external> extension is available.

gl_OES_EGL_image_external :: Bool
gl_OES_EGL_image_external = member "GL_OES_EGL_image_external" extensions
{-# NOINLINE gl_OES_EGL_image_external #-}

pattern GL_SAMPLER_EXTERNAL_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLER_EXTERNAL_OES = 0x8D66