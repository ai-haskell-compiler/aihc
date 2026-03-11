-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureMultisample (
  -- * Extension Support
    gl_ARB_texture_multisample

  -- * GL_ARB_texture_multisample
  , glGetMultisamplefv
  , glSampleMaski
  , glTexImage2DMultisample
  , glTexImage3DMultisample
  , pattern GL_INT_SAMPLER_2D_MULTISAMPLE
  , pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
  , pattern GL_MAX_COLOR_TEXTURE_SAMPLES
  , pattern GL_MAX_DEPTH_TEXTURE_SAMPLES
  , pattern GL_MAX_INTEGER_SAMPLES
  , pattern GL_MAX_SAMPLE_MASK_WORDS
  , pattern GL_PROXY_TEXTURE_2D_MULTISAMPLE
  , pattern GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
  , pattern GL_SAMPLER_2D_MULTISAMPLE
  , pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY
  , pattern GL_SAMPLE_MASK
  , pattern GL_SAMPLE_MASK_VALUE
  , pattern GL_SAMPLE_POSITION
  , pattern GL_TEXTURE_2D_MULTISAMPLE
  , pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE
  , pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY
  , pattern GL_TEXTURE_FIXED_SAMPLE_LOCATIONS
  , pattern GL_TEXTURE_SAMPLES
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_multisample.txt GL_ARB_texture_multisample> extension is available.

gl_ARB_texture_multisample :: Bool
gl_ARB_texture_multisample = member "GL_ARB_texture_multisample" extensions
{-# NOINLINE gl_ARB_texture_multisample #-}