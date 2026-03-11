-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SamplerObjects (
  -- * Extension Support
    gl_ARB_sampler_objects

  -- * GL_ARB_sampler_objects
  , glBindSampler
  , glDeleteSamplers
  , glGenSamplers
  , glGetSamplerParameterIiv
  , glGetSamplerParameterIuiv
  , glGetSamplerParameterfv
  , glGetSamplerParameteriv
  , glIsSampler
  , glSamplerParameterIiv
  , glSamplerParameterIuiv
  , glSamplerParameterf
  , glSamplerParameterfv
  , glSamplerParameteri
  , glSamplerParameteriv
  , pattern GL_SAMPLER_BINDING
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sampler_objects.txt GL_ARB_sampler_objects> extension is available.

gl_ARB_sampler_objects :: Bool
gl_ARB_sampler_objects = member "GL_ARB_sampler_objects" extensions
{-# NOINLINE gl_ARB_sampler_objects #-}