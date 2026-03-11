-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.FragmentShaderOrdering (
  -- * Extension Support
    gl_INTEL_fragment_shader_ordering
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INTEL/fragment_shader_ordering.txt GL_INTEL_fragment_shader_ordering> extension is available.

gl_INTEL_fragment_shader_ordering :: Bool
gl_INTEL_fragment_shader_ordering = member "GL_INTEL_fragment_shader_ordering" extensions
{-# NOINLINE gl_INTEL_fragment_shader_ordering #-}