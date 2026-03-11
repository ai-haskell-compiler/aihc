-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FragmentShaderInterlock (
  -- * Extension Support
    gl_ARB_fragment_shader_interlock
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/fragment_shader_interlock.txt GL_ARB_fragment_shader_interlock> extension is available.

gl_ARB_fragment_shader_interlock :: Bool
gl_ARB_fragment_shader_interlock = member "GL_ARB_fragment_shader_interlock" extensions
{-# NOINLINE gl_ARB_fragment_shader_interlock #-}