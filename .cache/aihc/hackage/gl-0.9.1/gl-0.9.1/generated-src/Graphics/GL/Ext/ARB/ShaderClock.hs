-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderClock (
  -- * Extension Support
    gl_ARB_shader_clock
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_clock.txt GL_ARB_shader_clock> extension is available.

gl_ARB_shader_clock :: Bool
gl_ARB_shader_clock = member "GL_ARB_shader_clock" extensions
{-# NOINLINE gl_ARB_shader_clock #-}