-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderPrecision (
  -- * Extension Support
    gl_ARB_shader_precision
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_precision.txt GL_ARB_shader_precision> extension is available.

gl_ARB_shader_precision :: Bool
gl_ARB_shader_precision = member "GL_ARB_shader_precision" extensions
{-# NOINLINE gl_ARB_shader_precision #-}