-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.HalfFloatVertex (
  -- * Extension Support
    gl_ARB_half_float_vertex

  -- * GL_ARB_half_float_vertex
  , pattern GL_HALF_FLOAT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/half_float_vertex.txt GL_ARB_half_float_vertex> extension is available.

gl_ARB_half_float_vertex :: Bool
gl_ARB_half_float_vertex = member "GL_ARB_half_float_vertex" extensions
{-# NOINLINE gl_ARB_half_float_vertex #-}