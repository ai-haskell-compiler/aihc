-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DepthBufferFloat (
  -- * Extension Support
    gl_ARB_depth_buffer_float

  -- * GL_ARB_depth_buffer_float
  , pattern GL_DEPTH32F_STENCIL8
  , pattern GL_DEPTH_COMPONENT32F
  , pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/depth_buffer_float.txt GL_ARB_depth_buffer_float> extension is available.

gl_ARB_depth_buffer_float :: Bool
gl_ARB_depth_buffer_float = member "GL_ARB_depth_buffer_float" extensions
{-# NOINLINE gl_ARB_depth_buffer_float #-}