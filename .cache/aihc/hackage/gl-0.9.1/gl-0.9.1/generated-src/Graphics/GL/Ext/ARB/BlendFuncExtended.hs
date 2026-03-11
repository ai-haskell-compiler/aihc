-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.BlendFuncExtended (
  -- * Extension Support
    gl_ARB_blend_func_extended

  -- * GL_ARB_blend_func_extended
  , glBindFragDataLocationIndexed
  , glGetFragDataIndex
  , pattern GL_MAX_DUAL_SOURCE_DRAW_BUFFERS
  , pattern GL_ONE_MINUS_SRC1_ALPHA
  , pattern GL_ONE_MINUS_SRC1_COLOR
  , pattern GL_SRC1_ALPHA
  , pattern GL_SRC1_COLOR
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/blend_func_extended.txt GL_ARB_blend_func_extended> extension is available.

gl_ARB_blend_func_extended :: Bool
gl_ARB_blend_func_extended = member "GL_ARB_blend_func_extended" extensions
{-# NOINLINE gl_ARB_blend_func_extended #-}