-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DrawIndirect (
  -- * Extension Support
    gl_ARB_draw_indirect

  -- * GL_ARB_draw_indirect
  , glDrawArraysIndirect
  , glDrawElementsIndirect
  , pattern GL_DRAW_INDIRECT_BUFFER
  , pattern GL_DRAW_INDIRECT_BUFFER_BINDING
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/draw_indirect.txt GL_ARB_draw_indirect> extension is available.

gl_ARB_draw_indirect :: Bool
gl_ARB_draw_indirect = member "GL_ARB_draw_indirect" extensions
{-# NOINLINE gl_ARB_draw_indirect #-}