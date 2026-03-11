-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.MultiDrawIndirect (
  -- * Extension Support
    gl_ARB_multi_draw_indirect

  -- * GL_ARB_multi_draw_indirect
  , glMultiDrawArraysIndirect
  , glMultiDrawElementsIndirect
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/multi_draw_indirect.txt GL_ARB_multi_draw_indirect> extension is available.

gl_ARB_multi_draw_indirect :: Bool
gl_ARB_multi_draw_indirect = member "GL_ARB_multi_draw_indirect" extensions
{-# NOINLINE gl_ARB_multi_draw_indirect #-}