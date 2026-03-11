-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ViewportArray (
  -- * Extension Support
    gl_ARB_viewport_array

  -- * GL_ARB_viewport_array
  , glDepthRangeArrayv
  , glDepthRangeIndexed
  , glGetDoublei_v
  , glGetFloati_v
  , glScissorArrayv
  , glScissorIndexed
  , glScissorIndexedv
  , glViewportArrayv
  , glViewportIndexedf
  , glViewportIndexedfv
  , pattern GL_DEPTH_RANGE
  , pattern GL_FIRST_VERTEX_CONVENTION
  , pattern GL_LAST_VERTEX_CONVENTION
  , pattern GL_LAYER_PROVOKING_VERTEX
  , pattern GL_MAX_VIEWPORTS
  , pattern GL_PROVOKING_VERTEX
  , pattern GL_SCISSOR_BOX
  , pattern GL_SCISSOR_TEST
  , pattern GL_UNDEFINED_VERTEX
  , pattern GL_VIEWPORT
  , pattern GL_VIEWPORT_BOUNDS_RANGE
  , pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX
  , pattern GL_VIEWPORT_SUBPIXEL_BITS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/viewport_array.txt GL_ARB_viewport_array> extension is available.

gl_ARB_viewport_array :: Bool
gl_ARB_viewport_array = member "GL_ARB_viewport_array" extensions
{-# NOINLINE gl_ARB_viewport_array #-}