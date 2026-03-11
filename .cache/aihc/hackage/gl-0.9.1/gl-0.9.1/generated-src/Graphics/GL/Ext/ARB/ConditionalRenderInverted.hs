-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ConditionalRenderInverted (
  -- * Extension Support
    gl_ARB_conditional_render_inverted

  -- * GL_ARB_conditional_render_inverted
  , pattern GL_QUERY_BY_REGION_NO_WAIT_INVERTED
  , pattern GL_QUERY_BY_REGION_WAIT_INVERTED
  , pattern GL_QUERY_NO_WAIT_INVERTED
  , pattern GL_QUERY_WAIT_INVERTED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/conditional_render_inverted.txt GL_ARB_conditional_render_inverted> extension is available.

gl_ARB_conditional_render_inverted :: Bool
gl_ARB_conditional_render_inverted = member "GL_ARB_conditional_render_inverted" extensions
{-# NOINLINE gl_ARB_conditional_render_inverted #-}