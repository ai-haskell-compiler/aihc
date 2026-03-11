-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.EnhancedLayouts (
  -- * Extension Support
    gl_ARB_enhanced_layouts

  -- * GL_ARB_enhanced_layouts
  , pattern GL_LOCATION_COMPONENT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/enhanced_layouts.txt GL_ARB_enhanced_layouts> extension is available.

gl_ARB_enhanced_layouts :: Bool
gl_ARB_enhanced_layouts = member "GL_ARB_enhanced_layouts" extensions
{-# NOINLINE gl_ARB_enhanced_layouts #-}