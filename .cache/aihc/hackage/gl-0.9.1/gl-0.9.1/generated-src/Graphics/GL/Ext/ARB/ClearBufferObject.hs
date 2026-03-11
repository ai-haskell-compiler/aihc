-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ClearBufferObject (
  -- * Extension Support
    gl_ARB_clear_buffer_object

  -- * GL_ARB_clear_buffer_object
  , glClearBufferData
  , glClearBufferSubData
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/clear_buffer_object.txt GL_ARB_clear_buffer_object> extension is available.

gl_ARB_clear_buffer_object :: Bool
gl_ARB_clear_buffer_object = member "GL_ARB_clear_buffer_object" extensions
{-# NOINLINE gl_ARB_clear_buffer_object #-}