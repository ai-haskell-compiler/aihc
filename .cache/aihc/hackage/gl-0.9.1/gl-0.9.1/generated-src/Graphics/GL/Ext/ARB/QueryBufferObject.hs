-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.QueryBufferObject (
  -- * Extension Support
    gl_ARB_query_buffer_object

  -- * GL_ARB_query_buffer_object
  , pattern GL_QUERY_BUFFER
  , pattern GL_QUERY_BUFFER_BARRIER_BIT
  , pattern GL_QUERY_BUFFER_BINDING
  , pattern GL_QUERY_RESULT_NO_WAIT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/query_buffer_object.txt GL_ARB_query_buffer_object> extension is available.

gl_ARB_query_buffer_object :: Bool
gl_ARB_query_buffer_object = member "GL_ARB_query_buffer_object" extensions
{-# NOINLINE gl_ARB_query_buffer_object #-}