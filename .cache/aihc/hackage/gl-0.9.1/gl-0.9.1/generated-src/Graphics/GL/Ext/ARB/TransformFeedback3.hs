-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TransformFeedback3 (
  -- * Extension Support
    gl_ARB_transform_feedback3

  -- * GL_ARB_transform_feedback3
  , glBeginQueryIndexed
  , glDrawTransformFeedbackStream
  , glEndQueryIndexed
  , glGetQueryIndexediv
  , pattern GL_MAX_TRANSFORM_FEEDBACK_BUFFERS
  , pattern GL_MAX_VERTEX_STREAMS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/transform_feedback3.txt GL_ARB_transform_feedback3> extension is available.

gl_ARB_transform_feedback3 :: Bool
gl_ARB_transform_feedback3 = member "GL_ARB_transform_feedback3" extensions
{-# NOINLINE gl_ARB_transform_feedback3 #-}