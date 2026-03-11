-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TransformFeedback2 (
  -- * Extension Support
    gl_ARB_transform_feedback2

  -- * GL_ARB_transform_feedback2
  , glBindTransformFeedback
  , glDeleteTransformFeedbacks
  , glDrawTransformFeedback
  , glGenTransformFeedbacks
  , glIsTransformFeedback
  , glPauseTransformFeedback
  , glResumeTransformFeedback
  , pattern GL_TRANSFORM_FEEDBACK
  , pattern GL_TRANSFORM_FEEDBACK_BINDING
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/transform_feedback2.txt GL_ARB_transform_feedback2> extension is available.

gl_ARB_transform_feedback2 :: Bool
gl_ARB_transform_feedback2 = member "GL_ARB_transform_feedback2" extensions
{-# NOINLINE gl_ARB_transform_feedback2 #-}