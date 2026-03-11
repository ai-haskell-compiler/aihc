-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TransformFeedbackInstanced (
  -- * Extension Support
    gl_ARB_transform_feedback_instanced

  -- * GL_ARB_transform_feedback_instanced
  , glDrawTransformFeedbackInstanced
  , glDrawTransformFeedbackStreamInstanced
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/transform_feedback_instanced.txt GL_ARB_transform_feedback_instanced> extension is available.

gl_ARB_transform_feedback_instanced :: Bool
gl_ARB_transform_feedback_instanced = member "GL_ARB_transform_feedback_instanced" extensions
{-# NOINLINE gl_ARB_transform_feedback_instanced #-}