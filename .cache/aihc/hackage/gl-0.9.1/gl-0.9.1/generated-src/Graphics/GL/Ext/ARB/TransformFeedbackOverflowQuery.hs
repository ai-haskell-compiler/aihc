-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TransformFeedbackOverflowQuery (
  -- * Extension Support
    gl_ARB_transform_feedback_overflow_query

  -- * GL_ARB_transform_feedback_overflow_query
  , pattern GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB
  , pattern GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/transform_feedback_overflow_query.txt GL_ARB_transform_feedback_overflow_query> extension is available.

gl_ARB_transform_feedback_overflow_query :: Bool
gl_ARB_transform_feedback_overflow_query = member "GL_ARB_transform_feedback_overflow_query" extensions
{-# NOINLINE gl_ARB_transform_feedback_overflow_query #-}

pattern GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB = 0x82EC

pattern GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB = 0x82ED