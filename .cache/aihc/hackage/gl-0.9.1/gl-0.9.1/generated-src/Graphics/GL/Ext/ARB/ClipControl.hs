-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ClipControl (
  -- * Extension Support
    gl_ARB_clip_control

  -- * GL_ARB_clip_control
  , glClipControl
  , pattern GL_CLIP_DEPTH_MODE
  , pattern GL_CLIP_ORIGIN
  , pattern GL_LOWER_LEFT
  , pattern GL_NEGATIVE_ONE_TO_ONE
  , pattern GL_UPPER_LEFT
  , pattern GL_ZERO_TO_ONE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/clip_control.txt GL_ARB_clip_control> extension is available.

gl_ARB_clip_control :: Bool
gl_ARB_clip_control = member "GL_ARB_clip_control" extensions
{-# NOINLINE gl_ARB_clip_control #-}