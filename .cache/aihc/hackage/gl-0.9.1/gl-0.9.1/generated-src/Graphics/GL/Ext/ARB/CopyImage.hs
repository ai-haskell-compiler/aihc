-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.CopyImage (
  -- * Extension Support
    gl_ARB_copy_image

  -- * GL_ARB_copy_image
  , glCopyImageSubData
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/copy_image.txt GL_ARB_copy_image> extension is available.

gl_ARB_copy_image :: Bool
gl_ARB_copy_image = member "GL_ARB_copy_image" extensions
{-# NOINLINE gl_ARB_copy_image #-}