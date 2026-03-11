-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.MultiBind (
  -- * Extension Support
    gl_ARB_multi_bind

  -- * GL_ARB_multi_bind
  , glBindBuffersBase
  , glBindBuffersRange
  , glBindImageTextures
  , glBindSamplers
  , glBindTextures
  , glBindVertexBuffers
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/multi_bind.txt GL_ARB_multi_bind> extension is available.

gl_ARB_multi_bind :: Bool
gl_ARB_multi_bind = member "GL_ARB_multi_bind" extensions
{-# NOINLINE gl_ARB_multi_bind #-}