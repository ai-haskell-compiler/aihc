-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.InvalidateSubdata (
  -- * Extension Support
    gl_ARB_invalidate_subdata

  -- * GL_ARB_invalidate_subdata
  , glInvalidateBufferData
  , glInvalidateBufferSubData
  , glInvalidateFramebuffer
  , glInvalidateSubFramebuffer
  , glInvalidateTexImage
  , glInvalidateTexSubImage
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/invalidate_subdata.txt GL_ARB_invalidate_subdata> extension is available.

gl_ARB_invalidate_subdata :: Bool
gl_ARB_invalidate_subdata = member "GL_ARB_invalidate_subdata" extensions
{-# NOINLINE gl_ARB_invalidate_subdata #-}