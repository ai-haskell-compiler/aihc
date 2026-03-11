-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.CopyBuffer (
  -- * Extension Support
    gl_ARB_copy_buffer

  -- * GL_ARB_copy_buffer
  , glCopyBufferSubData
  , pattern GL_COPY_READ_BUFFER
  , pattern GL_COPY_WRITE_BUFFER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/copy_buffer.txt GL_ARB_copy_buffer> extension is available.

gl_ARB_copy_buffer :: Bool
gl_ARB_copy_buffer = member "GL_ARB_copy_buffer" extensions
{-# NOINLINE gl_ARB_copy_buffer #-}