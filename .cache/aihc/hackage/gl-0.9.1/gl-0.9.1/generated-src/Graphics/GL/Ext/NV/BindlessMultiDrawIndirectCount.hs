-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.BindlessMultiDrawIndirectCount (
  -- * Extension Support
    gl_NV_bindless_multi_draw_indirect_count

  -- * GL_NV_bindless_multi_draw_indirect_count
  , glMultiDrawArraysIndirectBindlessCountNV
  , glMultiDrawElementsIndirectBindlessCountNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/bindless_multi_draw_indirect_count.txt GL_NV_bindless_multi_draw_indirect_count> extension is available.

gl_NV_bindless_multi_draw_indirect_count :: Bool
gl_NV_bindless_multi_draw_indirect_count = member "GL_NV_bindless_multi_draw_indirect_count" extensions
{-# NOINLINE gl_NV_bindless_multi_draw_indirect_count #-}

-- | Usage: @'glMultiDrawArraysIndirectBindlessCountNV' mode indirect drawCount maxDrawCount stride vertexBufferCount@


glMultiDrawArraysIndirectBindlessCountNV :: MonadIO m => GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> m ()
glMultiDrawArraysIndirectBindlessCountNV = ffienumPtrVsizeisizeisizeiintIOV glMultiDrawArraysIndirectBindlessCountNVFunPtr

glMultiDrawArraysIndirectBindlessCountNVFunPtr :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
glMultiDrawArraysIndirectBindlessCountNVFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysIndirectBindlessCountNV")

{-# NOINLINE glMultiDrawArraysIndirectBindlessCountNVFunPtr #-}

-- | Usage: @'glMultiDrawElementsIndirectBindlessCountNV' mode type indirect drawCount maxDrawCount stride vertexBufferCount@


glMultiDrawElementsIndirectBindlessCountNV :: MonadIO m => GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> m ()
glMultiDrawElementsIndirectBindlessCountNV = ffienumenumPtrVsizeisizeisizeiintIOV glMultiDrawElementsIndirectBindlessCountNVFunPtr

glMultiDrawElementsIndirectBindlessCountNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
glMultiDrawElementsIndirectBindlessCountNVFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsIndirectBindlessCountNV")

{-# NOINLINE glMultiDrawElementsIndirectBindlessCountNVFunPtr #-}