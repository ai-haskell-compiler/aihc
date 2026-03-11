-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.BindlessMultiDrawIndirect (
  -- * Extension Support
    gl_NV_bindless_multi_draw_indirect

  -- * GL_NV_bindless_multi_draw_indirect
  , glMultiDrawArraysIndirectBindlessNV
  , glMultiDrawElementsIndirectBindlessNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/bindless_multi_draw_indirect.txt GL_NV_bindless_multi_draw_indirect> extension is available.

gl_NV_bindless_multi_draw_indirect :: Bool
gl_NV_bindless_multi_draw_indirect = member "GL_NV_bindless_multi_draw_indirect" extensions
{-# NOINLINE gl_NV_bindless_multi_draw_indirect #-}

-- | Usage: @'glMultiDrawArraysIndirectBindlessNV' mode indirect drawCount stride vertexBufferCount@


glMultiDrawArraysIndirectBindlessNV :: MonadIO m => GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> m ()
glMultiDrawArraysIndirectBindlessNV = ffienumPtrVsizeisizeiintIOV glMultiDrawArraysIndirectBindlessNVFunPtr

glMultiDrawArraysIndirectBindlessNVFunPtr :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ())
glMultiDrawArraysIndirectBindlessNVFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysIndirectBindlessNV")

{-# NOINLINE glMultiDrawArraysIndirectBindlessNVFunPtr #-}

-- | Usage: @'glMultiDrawElementsIndirectBindlessNV' mode type indirect drawCount stride vertexBufferCount@


glMultiDrawElementsIndirectBindlessNV :: MonadIO m => GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> m ()
glMultiDrawElementsIndirectBindlessNV = ffienumenumPtrVsizeisizeiintIOV glMultiDrawElementsIndirectBindlessNVFunPtr

glMultiDrawElementsIndirectBindlessNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GLint -> IO ())
glMultiDrawElementsIndirectBindlessNVFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsIndirectBindlessNV")

{-# NOINLINE glMultiDrawElementsIndirectBindlessNVFunPtr #-}