-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.MultiDrawIndirect (
  -- * Extension Support
    gl_AMD_multi_draw_indirect

  -- * GL_AMD_multi_draw_indirect
  , glMultiDrawArraysIndirectAMD
  , glMultiDrawElementsIndirectAMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/multi_draw_indirect.txt GL_AMD_multi_draw_indirect> extension is available.

gl_AMD_multi_draw_indirect :: Bool
gl_AMD_multi_draw_indirect = member "GL_AMD_multi_draw_indirect" extensions
{-# NOINLINE gl_AMD_multi_draw_indirect #-}

-- | Usage: @'glMultiDrawArraysIndirectAMD' mode indirect primcount stride@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawArraysIndirect'.


glMultiDrawArraysIndirectAMD :: MonadIO m => GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
glMultiDrawArraysIndirectAMD = ffienumPtrVsizeisizeiIOV glMultiDrawArraysIndirectAMDFunPtr

glMultiDrawArraysIndirectAMDFunPtr :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ())
glMultiDrawArraysIndirectAMDFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysIndirectAMD")

{-# NOINLINE glMultiDrawArraysIndirectAMDFunPtr #-}

-- | Usage: @'glMultiDrawElementsIndirectAMD' mode type indirect primcount stride@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawElementsIndirect'.


glMultiDrawElementsIndirectAMD :: MonadIO m => GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
glMultiDrawElementsIndirectAMD = ffienumenumPtrVsizeisizeiIOV glMultiDrawElementsIndirectAMDFunPtr

glMultiDrawElementsIndirectAMDFunPtr :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ())
glMultiDrawElementsIndirectAMDFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsIndirectAMD")

{-# NOINLINE glMultiDrawElementsIndirectAMDFunPtr #-}