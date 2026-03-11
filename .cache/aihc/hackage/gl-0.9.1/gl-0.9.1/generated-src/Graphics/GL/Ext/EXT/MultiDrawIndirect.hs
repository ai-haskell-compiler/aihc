-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MultiDrawIndirect (
  -- * Extension Support
    gl_EXT_multi_draw_indirect

  -- * GL_EXT_multi_draw_indirect
  , glMultiDrawArraysIndirectEXT
  , glMultiDrawElementsIndirectEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_EXT_multi_draw_indirect extension is available.

gl_EXT_multi_draw_indirect :: Bool
gl_EXT_multi_draw_indirect = member "GL_EXT_multi_draw_indirect" extensions
{-# NOINLINE gl_EXT_multi_draw_indirect #-}

-- | Usage: @'glMultiDrawArraysIndirectEXT' mode indirect drawcount stride@
--
-- The length of @indirect@ should be @COMPSIZE(drawcount,stride)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawArraysIndirect'.


glMultiDrawArraysIndirectEXT :: MonadIO m => GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
glMultiDrawArraysIndirectEXT = ffienumPtrVsizeisizeiIOV glMultiDrawArraysIndirectEXTFunPtr

glMultiDrawArraysIndirectEXTFunPtr :: FunPtr (GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ())
glMultiDrawArraysIndirectEXTFunPtr = unsafePerformIO (getProcAddress "glMultiDrawArraysIndirectEXT")

{-# NOINLINE glMultiDrawArraysIndirectEXTFunPtr #-}

-- | Usage: @'glMultiDrawElementsIndirectEXT' mode type indirect drawcount stride@
--
-- The length of @indirect@ should be @COMPSIZE(drawcount,stride)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMultiDrawElementsIndirect'.


glMultiDrawElementsIndirectEXT :: MonadIO m => GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> m ()
glMultiDrawElementsIndirectEXT = ffienumenumPtrVsizeisizeiIOV glMultiDrawElementsIndirectEXTFunPtr

glMultiDrawElementsIndirectEXTFunPtr :: FunPtr (GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> IO ())
glMultiDrawElementsIndirectEXTFunPtr = unsafePerformIO (getProcAddress "glMultiDrawElementsIndirectEXT")

{-# NOINLINE glMultiDrawElementsIndirectEXTFunPtr #-}