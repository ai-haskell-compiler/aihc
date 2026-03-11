-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.Fence (
  -- * Extension Support
    gl_APPLE_fence

  -- * GL_APPLE_fence
  , glDeleteFencesAPPLE
  , glFinishFenceAPPLE
  , glFinishObjectAPPLE
  , glGenFencesAPPLE
  , glIsFenceAPPLE
  , glSetFenceAPPLE
  , glTestFenceAPPLE
  , glTestObjectAPPLE
  , pattern GL_DRAW_PIXELS_APPLE
  , pattern GL_FENCE_APPLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/fence.txt GL_APPLE_fence> extension is available.

gl_APPLE_fence :: Bool
gl_APPLE_fence = member "GL_APPLE_fence" extensions
{-# NOINLINE gl_APPLE_fence #-}

-- | Usage: @'glDeleteFencesAPPLE' n fences@
--
-- The parameter @fences@ is a @FenceNV@.
--
-- The length of @fences@ should be @n@.


glDeleteFencesAPPLE :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteFencesAPPLE = ffisizeiPtruintIOV glDeleteFencesAPPLEFunPtr

glDeleteFencesAPPLEFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteFencesAPPLEFunPtr = unsafePerformIO (getProcAddress "glDeleteFencesAPPLE")

{-# NOINLINE glDeleteFencesAPPLEFunPtr #-}

-- | Usage: @'glFinishFenceAPPLE' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glFinishFenceAPPLE :: MonadIO m => GLuint -> m ()
glFinishFenceAPPLE = ffiuintIOV glFinishFenceAPPLEFunPtr

glFinishFenceAPPLEFunPtr :: FunPtr (GLuint -> IO ())
glFinishFenceAPPLEFunPtr = unsafePerformIO (getProcAddress "glFinishFenceAPPLE")

{-# NOINLINE glFinishFenceAPPLEFunPtr #-}

-- | Usage: @'glFinishObjectAPPLE' object name@
--
-- The parameter @object@ is a @ObjectTypeAPPLE@.


glFinishObjectAPPLE :: MonadIO m => GLenum -> GLint -> m ()
glFinishObjectAPPLE = ffienumintIOV glFinishObjectAPPLEFunPtr

glFinishObjectAPPLEFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glFinishObjectAPPLEFunPtr = unsafePerformIO (getProcAddress "glFinishObjectAPPLE")

{-# NOINLINE glFinishObjectAPPLEFunPtr #-}

-- | Usage: @'glGenFencesAPPLE' n fences@
--
-- The parameter @fences@ is a @FenceNV@.
--
-- The length of @fences@ should be @n@.


glGenFencesAPPLE :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenFencesAPPLE = ffisizeiPtruintIOV glGenFencesAPPLEFunPtr

glGenFencesAPPLEFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenFencesAPPLEFunPtr = unsafePerformIO (getProcAddress "glGenFencesAPPLE")

{-# NOINLINE glGenFencesAPPLEFunPtr #-}

-- | Usage: @'glIsFenceAPPLE' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glIsFenceAPPLE :: MonadIO m => GLuint -> m GLboolean
glIsFenceAPPLE = ffiuintIOboolean glIsFenceAPPLEFunPtr

glIsFenceAPPLEFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsFenceAPPLEFunPtr = unsafePerformIO (getProcAddress "glIsFenceAPPLE")

{-# NOINLINE glIsFenceAPPLEFunPtr #-}

-- | Usage: @'glSetFenceAPPLE' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glSetFenceAPPLE :: MonadIO m => GLuint -> m ()
glSetFenceAPPLE = ffiuintIOV glSetFenceAPPLEFunPtr

glSetFenceAPPLEFunPtr :: FunPtr (GLuint -> IO ())
glSetFenceAPPLEFunPtr = unsafePerformIO (getProcAddress "glSetFenceAPPLE")

{-# NOINLINE glSetFenceAPPLEFunPtr #-}

-- | Usage: @'glTestFenceAPPLE' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glTestFenceAPPLE :: MonadIO m => GLuint -> m GLboolean
glTestFenceAPPLE = ffiuintIOboolean glTestFenceAPPLEFunPtr

glTestFenceAPPLEFunPtr :: FunPtr (GLuint -> IO GLboolean)
glTestFenceAPPLEFunPtr = unsafePerformIO (getProcAddress "glTestFenceAPPLE")

{-# NOINLINE glTestFenceAPPLEFunPtr #-}

-- | Usage: @'glTestObjectAPPLE' object name@
--
-- The parameter @object@ is a @ObjectTypeAPPLE@.


glTestObjectAPPLE :: MonadIO m => GLenum -> GLuint -> m GLboolean
glTestObjectAPPLE = ffienumuintIOboolean glTestObjectAPPLEFunPtr

glTestObjectAPPLEFunPtr :: FunPtr (GLenum -> GLuint -> IO GLboolean)
glTestObjectAPPLEFunPtr = unsafePerformIO (getProcAddress "glTestObjectAPPLE")

{-# NOINLINE glTestObjectAPPLEFunPtr #-}

pattern GL_DRAW_PIXELS_APPLE  :: (Eq a, Num a) => a

pattern GL_DRAW_PIXELS_APPLE = 0x8A0A

pattern GL_FENCE_APPLE  :: (Eq a, Num a) => a

pattern GL_FENCE_APPLE = 0x8A0B