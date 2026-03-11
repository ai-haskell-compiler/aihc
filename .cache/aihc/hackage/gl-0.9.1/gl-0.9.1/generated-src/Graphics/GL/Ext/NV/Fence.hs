-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.Fence (
  -- * Extension Support
    gl_NV_fence

  -- * GL_NV_fence
  , glDeleteFencesNV
  , glFinishFenceNV
  , glGenFencesNV
  , glGetFenceivNV
  , glIsFenceNV
  , glSetFenceNV
  , glTestFenceNV
  , pattern GL_ALL_COMPLETED_NV
  , pattern GL_FENCE_CONDITION_NV
  , pattern GL_FENCE_STATUS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_fence.txt GL_NV_fence> extension is available.

gl_NV_fence :: Bool
gl_NV_fence = member "GL_NV_fence" extensions
{-# NOINLINE gl_NV_fence #-}

-- | Usage: @'glDeleteFencesNV' n fences@
--
-- The parameter @fences@ is a @FenceNV@.
--
-- The length of @fences@ should be @n@.


glDeleteFencesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteFencesNV = ffisizeiPtruintIOV glDeleteFencesNVFunPtr

glDeleteFencesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteFencesNVFunPtr = unsafePerformIO (getProcAddress "glDeleteFencesNV")

{-# NOINLINE glDeleteFencesNVFunPtr #-}

-- | Usage: @'glFinishFenceNV' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glFinishFenceNV :: MonadIO m => GLuint -> m ()
glFinishFenceNV = ffiuintIOV glFinishFenceNVFunPtr

glFinishFenceNVFunPtr :: FunPtr (GLuint -> IO ())
glFinishFenceNVFunPtr = unsafePerformIO (getProcAddress "glFinishFenceNV")

{-# NOINLINE glFinishFenceNVFunPtr #-}

-- | Usage: @'glGenFencesNV' n fences@
--
-- The parameter @fences@ is a @FenceNV@.
--
-- The length of @fences@ should be @n@.


glGenFencesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenFencesNV = ffisizeiPtruintIOV glGenFencesNVFunPtr

glGenFencesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenFencesNVFunPtr = unsafePerformIO (getProcAddress "glGenFencesNV")

{-# NOINLINE glGenFencesNVFunPtr #-}

-- | Usage: @'glGetFenceivNV' fence pname params@
--
-- The parameter @fence@ is a @FenceNV@.
--
-- The parameter @pname@ is a @FenceParameterNameNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFenceivNV :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetFenceivNV = ffiuintenumPtrintIOV glGetFenceivNVFunPtr

glGetFenceivNVFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetFenceivNVFunPtr = unsafePerformIO (getProcAddress "glGetFenceivNV")

{-# NOINLINE glGetFenceivNVFunPtr #-}

-- | Usage: @'glIsFenceNV' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glIsFenceNV :: MonadIO m => GLuint -> m GLboolean
glIsFenceNV = ffiuintIOboolean glIsFenceNVFunPtr

glIsFenceNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsFenceNVFunPtr = unsafePerformIO (getProcAddress "glIsFenceNV")

{-# NOINLINE glIsFenceNVFunPtr #-}

-- | Usage: @'glSetFenceNV' fence condition@
--
-- The parameter @fence@ is a @FenceNV@.
--
-- The parameter @condition@ is a @FenceConditionNV@.


glSetFenceNV :: MonadIO m => GLuint -> GLenum -> m ()
glSetFenceNV = ffiuintenumIOV glSetFenceNVFunPtr

glSetFenceNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glSetFenceNVFunPtr = unsafePerformIO (getProcAddress "glSetFenceNV")

{-# NOINLINE glSetFenceNVFunPtr #-}

-- | Usage: @'glTestFenceNV' fence@
--
-- The parameter @fence@ is a @FenceNV@.


glTestFenceNV :: MonadIO m => GLuint -> m GLboolean
glTestFenceNV = ffiuintIOboolean glTestFenceNVFunPtr

glTestFenceNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glTestFenceNVFunPtr = unsafePerformIO (getProcAddress "glTestFenceNV")

{-# NOINLINE glTestFenceNVFunPtr #-}

pattern GL_ALL_COMPLETED_NV  :: (Eq a, Num a) => a

pattern GL_ALL_COMPLETED_NV = 0x84F2

pattern GL_FENCE_CONDITION_NV  :: (Eq a, Num a) => a

pattern GL_FENCE_CONDITION_NV = 0x84F4

pattern GL_FENCE_STATUS_NV  :: (Eq a, Num a) => a

pattern GL_FENCE_STATUS_NV = 0x84F3