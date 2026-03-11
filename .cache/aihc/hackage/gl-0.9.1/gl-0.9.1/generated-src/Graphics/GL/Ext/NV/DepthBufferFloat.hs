-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DepthBufferFloat (
  -- * Extension Support
    gl_NV_depth_buffer_float

  -- * GL_NV_depth_buffer_float
  , glClearDepthdNV
  , glDepthBoundsdNV
  , glDepthRangedNV
  , pattern GL_DEPTH32F_STENCIL8_NV
  , pattern GL_DEPTH_BUFFER_FLOAT_MODE_NV
  , pattern GL_DEPTH_COMPONENT32F_NV
  , pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/depth_buffer_float.txt GL_NV_depth_buffer_float> extension is available.

gl_NV_depth_buffer_float :: Bool
gl_NV_depth_buffer_float = member "GL_NV_depth_buffer_float" extensions
{-# NOINLINE gl_NV_depth_buffer_float #-}

-- | Usage: @'glClearDepthdNV' depth@


glClearDepthdNV :: MonadIO m => GLdouble -> m ()
glClearDepthdNV = ffidoubleIOV glClearDepthdNVFunPtr

glClearDepthdNVFunPtr :: FunPtr (GLdouble -> IO ())
glClearDepthdNVFunPtr = unsafePerformIO (getProcAddress "glClearDepthdNV")

{-# NOINLINE glClearDepthdNVFunPtr #-}

-- | Usage: @'glDepthBoundsdNV' zmin zmax@


glDepthBoundsdNV :: MonadIO m => GLdouble -> GLdouble -> m ()
glDepthBoundsdNV = ffidoubledoubleIOV glDepthBoundsdNVFunPtr

glDepthBoundsdNVFunPtr :: FunPtr (GLdouble -> GLdouble -> IO ())
glDepthBoundsdNVFunPtr = unsafePerformIO (getProcAddress "glDepthBoundsdNV")

{-# NOINLINE glDepthBoundsdNVFunPtr #-}

-- | Usage: @'glDepthRangedNV' zNear zFar@


glDepthRangedNV :: MonadIO m => GLdouble -> GLdouble -> m ()
glDepthRangedNV = ffidoubledoubleIOV glDepthRangedNVFunPtr

glDepthRangedNVFunPtr :: FunPtr (GLdouble -> GLdouble -> IO ())
glDepthRangedNVFunPtr = unsafePerformIO (getProcAddress "glDepthRangedNV")

{-# NOINLINE glDepthRangedNVFunPtr #-}

pattern GL_DEPTH32F_STENCIL8_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH32F_STENCIL8_NV = 0x8DAC

pattern GL_DEPTH_BUFFER_FLOAT_MODE_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_FLOAT_MODE_NV = 0x8DAF

pattern GL_DEPTH_COMPONENT32F_NV  :: (Eq a, Num a) => a

pattern GL_DEPTH_COMPONENT32F_NV = 0x8DAB

pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV = 0x8DAD