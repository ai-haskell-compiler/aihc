-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ViewportSwizzle (
  -- * Extension Support
    gl_NV_viewport_swizzle

  -- * GL_NV_viewport_swizzle
  , glViewportSwizzleNV
  , pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV
  , pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV
  , pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV
  , pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV
  , pattern GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV
  , pattern GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV
  , pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV
  , pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV
  , pattern GL_VIEWPORT_SWIZZLE_W_NV
  , pattern GL_VIEWPORT_SWIZZLE_X_NV
  , pattern GL_VIEWPORT_SWIZZLE_Y_NV
  , pattern GL_VIEWPORT_SWIZZLE_Z_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/viewport_swizzle.txt GL_NV_viewport_swizzle> extension is available.

gl_NV_viewport_swizzle :: Bool
gl_NV_viewport_swizzle = member "GL_NV_viewport_swizzle" extensions
{-# NOINLINE gl_NV_viewport_swizzle #-}

-- | Usage: @'glViewportSwizzleNV' index swizzlex swizzley swizzlez swizzlew@


glViewportSwizzleNV :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glViewportSwizzleNV = ffiuintenumenumenumenumIOV glViewportSwizzleNVFunPtr

glViewportSwizzleNVFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glViewportSwizzleNVFunPtr = unsafePerformIO (getProcAddress "glViewportSwizzleNV")

{-# NOINLINE glViewportSwizzleNVFunPtr #-}

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV = 0x9357

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV = 0x9351

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV = 0x9353

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV = 0x9355

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV = 0x9356

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV = 0x9350

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV = 0x9352

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV = 0x9354

pattern GL_VIEWPORT_SWIZZLE_W_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_W_NV = 0x935B

pattern GL_VIEWPORT_SWIZZLE_X_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_X_NV = 0x9358

pattern GL_VIEWPORT_SWIZZLE_Y_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_Y_NV = 0x9359

pattern GL_VIEWPORT_SWIZZLE_Z_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SWIZZLE_Z_NV = 0x935A