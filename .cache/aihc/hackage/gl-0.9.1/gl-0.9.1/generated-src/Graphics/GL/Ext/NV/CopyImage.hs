-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.CopyImage (
  -- * Extension Support
    gl_NV_copy_image

  -- * GL_NV_copy_image
  , glCopyImageSubDataNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/copy_image.txt GL_NV_copy_image> extension is available.

gl_NV_copy_image :: Bool
gl_NV_copy_image = member "GL_NV_copy_image" extensions
{-# NOINLINE gl_NV_copy_image #-}

-- | Usage: @'glCopyImageSubDataNV' srcName srcTarget srcLevel srcX srcY srcZ dstName dstTarget dstLevel dstX dstY dstZ width height depth@


glCopyImageSubDataNV :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> m ()
glCopyImageSubDataNV = ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV glCopyImageSubDataNVFunPtr

glCopyImageSubDataNVFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
glCopyImageSubDataNVFunPtr = unsafePerformIO (getProcAddress "glCopyImageSubDataNV")

{-# NOINLINE glCopyImageSubDataNVFunPtr #-}