-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.CopyImage (
  -- * Extension Support
    gl_OES_copy_image

  -- * GL_OES_copy_image
  , glCopyImageSubDataOES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_copy_image.txt GL_OES_copy_image> extension is available.

gl_OES_copy_image :: Bool
gl_OES_copy_image = member "GL_OES_copy_image" extensions
{-# NOINLINE gl_OES_copy_image #-}

-- | Usage: @'glCopyImageSubDataOES' srcName srcTarget srcLevel srcX srcY srcZ dstName dstTarget dstLevel dstX dstY dstZ srcWidth srcHeight srcDepth@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyImageSubData'.


glCopyImageSubDataOES :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> m ()
glCopyImageSubDataOES = ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV glCopyImageSubDataOESFunPtr

glCopyImageSubDataOESFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
glCopyImageSubDataOESFunPtr = unsafePerformIO (getProcAddress "glCopyImageSubDataOES")

{-# NOINLINE glCopyImageSubDataOESFunPtr #-}