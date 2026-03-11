-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.CopyImage (
  -- * Extension Support
    gl_EXT_copy_image

  -- * GL_EXT_copy_image
  , glCopyImageSubDataEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_copy_image.txt GL_EXT_copy_image> extension is available.

gl_EXT_copy_image :: Bool
gl_EXT_copy_image = member "GL_EXT_copy_image" extensions
{-# NOINLINE gl_EXT_copy_image #-}

-- | Usage: @'glCopyImageSubDataEXT' srcName srcTarget srcLevel srcX srcY srcZ dstName dstTarget dstLevel dstX dstY dstZ srcWidth srcHeight srcDepth@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glCopyImageSubData'.


glCopyImageSubDataEXT :: MonadIO m => GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> m ()
glCopyImageSubDataEXT = ffiuintenumintintintintuintenumintintintintsizeisizeisizeiIOV glCopyImageSubDataEXTFunPtr

glCopyImageSubDataEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
glCopyImageSubDataEXTFunPtr = unsafePerformIO (getProcAddress "glCopyImageSubDataEXT")

{-# NOINLINE glCopyImageSubDataEXTFunPtr #-}