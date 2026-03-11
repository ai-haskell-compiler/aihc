-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ClearTexture (
  -- * Extension Support
    gl_EXT_clear_texture

  -- * GL_EXT_clear_texture
  , glClearTexImageEXT
  , glClearTexSubImageEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_clear_texture.txt GL_EXT_clear_texture> extension is available.

gl_EXT_clear_texture :: Bool
gl_EXT_clear_texture = member "GL_EXT_clear_texture" extensions
{-# NOINLINE gl_EXT_clear_texture #-}

-- | Usage: @'glClearTexImageEXT' texture level format type data@
--
-- The length of @data@ should be @COMPSIZE(format,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClearTexImage'.


glClearTexImageEXT :: MonadIO m => GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
glClearTexImageEXT = ffiuintintenumenumPtrVIOV glClearTexImageEXTFunPtr

glClearTexImageEXTFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
glClearTexImageEXTFunPtr = unsafePerformIO (getProcAddress "glClearTexImageEXT")

{-# NOINLINE glClearTexImageEXTFunPtr #-}

-- | Usage: @'glClearTexSubImageEXT' texture level xoffset yoffset zoffset width height depth format type data@
--
-- The length of @data@ should be @COMPSIZE(format,type)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClearTexSubImage'.


glClearTexSubImageEXT :: MonadIO m => GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glClearTexSubImageEXT = ffiuintintintintintsizeisizeisizeienumenumPtrVIOV glClearTexSubImageEXTFunPtr

glClearTexSubImageEXTFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glClearTexSubImageEXTFunPtr = unsafePerformIO (getProcAddress "glClearTexSubImageEXT")

{-# NOINLINE glClearTexSubImageEXTFunPtr #-}