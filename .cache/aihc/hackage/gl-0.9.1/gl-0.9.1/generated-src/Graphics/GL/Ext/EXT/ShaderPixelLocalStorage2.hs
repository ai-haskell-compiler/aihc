-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderPixelLocalStorage2 (
  -- * Extension Support
    gl_EXT_shader_pixel_local_storage2

  -- * GL_EXT_shader_pixel_local_storage2
  , glClearPixelLocalStorageuiEXT
  , glFramebufferPixelLocalStorageSizeEXT
  , glGetFramebufferPixelLocalStorageSizeEXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_INSUFFICIENT_SHADER_COMBINED_LOCAL_STORAGE_EXT
  , pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_FAST_SIZE_EXT
  , pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_SIZE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_shader_pixel_local_storage2.txt GL_EXT_shader_pixel_local_storage2> extension is available.

gl_EXT_shader_pixel_local_storage2 :: Bool
gl_EXT_shader_pixel_local_storage2 = member "GL_EXT_shader_pixel_local_storage2" extensions
{-# NOINLINE gl_EXT_shader_pixel_local_storage2 #-}

-- | Usage: @'glClearPixelLocalStorageuiEXT' offset n values@


glClearPixelLocalStorageuiEXT :: MonadIO m => GLsizei -> GLsizei -> Ptr GLuint -> m ()
glClearPixelLocalStorageuiEXT = ffisizeisizeiPtruintIOV glClearPixelLocalStorageuiEXTFunPtr

glClearPixelLocalStorageuiEXTFunPtr :: FunPtr (GLsizei -> GLsizei -> Ptr GLuint -> IO ())
glClearPixelLocalStorageuiEXTFunPtr = unsafePerformIO (getProcAddress "glClearPixelLocalStorageuiEXT")

{-# NOINLINE glClearPixelLocalStorageuiEXTFunPtr #-}

-- | Usage: @'glFramebufferPixelLocalStorageSizeEXT' target size@


glFramebufferPixelLocalStorageSizeEXT :: MonadIO m => GLuint -> GLsizei -> m ()
glFramebufferPixelLocalStorageSizeEXT = ffiuintsizeiIOV glFramebufferPixelLocalStorageSizeEXTFunPtr

glFramebufferPixelLocalStorageSizeEXTFunPtr :: FunPtr (GLuint -> GLsizei -> IO ())
glFramebufferPixelLocalStorageSizeEXTFunPtr = unsafePerformIO (getProcAddress "glFramebufferPixelLocalStorageSizeEXT")

{-# NOINLINE glFramebufferPixelLocalStorageSizeEXTFunPtr #-}

-- | Usage: @'glGetFramebufferPixelLocalStorageSizeEXT' target@


glGetFramebufferPixelLocalStorageSizeEXT :: MonadIO m => GLuint -> m GLsizei
glGetFramebufferPixelLocalStorageSizeEXT = ffiuintIOsizei glGetFramebufferPixelLocalStorageSizeEXTFunPtr

glGetFramebufferPixelLocalStorageSizeEXTFunPtr :: FunPtr (GLuint -> IO GLsizei)
glGetFramebufferPixelLocalStorageSizeEXTFunPtr = unsafePerformIO (getProcAddress "glGetFramebufferPixelLocalStorageSizeEXT")

{-# NOINLINE glGetFramebufferPixelLocalStorageSizeEXTFunPtr #-}

pattern GL_FRAMEBUFFER_INCOMPLETE_INSUFFICIENT_SHADER_COMBINED_LOCAL_STORAGE_EXT  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_INSUFFICIENT_SHADER_COMBINED_LOCAL_STORAGE_EXT = 0x9652

pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_FAST_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_FAST_SIZE_EXT = 0x9650

pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_COMBINED_LOCAL_STORAGE_SIZE_EXT = 0x9651