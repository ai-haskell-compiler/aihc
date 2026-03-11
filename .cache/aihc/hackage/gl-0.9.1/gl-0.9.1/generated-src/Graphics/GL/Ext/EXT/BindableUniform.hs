-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BindableUniform (
  -- * Extension Support
    gl_EXT_bindable_uniform

  -- * GL_EXT_bindable_uniform
  , glGetUniformBufferSizeEXT
  , glGetUniformOffsetEXT
  , glUniformBufferEXT
  , pattern GL_MAX_BINDABLE_UNIFORM_SIZE_EXT
  , pattern GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT
  , pattern GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT
  , pattern GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT
  , pattern GL_UNIFORM_BUFFER_BINDING_EXT
  , pattern GL_UNIFORM_BUFFER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/bindable_uniform.txt GL_EXT_bindable_uniform> extension is available.

gl_EXT_bindable_uniform :: Bool
gl_EXT_bindable_uniform = member "GL_EXT_bindable_uniform" extensions
{-# NOINLINE gl_EXT_bindable_uniform #-}

-- | Usage: @'glGetUniformBufferSizeEXT' program location@


glGetUniformBufferSizeEXT :: MonadIO m => GLuint -> GLint -> m GLint
glGetUniformBufferSizeEXT = ffiuintintIOint glGetUniformBufferSizeEXTFunPtr

glGetUniformBufferSizeEXTFunPtr :: FunPtr (GLuint -> GLint -> IO GLint)
glGetUniformBufferSizeEXTFunPtr = unsafePerformIO (getProcAddress "glGetUniformBufferSizeEXT")

{-# NOINLINE glGetUniformBufferSizeEXTFunPtr #-}

-- | Usage: @'glGetUniformOffsetEXT' program location@


glGetUniformOffsetEXT :: MonadIO m => GLuint -> GLint -> m GLintptr
glGetUniformOffsetEXT = ffiuintintIOintptr glGetUniformOffsetEXTFunPtr

glGetUniformOffsetEXTFunPtr :: FunPtr (GLuint -> GLint -> IO GLintptr)
glGetUniformOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glGetUniformOffsetEXT")

{-# NOINLINE glGetUniformOffsetEXTFunPtr #-}

-- | Usage: @'glUniformBufferEXT' program location buffer@


glUniformBufferEXT :: MonadIO m => GLuint -> GLint -> GLuint -> m ()
glUniformBufferEXT = ffiuintintuintIOV glUniformBufferEXTFunPtr

glUniformBufferEXTFunPtr :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
glUniformBufferEXTFunPtr = unsafePerformIO (getProcAddress "glUniformBufferEXT")

{-# NOINLINE glUniformBufferEXTFunPtr #-}

pattern GL_MAX_BINDABLE_UNIFORM_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_BINDABLE_UNIFORM_SIZE_EXT = 0x8DED

pattern GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT = 0x8DE3

pattern GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT = 0x8DE4

pattern GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT = 0x8DE2

pattern GL_UNIFORM_BUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BUFFER_BINDING_EXT = 0x8DEF

pattern GL_UNIFORM_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BUFFER_EXT = 0x8DEE