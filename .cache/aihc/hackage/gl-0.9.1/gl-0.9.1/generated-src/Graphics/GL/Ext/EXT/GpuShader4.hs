-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.GpuShader4 (
  -- * Extension Support
    gl_EXT_gpu_shader4

  -- * GL_EXT_gpu_shader4
  , glBindFragDataLocationEXT
  , glGetFragDataLocationEXT
  , glGetUniformuivEXT
  , glUniform1uiEXT
  , glUniform1uivEXT
  , glUniform2uiEXT
  , glUniform2uivEXT
  , glUniform3uiEXT
  , glUniform3uivEXT
  , glUniform4uiEXT
  , glUniform4uivEXT
  , pattern GL_INT_SAMPLER_1D_ARRAY_EXT
  , pattern GL_INT_SAMPLER_1D_EXT
  , pattern GL_INT_SAMPLER_2D_ARRAY_EXT
  , pattern GL_INT_SAMPLER_2D_EXT
  , pattern GL_INT_SAMPLER_2D_RECT_EXT
  , pattern GL_INT_SAMPLER_3D_EXT
  , pattern GL_INT_SAMPLER_BUFFER_EXT
  , pattern GL_INT_SAMPLER_CUBE_EXT
  , pattern GL_MAX_PROGRAM_TEXEL_OFFSET_EXT
  , pattern GL_MIN_PROGRAM_TEXEL_OFFSET_EXT
  , pattern GL_SAMPLER_1D_ARRAY_EXT
  , pattern GL_SAMPLER_1D_ARRAY_SHADOW_EXT
  , pattern GL_SAMPLER_2D_ARRAY_EXT
  , pattern GL_SAMPLER_2D_ARRAY_SHADOW_EXT
  , pattern GL_SAMPLER_BUFFER_EXT
  , pattern GL_SAMPLER_CUBE_SHADOW_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_1D_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_3D_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT
  , pattern GL_UNSIGNED_INT_SAMPLER_CUBE_EXT
  , pattern GL_UNSIGNED_INT_VEC2_EXT
  , pattern GL_UNSIGNED_INT_VEC3_EXT
  , pattern GL_UNSIGNED_INT_VEC4_EXT
  , pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/gpu_shader4.txt GL_EXT_gpu_shader4> extension is available.

gl_EXT_gpu_shader4 :: Bool
gl_EXT_gpu_shader4 = member "GL_EXT_gpu_shader4" extensions
{-# NOINLINE gl_EXT_gpu_shader4 #-}

-- | Usage: @'glGetFragDataLocationEXT' program name@
--
-- The length of @name@ should be @COMPSIZE(name)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFragDataLocation'.


glGetFragDataLocationEXT :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
glGetFragDataLocationEXT = ffiuintPtrcharIOint glGetFragDataLocationEXTFunPtr

glGetFragDataLocationEXTFunPtr :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
glGetFragDataLocationEXTFunPtr = unsafePerformIO (getProcAddress "glGetFragDataLocationEXT")

{-# NOINLINE glGetFragDataLocationEXTFunPtr #-}

-- | Usage: @'glGetUniformuivEXT' program location params@
--
-- The length of @params@ should be @COMPSIZE(program,location)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetUniformuiv'.


glGetUniformuivEXT :: MonadIO m => GLuint -> GLint -> Ptr GLuint -> m ()
glGetUniformuivEXT = ffiuintintPtruintIOV glGetUniformuivEXTFunPtr

glGetUniformuivEXTFunPtr :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ())
glGetUniformuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetUniformuivEXT")

{-# NOINLINE glGetUniformuivEXTFunPtr #-}

-- | Usage: @'glUniform1uiEXT' location v0@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1ui'.


glUniform1uiEXT :: MonadIO m => GLint -> GLuint -> m ()
glUniform1uiEXT = ffiintuintIOV glUniform1uiEXTFunPtr

glUniform1uiEXTFunPtr :: FunPtr (GLint -> GLuint -> IO ())
glUniform1uiEXTFunPtr = unsafePerformIO (getProcAddress "glUniform1uiEXT")

{-# NOINLINE glUniform1uiEXTFunPtr #-}

-- | Usage: @'glUniform1uivEXT' location count value@
--
-- The length of @value@ should be @count*1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform1uiv'.


glUniform1uivEXT :: MonadIO m => GLint -> GLsizei -> Ptr GLuint -> m ()
glUniform1uivEXT = ffiintsizeiPtruintIOV glUniform1uivEXTFunPtr

glUniform1uivEXTFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
glUniform1uivEXTFunPtr = unsafePerformIO (getProcAddress "glUniform1uivEXT")

{-# NOINLINE glUniform1uivEXTFunPtr #-}

-- | Usage: @'glUniform2uiEXT' location v0 v1@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2ui'.


glUniform2uiEXT :: MonadIO m => GLint -> GLuint -> GLuint -> m ()
glUniform2uiEXT = ffiintuintuintIOV glUniform2uiEXTFunPtr

glUniform2uiEXTFunPtr :: FunPtr (GLint -> GLuint -> GLuint -> IO ())
glUniform2uiEXTFunPtr = unsafePerformIO (getProcAddress "glUniform2uiEXT")

{-# NOINLINE glUniform2uiEXTFunPtr #-}

-- | Usage: @'glUniform2uivEXT' location count value@
--
-- The length of @value@ should be @count*2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform2uiv'.


glUniform2uivEXT :: MonadIO m => GLint -> GLsizei -> Ptr GLuint -> m ()
glUniform2uivEXT = ffiintsizeiPtruintIOV glUniform2uivEXTFunPtr

glUniform2uivEXTFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
glUniform2uivEXTFunPtr = unsafePerformIO (getProcAddress "glUniform2uivEXT")

{-# NOINLINE glUniform2uivEXTFunPtr #-}

-- | Usage: @'glUniform3uiEXT' location v0 v1 v2@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3ui'.


glUniform3uiEXT :: MonadIO m => GLint -> GLuint -> GLuint -> GLuint -> m ()
glUniform3uiEXT = ffiintuintuintuintIOV glUniform3uiEXTFunPtr

glUniform3uiEXTFunPtr :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ())
glUniform3uiEXTFunPtr = unsafePerformIO (getProcAddress "glUniform3uiEXT")

{-# NOINLINE glUniform3uiEXTFunPtr #-}

-- | Usage: @'glUniform3uivEXT' location count value@
--
-- The length of @value@ should be @count*3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform3uiv'.


glUniform3uivEXT :: MonadIO m => GLint -> GLsizei -> Ptr GLuint -> m ()
glUniform3uivEXT = ffiintsizeiPtruintIOV glUniform3uivEXTFunPtr

glUniform3uivEXTFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
glUniform3uivEXTFunPtr = unsafePerformIO (getProcAddress "glUniform3uivEXT")

{-# NOINLINE glUniform3uivEXTFunPtr #-}

-- | Usage: @'glUniform4uiEXT' location v0 v1 v2 v3@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4ui'.


glUniform4uiEXT :: MonadIO m => GLint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glUniform4uiEXT = ffiintuintuintuintuintIOV glUniform4uiEXTFunPtr

glUniform4uiEXTFunPtr :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glUniform4uiEXTFunPtr = unsafePerformIO (getProcAddress "glUniform4uiEXT")

{-# NOINLINE glUniform4uiEXTFunPtr #-}

-- | Usage: @'glUniform4uivEXT' location count value@
--
-- The length of @value@ should be @count*4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glUniform4uiv'.


glUniform4uivEXT :: MonadIO m => GLint -> GLsizei -> Ptr GLuint -> m ()
glUniform4uivEXT = ffiintsizeiPtruintIOV glUniform4uivEXTFunPtr

glUniform4uivEXTFunPtr :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
glUniform4uivEXTFunPtr = unsafePerformIO (getProcAddress "glUniform4uivEXT")

{-# NOINLINE glUniform4uivEXTFunPtr #-}

pattern GL_INT_SAMPLER_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_1D_ARRAY_EXT = 0x8DCE

pattern GL_INT_SAMPLER_1D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_1D_EXT = 0x8DC9

pattern GL_INT_SAMPLER_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_2D_ARRAY_EXT = 0x8DCF

pattern GL_INT_SAMPLER_2D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_2D_EXT = 0x8DCA

pattern GL_INT_SAMPLER_2D_RECT_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_2D_RECT_EXT = 0x8DCD

pattern GL_INT_SAMPLER_3D_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_3D_EXT = 0x8DCB

pattern GL_INT_SAMPLER_CUBE_EXT  :: (Eq a, Num a) => a

pattern GL_INT_SAMPLER_CUBE_EXT = 0x8DCC

pattern GL_MAX_PROGRAM_TEXEL_OFFSET_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TEXEL_OFFSET_EXT = 0x8905

pattern GL_MIN_PROGRAM_TEXEL_OFFSET_EXT  :: (Eq a, Num a) => a

pattern GL_MIN_PROGRAM_TEXEL_OFFSET_EXT = 0x8904

pattern GL_SAMPLER_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_1D_ARRAY_EXT = 0x8DC0

pattern GL_SAMPLER_1D_ARRAY_SHADOW_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_1D_ARRAY_SHADOW_EXT = 0x8DC3

pattern GL_SAMPLER_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_ARRAY_EXT = 0x8DC1

pattern GL_SAMPLER_2D_ARRAY_SHADOW_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_ARRAY_SHADOW_EXT = 0x8DC4

pattern GL_SAMPLER_CUBE_SHADOW_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_SHADOW_EXT = 0x8DC5

pattern GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT = 0x8DD6

pattern GL_UNSIGNED_INT_SAMPLER_1D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_1D_EXT = 0x8DD1

pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT = 0x8DD7

pattern GL_UNSIGNED_INT_SAMPLER_2D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_2D_EXT = 0x8DD2

pattern GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT = 0x8DD5

pattern GL_UNSIGNED_INT_SAMPLER_3D_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_3D_EXT = 0x8DD3

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_SAMPLER_CUBE_EXT = 0x8DD4

pattern GL_UNSIGNED_INT_VEC2_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_VEC2_EXT = 0x8DC6

pattern GL_UNSIGNED_INT_VEC3_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_VEC3_EXT = 0x8DC7

pattern GL_UNSIGNED_INT_VEC4_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_VEC4_EXT = 0x8DC8

pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT = 0x88FD