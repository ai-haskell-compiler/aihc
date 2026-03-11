-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.VertexShader (
  -- * Extension Support
    gl_ARB_vertex_shader

  -- * GL_ARB_vertex_shader
  , glBindAttribLocationARB
  , glDisableVertexAttribArrayARB
  , glEnableVertexAttribArrayARB
  , glGetActiveAttribARB
  , glGetAttribLocationARB
  , glGetVertexAttribPointervARB
  , glGetVertexAttribdvARB
  , glGetVertexAttribfvARB
  , glGetVertexAttribivARB
  , glVertexAttrib1dARB
  , glVertexAttrib1dvARB
  , glVertexAttrib1fARB
  , glVertexAttrib1fvARB
  , glVertexAttrib1sARB
  , glVertexAttrib1svARB
  , glVertexAttrib2dARB
  , glVertexAttrib2dvARB
  , glVertexAttrib2fARB
  , glVertexAttrib2fvARB
  , glVertexAttrib2sARB
  , glVertexAttrib2svARB
  , glVertexAttrib3dARB
  , glVertexAttrib3dvARB
  , glVertexAttrib3fARB
  , glVertexAttrib3fvARB
  , glVertexAttrib3sARB
  , glVertexAttrib3svARB
  , glVertexAttrib4NbvARB
  , glVertexAttrib4NivARB
  , glVertexAttrib4NsvARB
  , glVertexAttrib4NubARB
  , glVertexAttrib4NubvARB
  , glVertexAttrib4NuivARB
  , glVertexAttrib4NusvARB
  , glVertexAttrib4bvARB
  , glVertexAttrib4dARB
  , glVertexAttrib4dvARB
  , glVertexAttrib4fARB
  , glVertexAttrib4fvARB
  , glVertexAttrib4ivARB
  , glVertexAttrib4sARB
  , glVertexAttrib4svARB
  , glVertexAttrib4ubvARB
  , glVertexAttrib4uivARB
  , glVertexAttrib4usvARB
  , glVertexAttribPointerARB
  , pattern GL_CURRENT_VERTEX_ATTRIB_ARB
  , pattern GL_FLOAT
  , pattern GL_FLOAT_MAT2_ARB
  , pattern GL_FLOAT_MAT3_ARB
  , pattern GL_FLOAT_MAT4_ARB
  , pattern GL_FLOAT_VEC2_ARB
  , pattern GL_FLOAT_VEC3_ARB
  , pattern GL_FLOAT_VEC4_ARB
  , pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB
  , pattern GL_MAX_TEXTURE_COORDS_ARB
  , pattern GL_MAX_TEXTURE_IMAGE_UNITS_ARB
  , pattern GL_MAX_VARYING_FLOATS_ARB
  , pattern GL_MAX_VERTEX_ATTRIBS_ARB
  , pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB
  , pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB
  , pattern GL_OBJECT_ACTIVE_ATTRIBUTES_ARB
  , pattern GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB
  , pattern GL_VERTEX_PROGRAM_POINT_SIZE_ARB
  , pattern GL_VERTEX_PROGRAM_TWO_SIDE_ARB
  , pattern GL_VERTEX_SHADER_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/vertex_shader.txt GL_ARB_vertex_shader> extension is available.

gl_ARB_vertex_shader :: Bool
gl_ARB_vertex_shader = member "GL_ARB_vertex_shader" extensions
{-# NOINLINE gl_ARB_vertex_shader #-}

-- | Usage: @'glBindAttribLocationARB' programObj index name@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindAttribLocation'.


glBindAttribLocationARB :: MonadIO m => GLhandleARB -> GLuint -> Ptr GLcharARB -> m ()
glBindAttribLocationARB = ffihandleARBuintPtrcharARBIOV glBindAttribLocationARBFunPtr

glBindAttribLocationARBFunPtr :: FunPtr (GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ())
glBindAttribLocationARBFunPtr = unsafePerformIO (getProcAddress "glBindAttribLocationARB")

{-# NOINLINE glBindAttribLocationARBFunPtr #-}

-- | Usage: @'glGetActiveAttribARB' programObj index maxLength length size type name@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- The length of @length@ should be @1@.
--
-- The length of @size@ should be @1@.
--
-- The length of @type@ should be @1@.
--
-- The length of @name@ should be @maxLength@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetActiveAttrib'.


glGetActiveAttribARB :: MonadIO m => GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> m ()
glGetActiveAttribARB = ffihandleARBuintsizeiPtrsizeiPtrintPtrenumPtrcharARBIOV glGetActiveAttribARBFunPtr

glGetActiveAttribARBFunPtr :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
glGetActiveAttribARBFunPtr = unsafePerformIO (getProcAddress "glGetActiveAttribARB")

{-# NOINLINE glGetActiveAttribARBFunPtr #-}

-- | Usage: @'glGetAttribLocationARB' programObj name@
--
-- The parameter @programObj@ is a @handleARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetAttribLocation'.


glGetAttribLocationARB :: MonadIO m => GLhandleARB -> Ptr GLcharARB -> m GLint
glGetAttribLocationARB = ffihandleARBPtrcharARBIOint glGetAttribLocationARBFunPtr

glGetAttribLocationARBFunPtr :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
glGetAttribLocationARBFunPtr = unsafePerformIO (getProcAddress "glGetAttribLocationARB")

{-# NOINLINE glGetAttribLocationARBFunPtr #-}

pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = 0x8B4D

pattern GL_MAX_VARYING_FLOATS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_VARYING_FLOATS_ARB = 0x8B4B

pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = 0x8B4A

pattern GL_OBJECT_ACTIVE_ATTRIBUTES_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = 0x8B89

pattern GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = 0x8B8A

pattern GL_VERTEX_SHADER_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_ARB = 0x8B31