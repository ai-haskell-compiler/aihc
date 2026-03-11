-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderImageLoadStore (
  -- * Extension Support
    gl_ARB_shader_image_load_store

  -- * GL_ARB_shader_image_load_store
  , glBindImageTexture
  , glMemoryBarrier
  , pattern GL_ALL_BARRIER_BITS
  , pattern GL_ATOMIC_COUNTER_BARRIER_BIT
  , pattern GL_BUFFER_UPDATE_BARRIER_BIT
  , pattern GL_COMMAND_BARRIER_BIT
  , pattern GL_ELEMENT_ARRAY_BARRIER_BIT
  , pattern GL_FRAMEBUFFER_BARRIER_BIT
  , pattern GL_IMAGE_1D
  , pattern GL_IMAGE_1D_ARRAY
  , pattern GL_IMAGE_2D
  , pattern GL_IMAGE_2D_ARRAY
  , pattern GL_IMAGE_2D_MULTISAMPLE
  , pattern GL_IMAGE_2D_MULTISAMPLE_ARRAY
  , pattern GL_IMAGE_2D_RECT
  , pattern GL_IMAGE_3D
  , pattern GL_IMAGE_BINDING_ACCESS
  , pattern GL_IMAGE_BINDING_FORMAT
  , pattern GL_IMAGE_BINDING_LAYER
  , pattern GL_IMAGE_BINDING_LAYERED
  , pattern GL_IMAGE_BINDING_LEVEL
  , pattern GL_IMAGE_BINDING_NAME
  , pattern GL_IMAGE_BUFFER
  , pattern GL_IMAGE_CUBE
  , pattern GL_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE
  , pattern GL_IMAGE_FORMAT_COMPATIBILITY_TYPE
  , pattern GL_INT_IMAGE_1D
  , pattern GL_INT_IMAGE_1D_ARRAY
  , pattern GL_INT_IMAGE_2D
  , pattern GL_INT_IMAGE_2D_ARRAY
  , pattern GL_INT_IMAGE_2D_MULTISAMPLE
  , pattern GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
  , pattern GL_INT_IMAGE_2D_RECT
  , pattern GL_INT_IMAGE_3D
  , pattern GL_INT_IMAGE_BUFFER
  , pattern GL_INT_IMAGE_CUBE
  , pattern GL_INT_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_MAX_COMBINED_IMAGE_UNIFORMS
  , pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS
  , pattern GL_MAX_FRAGMENT_IMAGE_UNIFORMS
  , pattern GL_MAX_GEOMETRY_IMAGE_UNIFORMS
  , pattern GL_MAX_IMAGE_SAMPLES
  , pattern GL_MAX_IMAGE_UNITS
  , pattern GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS
  , pattern GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS
  , pattern GL_MAX_VERTEX_IMAGE_UNIFORMS
  , pattern GL_PIXEL_BUFFER_BARRIER_BIT
  , pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
  , pattern GL_TEXTURE_FETCH_BARRIER_BIT
  , pattern GL_TEXTURE_UPDATE_BARRIER_BIT
  , pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT
  , pattern GL_UNIFORM_BARRIER_BIT
  , pattern GL_UNSIGNED_INT_IMAGE_1D
  , pattern GL_UNSIGNED_INT_IMAGE_1D_ARRAY
  , pattern GL_UNSIGNED_INT_IMAGE_2D
  , pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY
  , pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
  , pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
  , pattern GL_UNSIGNED_INT_IMAGE_2D_RECT
  , pattern GL_UNSIGNED_INT_IMAGE_3D
  , pattern GL_UNSIGNED_INT_IMAGE_BUFFER
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE
  , pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY
  , pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_image_load_store.txt GL_ARB_shader_image_load_store> extension is available.

gl_ARB_shader_image_load_store :: Bool
gl_ARB_shader_image_load_store = member "GL_ARB_shader_image_load_store" extensions
{-# NOINLINE gl_ARB_shader_image_load_store #-}