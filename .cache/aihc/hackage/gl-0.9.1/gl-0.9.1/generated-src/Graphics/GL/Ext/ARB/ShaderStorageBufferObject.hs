-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderStorageBufferObject (
  -- * Extension Support
    gl_ARB_shader_storage_buffer_object

  -- * GL_ARB_shader_storage_buffer_object
  , glShaderStorageBlockBinding
  , pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS
  , pattern GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES
  , pattern GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_SHADER_STORAGE_BLOCK_SIZE
  , pattern GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS
  , pattern GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS
  , pattern GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS
  , pattern GL_SHADER_STORAGE_BARRIER_BIT
  , pattern GL_SHADER_STORAGE_BUFFER
  , pattern GL_SHADER_STORAGE_BUFFER_BINDING
  , pattern GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT
  , pattern GL_SHADER_STORAGE_BUFFER_SIZE
  , pattern GL_SHADER_STORAGE_BUFFER_START
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_storage_buffer_object.txt GL_ARB_shader_storage_buffer_object> extension is available.

gl_ARB_shader_storage_buffer_object :: Bool
gl_ARB_shader_storage_buffer_object = member "GL_ARB_shader_storage_buffer_object" extensions
{-# NOINLINE gl_ARB_shader_storage_buffer_object #-}