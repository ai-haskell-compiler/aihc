-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShaderStorageBufferObject (
  -- * Extension Support
    gl_NV_shader_storage_buffer_object
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/shader_storage_buffer_object.txt GL_NV_shader_storage_buffer_object> extension is available.

gl_NV_shader_storage_buffer_object :: Bool
gl_NV_shader_storage_buffer_object = member "GL_NV_shader_storage_buffer_object" extensions
{-# NOINLINE gl_NV_shader_storage_buffer_object #-}