-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderViewportLayerArray (
  -- * Extension Support
    gl_ARB_shader_viewport_layer_array
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_viewport_layer_array.txt GL_ARB_shader_viewport_layer_array> extension is available.

gl_ARB_shader_viewport_layer_array :: Bool
gl_ARB_shader_viewport_layer_array = member "GL_ARB_shader_viewport_layer_array" extensions
{-# NOINLINE gl_ARB_shader_viewport_layer_array #-}